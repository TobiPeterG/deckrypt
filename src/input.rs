use evdev::AbsoluteAxisType;
use evdev::{
    uinput::{VirtualDevice, VirtualDeviceBuilder},
    Device, EventType, InputEvent, InputEventKind, Key,
};
use libc::input_absinfo;
use log::{debug, error, trace, warn};
use std::collections::{HashMap, HashSet};
use std::io;

use crate::keymap::{generate_chrmap_auto, shift_transform, ALLOWED_CHARACTERS};
use crate::types::{
    Action, BuiltMappings, ControllerConfig, Direction, GamepadInput, Mapping, Modifiers,
    PressedMapping, SelectedDevice,
};

/// We keep the same threshold logic for axes. The axis value must exceed a certain percentage  
/// (here, 50% of the maximum) in order to register as "pressed."
const AXIS_THRESHOLD_PERCENTAGE: f32 = 0.5;

/// Creates a virtual keyboard with the specified keyboard keys enabled.  
/// We register the provided list of keys (`all_keyboard_keys`) as supported,  
/// plus some common modifier keys like SHIFT, ALTGR, etc.
pub fn create_virtual_keyboard(all_keyboard_keys: &[Key]) -> std::io::Result<VirtualDevice> {
    use evdev::AttributeSet;

    let mut keys = AttributeSet::<Key>::new();
    for &key in all_keyboard_keys {
        keys.insert(key);
    }
    // Add some common modifier keys
    keys.insert(Key::KEY_LEFTSHIFT);
    keys.insert(Key::KEY_RIGHTALT);
    keys.insert(Key::KEY_LEFTCTRL);
    // Add special keys
    keys.insert(Key::KEY_ENTER);
    keys.insert(Key::KEY_ESC);

    let device = VirtualDeviceBuilder::new()?
        .name("Deckrypt Virtual Keyboard")
        .with_keys(&keys)?
        .build()?;

    Ok(device)
}

/// Handles kernel-level modifier keys (SHIFT or ALTGR).
/// - `level = 1`: SHIFT
/// - `level = 2`: ALTGR
/// - `action = true`: Press
/// - `action = false`: Release
pub fn handle_kernel_modifier(
    level: u8,
    action: Action, // true for press, false for release
    virtual_keyboard: &mut VirtualDevice,
) -> std::io::Result<()> {
    let act = match action {
        Action::Activate => 1,
        Action::Deactivate => 0,
    };
    let event = match level {
        1 => InputEvent::new(EventType::KEY, Key::KEY_LEFTSHIFT.code(), act),
        2 => InputEvent::new(EventType::KEY, Key::KEY_RIGHTALT.code(), act),
        _ => return Ok(()),
    };
    virtual_keyboard.emit(&[event])?;
    Ok(())
}

/// Given an `input_absinfo`, returns a tuple of `(activation_threshold, release_threshold)`  
/// that the axis value must cross to be considered "pressed" or "released".  
///
/// We handle special cases for controllers that have a -1..1 range.
fn get_axis_thresholds(abs_info: &input_absinfo) -> (i32, i32) {
    if abs_info.maximum == 1 && abs_info.minimum == -1 {
        return (1, -1);
    }
    let activation_threshold = (AXIS_THRESHOLD_PERCENTAGE * abs_info.maximum as f32) as i32;
    let mut release_threshold = (AXIS_THRESHOLD_PERCENTAGE * abs_info.minimum as f32) as i32;
    if abs_info.minimum == 0 {
        release_threshold = -1;
    }
    (activation_threshold, release_threshold)
}

/// Retrieves the display name for a given `GamepadInput`.
/// - If `friendly` is `true` and a friendly name exists, it returns the friendly name.
/// - Otherwise, it returns the `Debug` representation of `GamepadInput`.
fn get_display_name(
    gamepad_input: &GamepadInput,
    friendly_names: &HashMap<GamepadInput, String>,
    friendly: bool,
) -> String {
    if friendly {
        if let Some(name) = friendly_names.get(gamepad_input) {
            return name.clone();
        }
    }
    match gamepad_input {
        GamepadInput::Button(button) => format!("{:?}", button),
        GamepadInput::Axis(num, direction) => {
            let dir = match direction {
                Direction::Negative => "NEG",
                Direction::Positive => "POS",
            };
            format!("{:?}_{}", AbsoluteAxisType(*num), dir)
        }
    }
}

/// Builds up a final set of normal and alternate mappings by:
/// 1. Using any user-specified button/axis mappings from the config (which may be empty for unknown).
/// 2. If `auto_mapping_enabled` is `true`, we attempt to determine if the device supports additional
///    axes/buttons and automatically assign them characters if they are unmapped.  
/// 3. Then, if `auto_mapping_enabled` is `true`, we generate alternate mappings for leftover inputs
///    (using reversed characters or leftover).  
///  
/// Returns a `BuiltMappings` struct with the final normal/alternate mappings and any special Enter input.
fn get_mappings(
    device: &Device,
    config: &ControllerConfig,
    abs_info_map: Option<&HashMap<u16, input_absinfo>>,
    friendly: bool,

    // Controls whether we auto-map leftover inputs:
    auto_mapping_enabled: bool,
) -> BuiltMappings {
    let button_mappings = &config.button_mappings;
    let axis_mappings = &config.axis_mappings;
    let alternate_button_mappings = &config.alternate_button_mappings;
    let alternate_axis_mappings = &config.alternate_axis_mappings;
    let modifiers = &config.modifiers;

    let mut normal_mapping = HashMap::new();
    let mut alternate_mapping = HashMap::new();
    let mut used_chars = HashSet::new();

    // Mark shift/alt as reserved
    let mut modifier_inputs = HashSet::new();
    if let Some(ref mod1) = modifiers.shift_modifier {
        modifier_inputs.insert(mod1.clone());
    }
    if let Some(ref mod2) = modifiers.alternate_modifier {
        modifier_inputs.insert(mod2.clone());
    }
    if let Some(ref mod3) = modifiers.enter_modifier {
        modifier_inputs.insert(mod3.clone());
    }

    // -------------------------------
    // 1) Insert button + axis mappings into normal
    // -------------------------------
    for &(ref gi, ref mapping) in button_mappings {
        normal_mapping.insert(gi.clone(), mapping.clone());
        if let Mapping::Character(c) = mapping {
            used_chars.insert(*c);
        }
    }
    for &(ref gi, ref mapping) in axis_mappings {
        normal_mapping.insert(gi.clone(), mapping.clone());
        if let Mapping::Character(c) = mapping {
            used_chars.insert(*c);
        }
    }

    // -------------------------------
    // 2) Insert alternate button + axis mappings
    // -------------------------------
    for &(ref gi, ref mapping) in alternate_button_mappings {
        alternate_mapping.insert(gi.clone(), mapping.clone());
        if let Mapping::Character(c) = mapping {
            used_chars.insert(*c);
        }
    }
    for &(ref gi, ref mapping) in alternate_axis_mappings {
        alternate_mapping.insert(gi.clone(), mapping.clone());
        if let Mapping::Character(c) = mapping {
            used_chars.insert(*c);
        }
    }

    // Collect gamepad inputs that are not modifiers
    let mut all_gamepad_inputs = Vec::new();
    if let Some(supported_keys) = device.supported_keys() {
        for key in supported_keys.iter() {
            let gi = GamepadInput::Button(key);
            if !modifier_inputs.contains(&gi) {
                all_gamepad_inputs.push(gi);
            }
        }
    }
    if let Some(supported_axes) = device.supported_absolute_axes() {
        for axis in supported_axes.iter() {
            if let Some(abs_info) = abs_info_map.and_then(|map| map.get(&axis.0)) {
                if abs_info.minimum < 0 {
                    let neg_input = GamepadInput::Axis(axis.0, Direction::Negative);
                    if !modifier_inputs.contains(&neg_input) {
                        all_gamepad_inputs.push(neg_input);
                    }
                }
                if abs_info.maximum > 0 {
                    let pos_input = GamepadInput::Axis(axis.0, Direction::Positive);
                    if !modifier_inputs.contains(&pos_input) {
                        all_gamepad_inputs.push(pos_input);
                    }
                }
            }
        }
    }

    // Filter out any already mapped
    let available_normal_gamepad_inputs: Vec<_> = all_gamepad_inputs
        .iter()
        .filter(|gi| !normal_mapping.contains_key(*gi))
        .cloned()
        .collect();

    // -------------------------------
    // If auto_mapping_enabled == false, we SKIP any leftover auto-assignment
    // -------------------------------
    if auto_mapping_enabled {
        // 3) Assign a..z
        let normal_letters: Vec<char> = ALLOWED_CHARACTERS
            .iter()
            .cloned()
            .filter(|c| c.is_ascii_lowercase() && !used_chars.contains(c))
            .collect();

        let mut normal_chars_iter = normal_letters.into_iter();
        for gi in &available_normal_gamepad_inputs {
            if let Some(next_char) = normal_chars_iter.next() {
                normal_mapping.insert(gi.clone(), Mapping::Character(next_char));
                used_chars.insert(next_char);
                let display_name = get_display_name(&gi, &config.friendly_names, friendly);
                trace!("Automatically mapped {:?} -> '{}'", display_name, next_char);
            } else {
                trace!("No more letters available to map.");
                break;
            }
        }

        // 4) Then digits
        let remaining_gamepad_inputs: Vec<_> = available_normal_gamepad_inputs
            .iter()
            .filter(|gi| !normal_mapping.contains_key(*gi))
            .cloned()
            .collect();
        let normal_numbers: Vec<char> = ALLOWED_CHARACTERS
            .iter()
            .cloned()
            .filter(|c| c.is_ascii_digit() && !used_chars.contains(c))
            .collect();
        let mut numbers_iter = normal_numbers.into_iter();
        for gi in &remaining_gamepad_inputs {
            if let Some(next_char) = numbers_iter.next() {
                normal_mapping.insert(gi.clone(), Mapping::Character(next_char));
                used_chars.insert(next_char);
                let display_name = get_display_name(&gi, &config.friendly_names, friendly);
                trace!("Automatically mapped {:?} -> '{}'", display_name, next_char);
            } else {
                trace!("No more digits available to map.");
                break;
            }
        }

        // 5) Then symbols
        let remaining_gamepad_inputs: Vec<_> = remaining_gamepad_inputs
            .iter()
            .filter(|gi| !normal_mapping.contains_key(*gi))
            .cloned()
            .collect();
        let available_symbols: Vec<char> = ALLOWED_CHARACTERS
            .iter()
            .cloned()
            .filter(|c| {
                !c.is_ascii_alphanumeric()
                    && !c.is_whitespace()
                    && !c.is_control()
                    && !used_chars.contains(c)
            })
            .collect();
        let mut symbols_iter = available_symbols.into_iter();
        for gi in &remaining_gamepad_inputs {
            if let Some(next_char) = symbols_iter.next() {
                normal_mapping.insert(gi.clone(), Mapping::Character(next_char));
                used_chars.insert(next_char);
                let display_name = get_display_name(&gi, &config.friendly_names, friendly);
                trace!("Automatically mapped {:?} -> '{}'", display_name, next_char);
            } else {
                trace!("No more symbols available to map.");
                break;
            }
        }

        // -------------------------------
        // 6) For the alternate mapping, fill in reversed leftover
        // but only if there are actually characters left
        // -------------------------------
        if symbols_iter.next() != None {
            // Mark used chars from both normal and alternate
            for mapping in normal_mapping.values().chain(alternate_mapping.values()) {
                if let Mapping::Character(c) = mapping {
                    used_chars.insert(*c);
                }
            }
            let available_alternate_gamepad_inputs: Vec<_> = all_gamepad_inputs
                .iter()
                .filter(|gi| !alternate_mapping.contains_key(*gi))
                .cloned()
                .collect();

            // Collect all available characters for alternate mapping in the correct order
            let mut alternate_chars_iter = ALLOWED_CHARACTERS
                .iter()
                .cloned()
                .filter(|c| !used_chars.contains(c))
                .collect::<Vec<char>>()
                .into_iter();

            for gi in &available_alternate_gamepad_inputs {
                if !alternate_mapping.contains_key(gi) {
                    if let Some(c) = alternate_chars_iter.next() {
                        alternate_mapping.insert(gi.clone(), Mapping::Character(c));
                        used_chars.insert(c);
                        let display_name = get_display_name(&gi, &config.friendly_names, friendly);
                        trace!("Alternate mapped {:?} -> '{}'", display_name, c);
                    } else {
                        trace!("No more characters available to map for alternate mode.");
                        break;
                    }
                }
            }
        } else {
            trace!("No characters available to map for alternate mode.");
        }
    }

    BuiltMappings {
        normal_mapping,
        alternate_mapping,
    }
}

fn press_key(
    key_to_emit: Key,
    pressed_mapping: PressedMapping,
    virtual_keyboard: &mut VirtualDevice,
    pressed_mappings: &mut HashMap<GamepadInput, PressedMapping>,
    gamepad_input: &GamepadInput,
) -> io::Result<()> {
    let e = InputEvent::new(EventType::KEY, key_to_emit.code(), 1);
    virtual_keyboard.emit(&[e])?;
    // Track the pressed mapping
    pressed_mappings.insert(gamepad_input.clone(), pressed_mapping);
    Ok(())
}

fn print_friendly_input(
    config: &ControllerConfig,
    modifiers: &mut Modifiers,
    friendly: bool,
    display_name: &String,
) {
    let mut log_str = display_name.clone();
    if friendly == true {
        if modifiers.shift_active {
            let shift_name = match &modifiers.shift_modifier {
                Some(modifier) => get_display_name(&modifier, &config.friendly_names, friendly),
                None => "SHIFT".to_string(),
            };
            log_str = format!("{} + {}", shift_name, log_str);
        }
        if modifiers.alternate_active {
            let alt_name = match &modifiers.alternate_modifier {
                Some(modifier) => get_display_name(&modifier, &config.friendly_names, friendly),
                None => "ALTERNATE".to_string(),
            };
            log_str = format!("{} + {}", alt_name, log_str);
        }
        println!("{}", log_str);
    }
}

/// Generalized function to handle activation of a mapping (button or axis).
fn activate_mapping(
    gamepad_input: &GamepadInput,
    config: &ControllerConfig,
    mapping: &HashMap<GamepadInput, Mapping>,
    chrmap: &HashMap<char, (Key, u8)>,
    virtual_keyboard: &mut VirtualDevice,
    pressed_mappings: &mut HashMap<GamepadInput, PressedMapping>,
    modifiers: &mut Modifiers,
    friendly: bool,
) -> io::Result<()> {
    let display_name = get_display_name(&gamepad_input, &config.friendly_names, friendly);
    if Some(gamepad_input.clone()) == modifiers.shift_modifier {
        modifiers.shift_active = true;
        debug!("Activated {} ({})", display_name, "SHIFT");
        return Ok(());
    }
    if Some(gamepad_input.clone()) == modifiers.alternate_modifier {
        modifiers.alternate_active = true;
        debug!("Activated {} ({})", display_name, "ALTERNATE");
        return Ok(());
    }
    if Some(gamepad_input.clone()) == modifiers.enter_modifier {
        // Shift + Alternate + Enter quits deckrypt.
        if modifiers.shift_active && modifiers.alternate_active {
            let friendly_string = format!("{} (QUIT)", display_name);
            print_friendly_input(config, modifiers, friendly, &friendly_string);
            debug!("Quit combo pressed: SHIFT + ALTERNATE + ENTER -> exiting.");
            std::process::exit(0);
        }

        let friendly_string;
        let key_to_emit = if modifiers.shift_active || modifiers.alternate_active {
            friendly_string = format!("{} (BACKSPACE)", display_name);
            Key::KEY_BACKSPACE
        } else {
            friendly_string = format!("{} (ENTER)", display_name);
            Key::KEY_ENTER
        };
        press_key(
            key_to_emit,
            PressedMapping::Key(key_to_emit),
            virtual_keyboard,
            pressed_mappings,
            gamepad_input,
        )?;
        debug!("Activated {} ({:?})", display_name, key_to_emit);
        print_friendly_input(config, modifiers, friendly, &friendly_string);
        return Ok(());
    }

    if let Some(mapping_value) = mapping.get(&gamepad_input) {
        match mapping_value {
            Mapping::Character(mut ch) => {
                // Apply shift if active
                if modifiers.shift_active {
                    ch = shift_transform(ch);
                }

                if let Some((keycode, lvl)) = chrmap.get(&ch) {
                    // Handle modifiers based on level
                    handle_kernel_modifier(*lvl, Action::Activate, virtual_keyboard)?;
                    press_key(
                        *keycode,
                        PressedMapping::Character {
                            keycode: *keycode,
                            level: *lvl,
                        },
                        virtual_keyboard,
                        pressed_mappings,
                        gamepad_input,
                    )?;
                    debug!("Activated {} ({})", display_name, ch);
                } else {
                    warn!("No keycode mapping found for character '{}'", ch);
                }
            }
            Mapping::Key(kc) => {
                press_key(
                    *kc,
                    PressedMapping::Key(*kc),
                    virtual_keyboard,
                    pressed_mappings,
                    gamepad_input,
                )?;

                debug!("Activated {} ({:?})", display_name, kc);
            }
            Mapping::None => return Ok(()),
        }
        print_friendly_input(config, modifiers, friendly, &display_name);
    }
    Ok(())
}

/// Generalized function to handle release of a mapping (button or axis).
fn release_mapping(
    gamepad_input: &GamepadInput,
    mapping: &HashMap<GamepadInput, Mapping>,
    virtual_keyboard: &mut VirtualDevice,
    pressed_mappings: &mut HashMap<GamepadInput, PressedMapping>,
    modifiers: &mut Modifiers,
    display_name: &String,
) -> io::Result<()> {
    if Some(gamepad_input.clone()) == modifiers.shift_modifier {
        modifiers.shift_active = false;
        debug!("Deactivated {} ({})", display_name, "SHIFT");
        return Ok(());
    }
    if Some(gamepad_input.clone()) == modifiers.alternate_modifier {
        modifiers.alternate_active = false;
        debug!("Deactivated {} ({})", display_name, "ALTERNATE");
        return Ok(());
    }
    if Some(gamepad_input.clone()) == modifiers.enter_modifier {
        // If the quit combo was used, the process already exited in activate_mapping().
        let key_to_emit = if modifiers.shift_active || modifiers.alternate_active {
            Key::KEY_BACKSPACE
        } else {
            Key::KEY_ENTER
        };
        let e = InputEvent::new(EventType::KEY, key_to_emit.code(), 0);
        virtual_keyboard.emit(&[e])?;
        debug!("Deactivated {} ({:?})", display_name, key_to_emit);
        return Ok(());
    }
    if let Some(pressed) = pressed_mappings.remove(gamepad_input) {
        match pressed {
            PressedMapping::Character { keycode, level } => {
                // Emit key release
                let e = InputEvent::new(EventType::KEY, keycode.code(), 0);
                virtual_keyboard.emit(&[e])?;

                // Handle modifier deactivation
                handle_kernel_modifier(level, Action::Deactivate, virtual_keyboard)?;
            }
            PressedMapping::Key(kc) => {
                // Emit key release
                let e = InputEvent::new(EventType::KEY, kc.code(), 0);
                virtual_keyboard.emit(&[e])?;
            }
        }

        if let Some(mapping_value) = mapping.get(&gamepad_input) {
            match mapping_value {
                Mapping::Character(mut ch) => {
                    if modifiers.shift_active {
                        ch = shift_transform(ch);
                    }
                    debug!("Deactivated {} ({})", display_name, ch);
                }
                Mapping::Key(mapping_value) => {
                    debug!("Deactivated {} ({:?})", display_name, mapping_value);
                }
                Mapping::None => return Ok(()),
            };
        }
    }
    Ok(())
}

/// Handles the main event loop for a selected device.
///
/// This function encapsulates the shared logic for both known and unknown devices,
/// thereby eliminating duplicated code.
///
/// # Parameters
/// - `device_path`: The file system path to the input device.
/// - `config`: The controller configuration (either parsed or ephemeral).
/// - `auto_mapping_enabled`: Flag indicating whether to enable automatic mapping.
/// - `friendly`: Flag indicating whether to use friendly names in logs.
///
/// # Returns
/// - `io::Result<()>`: Returns `Ok(())` on success or an `io::Error` on failure.
fn handle_device(
    device_path: &str,
    config: &ControllerConfig,
    auto_mapping_enabled: bool,
    friendly: bool,
) -> io::Result<()> {
    // Open the device
    let mut gamepad_device = Device::open(device_path)?;

    // Read axis info
    let abs_info = gamepad_device.get_abs_state().ok();
    let abs_info_map = abs_info.as_ref().map(|abs_vec| {
        let mut map = HashMap::new();
        for (i, info) in abs_vec.iter().enumerate() {
            if info.maximum != 0 || info.minimum != 0 {
                map.insert(i as u16, *info);
            }
        }
        map
    });

    // Generate character maps
    let chrmap = match generate_chrmap_auto() {
        Some(maps) => maps,
        None => {
            error!("Failed to generate character map.");
            std::process::exit(1);
        }
    };

    // Gather all keys from chrmap
    let all_keyboard_keys: Vec<Key> = chrmap.values().map(|&(k, _)| k).collect();

    // Create the virtual keyboard
    let mut virtual_keyboard = create_virtual_keyboard(&all_keyboard_keys)?;

    // Build final normal + alternate mapping
    let built = get_mappings(
        &gamepad_device,
        config,
        abs_info_map.as_ref(),
        friendly,
        auto_mapping_enabled,
    );
    let normal_mapping = built.normal_mapping;
    let alternate_mapping = built.alternate_mapping;

    let mut axis_states: HashMap<u16, i32> = HashMap::new();
    let mut pressed_mappings: HashMap<GamepadInput, PressedMapping> = HashMap::new();
    let mut modifiers = config.modifiers.clone();

    debug!("Listening for gamepad events...");

    // Main event loop
    loop {
        for ev in gamepad_device.fetch_events()? {
            match ev.kind() {
                InputEventKind::Key(k) => {
                    let gamepad_input = GamepadInput::Button(k);
                    let is_pressed = ev.value() == 1;
                    let mapping = if modifiers.alternate_active {
                        &alternate_mapping
                    } else {
                        &normal_mapping
                    };

                    if is_pressed {
                        activate_mapping(
                            &gamepad_input,
                            config,
                            mapping,
                            &chrmap,
                            &mut virtual_keyboard,
                            &mut pressed_mappings,
                            &mut modifiers,
                            friendly,
                        )?;
                    } else {
                        release_mapping(
                            &gamepad_input,
                            mapping,
                            &mut virtual_keyboard,
                            &mut pressed_mappings,
                            &mut modifiers,
                            &get_display_name(&gamepad_input, &config.friendly_names, friendly),
                        )?;
                    }
                }

                InputEventKind::AbsAxis(ax) => {
                    let axis_value = ev.value();
                    let old_val = axis_states.get(&ax.0).cloned().unwrap_or(0);

                    if let Some(ref mp) = abs_info_map {
                        if let Some(abs_i) = mp.get(&ax.0) {
                            let (act_thr, rel_thr) = get_axis_thresholds(abs_i);
                            let current_mapping = if modifiers.alternate_active {
                                &alternate_mapping
                            } else {
                                &normal_mapping
                            };

                            // Negative direction activation
                            if abs_i.minimum < 0 && axis_value <= rel_thr && old_val > rel_thr {
                                let neg_input = GamepadInput::Axis(ax.0, Direction::Negative);
                                activate_mapping(
                                    &neg_input,
                                    config,
                                    current_mapping,
                                    &chrmap,
                                    &mut virtual_keyboard,
                                    &mut pressed_mappings,
                                    &mut modifiers,
                                    friendly,
                                )?;
                            }
                            // Positive direction activation
                            else if abs_i.maximum > 0
                                && axis_value >= act_thr
                                && old_val < act_thr
                            {
                                let pos_input = GamepadInput::Axis(ax.0, Direction::Positive);
                                activate_mapping(
                                    &pos_input,
                                    config,
                                    current_mapping,
                                    &chrmap,
                                    &mut virtual_keyboard,
                                    &mut pressed_mappings,
                                    &mut modifiers,
                                    friendly,
                                )?;
                            }
                            // Release
                            else if axis_value.abs() < act_thr && old_val.abs() >= act_thr {
                                let neg_input = GamepadInput::Axis(ax.0, Direction::Negative);
                                release_mapping(
                                    &neg_input,
                                    current_mapping,
                                    &mut virtual_keyboard,
                                    &mut pressed_mappings,
                                    &mut modifiers,
                                    &get_display_name(&neg_input, &config.friendly_names, friendly),
                                )?;
                                let pos_input = GamepadInput::Axis(ax.0, Direction::Positive);
                                release_mapping(
                                    &pos_input,
                                    current_mapping,
                                    &mut virtual_keyboard,
                                    &mut pressed_mappings,
                                    &mut modifiers,
                                    &get_display_name(&pos_input, &config.friendly_names, friendly),
                                )?;
                            }
                        }
                    }
                    axis_states.insert(ax.0, axis_value);
                }

                _ => {}
            }
        }
    }
}

/// The main loop for handling input from a selected device.  
/// 1. We attempt to select a device (either known or unknown).  
/// 2. Depending on the selection, we either use an ephemeral config or a parsed config.
/// 3. We then handle the device using a unified event loop.
///
/// This refactoring eliminates duplicated code by centralizing the event loop logic.
///
/// # Parameters
/// - `args`: Parsed command-line arguments.
///
/// # Returns
/// - `io::Result<()>`: Returns `Ok(())` on success or an `io::Error` on failure.
pub fn run_main_loop(args: &crate::cli::Args) -> std::io::Result<()> {
    let selection = match crate::device::attempt_device_selection(args) {
        Some(s) => s,
        None => {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "No supported device found",
            ));
        }
    };

    // Apply device-specific quirks (e.g. Steam Deck lizard-mode disable)
    // Keep the returned guard alive for as long as the main loop runs.
    let _quirk_guard = crate::quirks::apply_for_selected_device(&selection);

    match selection {
        // User wants an unknown device => create ephemeral config & proceed
        // We ALWAYS do auto mapping for unknown devices, ignoring `-m`.
        SelectedDevice::Unknown(unk) => {
            println!(
                "No config file was found for device: {} ({}) \n\
                If you want to create a config file for this device, use the following path:\n    \
                /etc/deckrypt/{:04x}_{:04x}.toml",
                unk.path, unk.name, unk.vendor_id, unk.product_id
            );
            println!("Using automatic mapping for all buttons/axes...");

            // Build an empty ephemeral config
            let ephemeral_cfg = ControllerConfig {
                required_buttons: HashSet::new(),
                required_axes: HashSet::new(),
                button_mappings: vec![],
                axis_mappings: vec![],
                alternate_button_mappings: vec![],
                alternate_axis_mappings: vec![],
                modifiers: Modifiers::default(),
                friendly_names: HashMap::new(),
                devices: Vec::new(),
            };

            // Handle the device with automatic mapping enabled
            handle_device(
                &unk.path,
                &ephemeral_cfg,
                /* auto_mapping_enabled = */ true,
                args.friendly,
            )
        }

        // User chose a known device => parse the config and proceed
        SelectedDevice::Known(kdev) => {
            // Parse the controller config
            let parsed_cfg = match crate::config::parse_controller_config(
                kdev.vendor_id,
                kdev.product_id,
                kdev.config_file_path,
            ) {
                Some(cfg) => cfg,
                None => {
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        "Failed to parse config for this device",
                    ));
                }
            };

            // Determine if automatic mapping should be enabled based on `-m`
            let auto_mapping_enabled = args.mapping;

            // Handle the device with the appropriate mapping flag
            handle_device(&kdev.path, &parsed_cfg, auto_mapping_enabled, args.friendly)
        }
    }
}
