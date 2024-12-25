use libc::input_absinfo;
use log::{debug, error, trace};
use std::collections::{HashMap, HashSet};
use std::io;

use evdev::{
    uinput::{VirtualDevice, VirtualDeviceBuilder},
    Device, EventType, InputEvent, InputEventKind, Key,
};

use crate::keymap::{generate_chrmap, shift_transform, ALLOWED_CHARACTERS};
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
            name.clone()
        } else {
            format!("{:?}", gamepad_input)
        }
    } else {
        format!("{:?}", gamepad_input)
    }
}

/// Handles activation of a mapping (e.g., axis crossing threshold).  
/// This could be pressing a character or a special key, taking into account SHIFT if it’s active.
fn handle_mapping_activation(
    gamepad_input: &GamepadInput,
    mapping_value: &Mapping,
    chrmap: &HashMap<char, (Key, u8)>,
    virtual_keyboard: &mut VirtualDevice,
    pressed_axes: &mut HashMap<GamepadInput, PressedMapping>,
    modifiers: &Modifiers,
) -> std::io::Result<()> {
    match mapping_value {
        Mapping::Character(mut ch) => {
            if modifiers.shift_active {
                ch = shift_transform(ch);
            }
            if let Some(&(keycode, lvl)) = chrmap.get(&ch) {
                handle_kernel_modifier(lvl, Action::Activate, virtual_keyboard)?;
                let e = InputEvent::new(EventType::KEY, keycode.code(), 1);
                virtual_keyboard.emit(&[e])?;

                pressed_axes.insert(
                    gamepad_input.clone(),
                    PressedMapping::Character {
                        keycode,
                        level: lvl,
                    },
                );
            }
            debug!("Activated {:?} ({})", gamepad_input, ch);
        }
        Mapping::Key(kc) => {
            let e = InputEvent::new(EventType::KEY, kc.code(), 1);
            virtual_keyboard.emit(&[e])?;
            pressed_axes.insert(gamepad_input.clone(), PressedMapping::Key(*kc));
            debug!("Activated {:?} ({:?})", gamepad_input, kc);
        }
    }
    Ok(())
}

/// Handles release of a mapping (e.g., axis returning to neutral).  
/// This corresponds to sending a KEY UP event and releasing any SHIFT/ALTGR if it was pressed.
fn handle_mapping_release(
    gamepad_input: &GamepadInput,
    mapping_value: &Mapping,
    vk: &mut VirtualDevice,
    pressed_axes: &mut HashMap<GamepadInput, PressedMapping>,
    modifiers: &Modifiers,
) -> io::Result<()> {
    let shift_active = modifiers.shift_active == true;
    if let Some(pressed) = pressed_axes.remove(gamepad_input) {
        match pressed {
            PressedMapping::Character { keycode, level } => {
                let evt = InputEvent::new(EventType::KEY, keycode.code(), 0);
                vk.emit(&[evt])?;
                handle_kernel_modifier(level, Action::Deactivate, vk)?;
            }
            PressedMapping::Key(kc) => {
                let evt = InputEvent::new(EventType::KEY, kc.code(), 0);
                vk.emit(&[evt])?;
            }
        }
        match mapping_value {
            Mapping::Character(mut mapping_value) => {
                if shift_active {
                    mapping_value = shift_transform(mapping_value);
                }
                debug!("Deactivated {:?} ({})", gamepad_input, mapping_value)
            }
            Mapping::Key(mapping_value) => {
                debug!("Deactivated {:?} ({:?})", gamepad_input, mapping_value)
            }
        };
    }
    Ok(())
}

/// Builds up a final set of normal and alternate mappings by:
/// 1. Using any user-specified manual/axis mappings from the config (which may be empty for unknown).
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

    // Controls whether we auto-map leftover inputs:
    auto_mapping_enabled: bool,
) -> BuiltMappings {
    let manual_mappings = &config.manual_mappings;
    let axis_mappings = &config.axis_mappings;
    let alternate_manual_mappings = &config.alternate_manual_mappings;
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

    // -------------------------------
    // 1) Insert manual + axis mappings into normal
    // -------------------------------
    for &(ref gi, ref mapping) in manual_mappings {
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
    // 2) Insert alternate manual + axis mappings
    // -------------------------------
    for &(ref gi, ref mapping) in alternate_manual_mappings {
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
    let available_gamepad_inputs: Vec<_> = all_gamepad_inputs
        .iter()
        .filter(|gi| !normal_mapping.contains_key(*gi) && !alternate_mapping.contains_key(*gi))
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
        for gi in &available_gamepad_inputs {
            if let Some(next_char) = normal_chars_iter.next() {
                normal_mapping.insert(gi.clone(), Mapping::Character(next_char));
                used_chars.insert(next_char);
                trace!("Automatically mapped {:?} -> '{}'", gi, next_char);
            } else {
                break;
            }
        }

        // 4) Then digits
        let remaining_gamepad_inputs: Vec<_> = available_gamepad_inputs
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
                trace!("Automatically mapped {:?} -> '{}'", gi, next_char);
            } else {
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
            .filter(|c| !c.is_ascii_alphanumeric() && !c.is_whitespace() && !c.is_control() && !used_chars.contains(c))
            .collect();
        let mut symbols_iter = available_symbols.into_iter();
        for gi in &remaining_gamepad_inputs {
            if let Some(next_char) = symbols_iter.next() {
                normal_mapping.insert(gi.clone(), Mapping::Character(next_char));
                used_chars.insert(next_char);
                trace!("Automatically mapped {:?} -> '{}'", gi, next_char);
            } else {
                break;
            }
        }
    }

    // -------------------------------
    // Special ENTER if found
    // -------------------------------
    let mut special_enter_input: Option<GamepadInput> = None;
    for (inp, m) in &normal_mapping {
        if let Mapping::Key(Key::KEY_ENTER) = m {
            special_enter_input = Some(inp.clone());
            break;
        }
    }

    // -------------------------------
    // 6) For the alternate mapping, fill in reversed leftover
    // but only if auto_mapping_enabled
    // -------------------------------
    if auto_mapping_enabled {
        // Mark used chars from both normal and alternate
        for mapping in normal_mapping.values().chain(alternate_mapping.values()) {
            if let Mapping::Character(c) = mapping {
                used_chars.insert(*c);
            }
        }

        // Collect all available characters for alternate mapping in the correct order
        let mut alternate_chars_iter = ALLOWED_CHARACTERS
            .iter()
            .cloned()
            .filter(|c| !used_chars.contains(c))
            .collect::<Vec<char>>()
            .into_iter();

        for gi in &available_gamepad_inputs {
            if !alternate_mapping.contains_key(gi) {
                if let Some(c) = alternate_chars_iter.next() {
                    alternate_mapping.insert(gi.clone(), Mapping::Character(c));
                    used_chars.insert(c);
                    trace!("Alternate mapped {:?} -> '{}'", gi, c);
                }
            }
        }

        // Ensure special_enter_input is not overridden in alternate mapping
        if let Some(ref enter_inp) = special_enter_input {
            if alternate_mapping.contains_key(enter_inp) {
                error!(
                    "Warning: The button assigned to ENTER in normal mode cannot be overridden in alternate mode."
                );
                alternate_mapping.remove(enter_inp);
            }
        }
    }

    BuiltMappings {
        normal_mapping,
        alternate_mapping,
        special_enter_input,
    }
}

fn handle_modifier_state(
    gamepad_input: &GamepadInput,
    modifier_label: &str,
    ev: &InputEvent,
    friendly: bool,
    config: &ControllerConfig,
    modifiers: &mut Modifiers,
) {
    let is_active = ev.value() == 1;
    match modifier_label {
        "SHIFT" => modifiers.shift_active = is_active,
        "ALTERNATE" => modifiers.alternate_active = is_active,
        _ => {}
    }
    if friendly {
        let friendly_name = config
            .friendly_names
            .get(gamepad_input)
            .unwrap_or(&modifier_label.to_string())
            .clone();
        debug!(
            "{} {} ({})",
            if is_active {
                "Activated"
            } else {
                "Deactivated"
            },
            friendly_name,
            modifier_label
        );
    }
}

fn handle_key_activation(
    gamepad_input: &GamepadInput,
    config: &ControllerConfig,
    modifiers: &mut Modifiers,
    keycode: Key,
    virtual_keyboard: &mut VirtualDevice,
    pressed_inputs: &mut HashMap<GamepadInput, bool>,
    friendly: bool,
    shift_name: &str,
    alt_name: &str,
    output: Option<char>,
) -> io::Result<()> {
    let e = InputEvent::new(EventType::KEY, keycode.code(), 1);
    virtual_keyboard.emit(&[e])?;
    pressed_inputs.insert(gamepad_input.clone(), modifiers.alternate_active);
    if friendly == true {
        let display_name = get_display_name(&gamepad_input, &config.friendly_names, friendly);
        let mut log_str = display_name.clone();
        if modifiers.shift_active {
            log_str = format!("{} + {}", shift_name, log_str);
        }
        if modifiers.alternate_active {
            log_str = format!("{} + {}", alt_name, log_str);
        }
        println!("{}", log_str);
    }
    match output {
        Some(ch) => debug!("Activated {:?} ({})", gamepad_input, ch),
        None => debug!("Activated {:?} ({:?})", gamepad_input, e),
    }
    Ok(())
}

fn handle_axis_activation(
    gamepad_input: GamepadInput,
    current_mapping: &HashMap<GamepadInput, Mapping>,
    chrmap: &HashMap<char, (Key, u8)>,
    mut pressed_axes: &mut HashMap<GamepadInput, PressedMapping>,
    config: &ControllerConfig,
    modifiers: &mut Modifiers,
    mut virtual_keyboard: &mut VirtualDevice,
    friendly: bool,
    shift_name: &str,
    alt_name: &str,
) -> io::Result<()> {
    if let Some(mval) = current_mapping.get(&gamepad_input) {
        handle_mapping_activation(
            &gamepad_input,
            mval,
            &chrmap,
            &mut virtual_keyboard,
            &mut pressed_axes,
            &modifiers,
        )?;

        if friendly == true {
            let display_name = get_display_name(&gamepad_input, &config.friendly_names, friendly);
            let mut log_str = display_name.clone();
            if modifiers.shift_active {
                log_str = format!("{} + {}", shift_name, log_str);
            }
            if modifiers.alternate_active {
                log_str = format!("{} + {}", alt_name, log_str);
            }
            println!("{}", log_str);
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

    // Try reading axis info
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

    // Read kernel keymap
    let (chrmap, _shifted_chars) = match generate_chrmap() {
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
        auto_mapping_enabled,
    );
    let normal_mapping = built.normal_mapping;
    let alternate_mapping = built.alternate_mapping;
    let special_enter_input = built.special_enter_input;

    let mut axis_states: HashMap<u16, i32> = HashMap::new();
    let mut pressed_inputs: HashMap<GamepadInput, bool> = HashMap::new();
    let mut pressed_axes: HashMap<GamepadInput, PressedMapping> = HashMap::new();
    let mut modifiers = config.modifiers.clone();
    let alt_name = match modifiers.alternate_modifier.clone() {
        Some(modifier) => get_display_name(&modifier, &config.friendly_names, friendly),
        None => "ALT".to_string(),
    };
    let shift_name = match modifiers.shift_modifier.clone() {
        Some(modifier) => get_display_name(&modifier, &config.friendly_names, friendly),
        None => "SHIFT".to_string(),
    };

    debug!("Listening for gamepad events...");

    // Main event loop
    loop {
        for ev in gamepad_device.fetch_events()? {
            match ev.kind() {
                InputEventKind::Key(k) => {
                    let gamepad_input = GamepadInput::Button(k);

                    // Handle modifiers
                    if Some(gamepad_input.clone()) == config.modifiers.shift_modifier {
                        handle_modifier_state(
                            &gamepad_input,
                            "SHIFT",
                            &ev,
                            friendly,
                            config,
                            &mut modifiers,
                        );
                        continue;
                    }
                    if Some(gamepad_input.clone()) == config.modifiers.alternate_modifier {
                        handle_modifier_state(
                            &gamepad_input,
                            "ALTERNATE",
                            &ev,
                            friendly,
                            config,
                            &mut modifiers,
                        );
                        continue;
                    }

                    // Handle special enter
                    if Some(gamepad_input.clone()) == special_enter_input {
                        let key_to_emit = if modifiers.shift_active && modifiers.alternate_active {
                            Key::KEY_ESC
                        } else if modifiers.shift_active || modifiers.alternate_active {
                            Key::KEY_BACKSPACE
                        } else {
                            Key::KEY_ENTER
                        };
                        let val = ev.value();
                        let e = InputEvent::new(EventType::KEY, key_to_emit.code(), val);
                        virtual_keyboard.emit(&[e])?;
                        let action = if val == 1 { "Activated" } else { "Deactivated" };
                        let key_str =
                            get_display_name(&gamepad_input, &config.friendly_names, friendly);
                        debug!("{} {} ({:?})", action, key_str, key_to_emit);
                        continue;
                    }

                    let is_pressed = ev.value() == 1;
                    let mapping = if modifiers.alternate_active {
                        &alternate_mapping
                    } else {
                        &normal_mapping
                    };

                    if is_pressed {
                        if let Some(mapval) = mapping.get(&gamepad_input) {
                            match mapval {
                                Mapping::Character(mut ch) => {
                                    if modifiers.shift_active {
                                        ch = shift_transform(ch);
                                    }
                                    if let Some(&(keycode, lvl)) = chrmap.get(&ch) {
                                        handle_kernel_modifier(
                                            lvl,
                                            Action::Activate,
                                            &mut virtual_keyboard,
                                        )?;
                                        handle_key_activation(
                                            &gamepad_input,
                                            config,
                                            &mut modifiers,
                                            keycode,
                                            &mut virtual_keyboard,
                                            &mut pressed_inputs,
                                            friendly,
                                            &shift_name,
                                            &alt_name,
                                            Some(ch),
                                        )?;
                                    }
                                }
                                Mapping::Key(kc) => {
                                    handle_key_activation(
                                        &gamepad_input,
                                        config,
                                        &mut modifiers,
                                        *kc,
                                        &mut virtual_keyboard,
                                        &mut pressed_inputs,
                                        friendly,
                                        &shift_name,
                                        &alt_name,
                                        None,
                                    )?;
                                }
                            }
                        }
                    } else {
                        // Release
                        if let Some(&was_alt) = pressed_inputs.get(&gamepad_input) {
                            let mapval = if was_alt {
                                &alternate_mapping
                            } else {
                                &normal_mapping
                            };
                            if let Some(mapping_value) = mapval.get(&gamepad_input) {
                                match mapping_value {
                                    Mapping::Character(mut ch) => {
                                        if modifiers.shift_active {
                                            ch = shift_transform(ch);
                                        }
                                        if let Some(&(keycode, lvl)) = chrmap.get(&ch) {
                                            let e =
                                                InputEvent::new(EventType::KEY, keycode.code(), 0);
                                            virtual_keyboard.emit(&[e])?;
                                            handle_kernel_modifier(
                                                lvl,
                                                Action::Deactivate,
                                                &mut virtual_keyboard,
                                            )?;
                                        }
                                        debug!("Deactivated {:?} ({})", gamepad_input, ch);
                                    }
                                    Mapping::Key(kc) => {
                                        let e = InputEvent::new(EventType::KEY, kc.code(), 0);
                                        virtual_keyboard.emit(&[e])?;
                                        debug!("Deactivated {:?} ({:?})", gamepad_input, e);
                                    }
                                }
                            }
                            pressed_inputs.remove(&gamepad_input);
                        }
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

                            // Negative direction
                            if abs_i.minimum < 0 && axis_value <= rel_thr && old_val > rel_thr {
                                let neg_input = GamepadInput::Axis(ax.0, Direction::Negative);
                                handle_axis_activation(
                                    neg_input,
                                    &current_mapping,
                                    &chrmap,
                                    &mut pressed_axes,
                                    config,
                                    &mut modifiers,
                                    &mut virtual_keyboard,
                                    friendly,
                                    &shift_name,
                                    &alt_name,
                                )?;
                            }
                            // Positive direction
                            else if abs_i.maximum > 0
                                && axis_value >= act_thr
                                && old_val < act_thr
                            {
                                let pos_input = GamepadInput::Axis(ax.0, Direction::Positive);
                                handle_axis_activation(
                                    pos_input,
                                    &current_mapping,
                                    &chrmap,
                                    &mut pressed_axes,
                                    config,
                                    &mut modifiers,
                                    &mut virtual_keyboard,
                                    friendly,
                                    &shift_name,
                                    &alt_name,
                                )?;
                            }
                            // Release
                            else if axis_value.abs() < act_thr && old_val.abs() >= act_thr {
                                let neg_input = GamepadInput::Axis(ax.0, Direction::Negative);
                                if let Some(mval) = current_mapping.get(&neg_input) {
                                    handle_mapping_release(
                                        &neg_input,
                                        mval,
                                        &mut virtual_keyboard,
                                        &mut pressed_axes,
                                        &modifiers
                                    )?;
                                }
                                let pos_input = GamepadInput::Axis(ax.0, Direction::Positive);
                                if let Some(mval) = current_mapping.get(&pos_input) {
                                    handle_mapping_release(
                                        &pos_input,
                                        mval,
                                        &mut virtual_keyboard,
                                        &mut pressed_axes,
                                        &modifiers
                                    )?;
                                }
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
                manual_mappings: vec![],
                axis_mappings: vec![],
                alternate_manual_mappings: vec![],
                alternate_axis_mappings: vec![],
                modifiers: Modifiers::default(),
                friendly_names: HashMap::new(), // No friendly names for unknown devices
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
            let parsed_cfg =
                match crate::config::parse_controller_config(kdev.vendor_id, kdev.product_id) {
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
