use evdev::{
    uinput::VirtualDeviceBuilder, AbsoluteAxisType, AttributeSet, Device, EventType, InputEvent,
    InputEventKind, Key,
};
use libc::{c_int, getuid, input_absinfo};
use std::collections::{HashMap, HashSet};
use std::fs::{self, File};
use std::io::{self, Read, Write};
use std::os::unix::fs::FileTypeExt;
use std::os::unix::io::AsRawFd;
use std::fmt;

// Include the toml crate in your Cargo.toml dependencies
use toml::Value as TomlValue;

// Include the sudo crate
use sudo;

// Include the clap crate for command-line argument parsing
use clap::Parser;

const AXIS_THRESHOLD_PERCENTAGE: f32 = 0.5;
const KEY_MAX: u16 = 255;

#[derive(Hash, Eq, PartialEq, Debug, Clone)]
enum Direction {
    Positive,
    Negative,
}

#[derive(Hash, Eq, PartialEq, Clone)]
enum GamepadInput {
    Button(Key),
    Axis(u16, Direction), // Use u16 instead of AbsoluteAxisType
}

// Modifiers based on configuration
#[derive(Default)]
struct Modifiers {
    shift_modifier: Option<GamepadInput>,
    alternate_modifier: Option<GamepadInput>,
    alternate_active: bool,
}

// Verbosity levels
#[derive(PartialEq, PartialOrd, Clone, Copy)]
enum Verbosity {
    Quiet,
    Verbose,
    VeryVerbose,
}

// Mapping can be a character or a special key
#[derive(Clone, Debug)]
enum Mapping {
    Character(char),
    Key(Key),
}

// FFI bindings to libkeymap
#[repr(C)]
pub struct LkCtx {
    _unused: [u8; 0],
}

extern "C" {
    pub fn lk_init() -> *mut LkCtx;
    pub fn lk_free(ctx: *mut LkCtx);
    pub fn lk_kernel_keymap(ctx: *mut LkCtx, fd: c_int) -> c_int;
    pub fn lk_get_key(ctx: *mut LkCtx, level: c_int, code: c_int) -> c_int;
}

fn ktyp(x: c_int) -> c_int {
    (x >> 8) & 0xFF
}

fn kval(x: c_int) -> c_int {
    x & 0xFF
}

const KT_LATIN: c_int = 0;
const KT_LETTER: c_int = 11;

impl fmt::Debug for GamepadInput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GamepadInput::Button(key) => {
                write!(f, "Button({:?})", key)
            }
            GamepadInput::Axis(axis_code, direction) => {
                let axis_name = absolute_axis_type_to_name(*axis_code).unwrap_or("Unknown");
                write!(f, "Axis({}, {:?})", axis_name, direction)
            }
        }
    }
}

// Function to generate the character map
fn generate_chrmap() -> Option<(HashMap<char, (Key, u8)>, HashMap<char, char>)> {
    let mut fd: Option<File> = None;

    if let Ok(tty) = std::env::var("TTY") {
        if tty.starts_with("/dev/tty") {
            if let Ok(f) = File::open(tty) {
                fd = Some(f);
            }
        }
    }

    if fd.is_none() {
        if let Ok(f) = File::open("/dev/console") {
            fd = Some(f);
        } else if unsafe { getuid() } == 0 {
            if let Ok(f) = File::open("/dev/tty0") {
                fd = Some(f);
            }
        }
    }

    if fd.is_none() {
        eprintln!(
            "Needs to run on tty/console or as root to read the kernel keymap. Cannot read keymap."
        );
        return None;
    }

    let fd = fd.unwrap();

    unsafe {
        let ctx = lk_init();
        if ctx.is_null() {
            eprintln!("Failed to initialize libkeymap context");
            return None;
        }

        if lk_kernel_keymap(ctx, fd.as_raw_fd()) != 0 {
            eprintln!("Failed to read kernel keymap");
            lk_free(ctx);
            return None;
        }

        // Now traverse the keymap
        let mut chrmap = HashMap::new();
        let mut shifted_chars = HashMap::new();

        for i in 1..=KEY_MAX as i32 {
            let mut chars = [None, None, None]; // for levels 0..2

            for j in 0..3 {
                let code = lk_get_key(ctx, j, i);
                let ktyp = ktyp(code);
                let kval = kval(code);

                if ktyp == KT_LATIN || ktyp == KT_LETTER {
                    if let Some(c) = std::char::from_u32(kval as u32) {
                        chars[j as usize] = Some(c);

                        let keycode = Key::new(i as u16);
                        let modifier = j as u8;
                        chrmap.entry(c).or_insert((keycode, modifier));
                    }
                } else if code == 513 && !chrmap.contains_key(&'\n') {
                    let keycode = Key::new(i as u16);
                    let modifier = j as u8;
                    chrmap.insert('\n', (keycode, modifier));
                }
            }

            // Build shifted_chars mapping
            if let (Some(c0), Some(c1)) = (chars[0], chars[1]) {
                // Only consider digits for shifted_chars
                if c0.is_ascii_digit() {
                    shifted_chars.insert(c0, c1);
                }
            }
        }

        lk_free(ctx);

        Some((chrmap, shifted_chars))
    }
}

fn create_virtual_keyboard(
    all_keyboard_keys: &[Key],
) -> std::io::Result<evdev::uinput::VirtualDevice> {
    let mut keys = AttributeSet::<Key>::new();
    for &key in all_keyboard_keys {
        keys.insert(key);
    }
    // Add modifier keys
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

// Function to get the activation threshold (percentage of maximum) and release threshold (percentage of minimum)
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

// Function to get the reversed character
fn get_reversed_char(c: char) -> char {
    if c.is_ascii_lowercase() {
        let offset = c as u8 - b'a';
        let reversed = b'z' - offset;
        reversed as char
    } else if c.is_ascii_uppercase() {
        let offset = c as u8 - b'A';
        let reversed = b'Z' - offset;
        reversed as char
    } else if c.is_ascii_digit() {
        // For digits, reverse '0'-'9'
        let offset = c as u8 - b'0';
        let reversed = b'9' - offset;
        reversed as char
    } else {
        // For other characters, return the same character
        c
    }
}

// Function to map gamepad events to mappings (both buttons and axes)
fn get_mappings(
    device: &Device,
    manual_mappings: &[(GamepadInput, Mapping)],
    axis_mappings: &[(GamepadInput, Mapping)],
    alternate_manual_mappings: &[(GamepadInput, Mapping)],
    alternate_axis_mappings: &[(GamepadInput, Mapping)],
    normal_chars_iter: impl Iterator<Item = char> + Clone,
    additional_signs: &[char],
    abs_info_map: Option<&HashMap<u16, input_absinfo>>,
    modifiers: &Modifiers,
    verbosity: Verbosity,
) -> (
    HashMap<GamepadInput, Mapping>,
    HashSet<char>,
    HashMap<GamepadInput, Mapping>,
) {
    let mut normal_mapping = HashMap::new();
    let mut alternate_mapping = HashMap::new();
    let mut used_chars = HashSet::new();

    // Add the modifier keys to the set to avoid assigning characters to them
    let mut modifier_inputs = HashSet::new();
    if let Some(ref mod1) = modifiers.shift_modifier {
        modifier_inputs.insert(mod1.clone());
    }
    if let Some(ref mod2) = modifiers.alternate_modifier {
        modifier_inputs.insert(mod2.clone());
    }

    // Map manual_mappings for normal mapping
    for &(ref gamepad_input, ref mapping) in manual_mappings {
        normal_mapping.insert(gamepad_input.clone(), mapping.clone());
        if let Mapping::Character(character) = mapping {
            used_chars.insert(*character);
        }
    }

    // Map axis_mappings for normal mapping
    for &(ref gamepad_input, ref mapping) in axis_mappings {
        normal_mapping.insert(gamepad_input.clone(), mapping.clone());
        if let Mapping::Character(character) = mapping {
            used_chars.insert(*character);
        }
    }

    // Map alternate_manual_mappings for alternate mapping
    for &(ref gamepad_input, ref mapping) in alternate_manual_mappings {
        alternate_mapping.insert(gamepad_input.clone(), mapping.clone());
        if let Mapping::Character(character) = mapping {
            used_chars.insert(*character);
        }
    }

    // Map alternate_axis_mappings for alternate mapping
    for &(ref gamepad_input, ref mapping) in alternate_axis_mappings {
        alternate_mapping.insert(gamepad_input.clone(), mapping.clone());
        if let Mapping::Character(character) = mapping {
            used_chars.insert(*character);
        }
    }

    // Collect all gamepad inputs that are not modifiers
    let mut all_gamepad_inputs = Vec::new();

    if let Some(supported_keys) = device.supported_keys() {
        for key in supported_keys.iter() {
            let gamepad_input = GamepadInput::Button(key);
            if !modifier_inputs.contains(&gamepad_input) {
                all_gamepad_inputs.push(gamepad_input);
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

    // Exclude already mapped inputs
    let available_gamepad_inputs: Vec<_> = all_gamepad_inputs
        .iter()
        .filter(|gi| !normal_mapping.contains_key(*gi) && !alternate_mapping.contains_key(*gi))
        .cloned()
        .collect();

    // Assign characters to unmapped inputs in normal mapping
    let normal_letters: Vec<char> = normal_chars_iter
        .clone()
        .filter(|c| c.is_ascii_lowercase() && !used_chars.contains(c))
        .collect();

    let mut normal_chars_iter = normal_letters.into_iter();

    for gamepad_input in &available_gamepad_inputs {
        if let Some(next_char) = normal_chars_iter.next() {
            normal_mapping.insert(gamepad_input.clone(), Mapping::Character(next_char));
            used_chars.insert(next_char);
            if verbosity >= Verbosity::VeryVerbose {
                println!(
                    "Automatically mapped gamepad input {:?} to character '{}'",
                    gamepad_input, next_char
                );
            }
        } else {
            break;
        }
    }

    // If there are still unmapped gamepad inputs, assign numbers '0' to '9'
    let remaining_gamepad_inputs: Vec<_> = available_gamepad_inputs
        .iter()
        .filter(|gi| !normal_mapping.contains_key(*gi))
        .cloned()
        .collect();

    let normal_numbers: Vec<char> = ('0'..='9').filter(|c| !used_chars.contains(c)).collect();

    let mut numbers_iter = normal_numbers.into_iter();

    for gamepad_input in &remaining_gamepad_inputs {
        if let Some(next_char) = numbers_iter.next() {
            normal_mapping.insert(gamepad_input.clone(), Mapping::Character(next_char));
            used_chars.insert(next_char);
            if verbosity >= Verbosity::VeryVerbose {
                println!(
                    "Automatically mapped gamepad input {:?} to character '{}'",
                    gamepad_input, next_char
                );
            }
        } else {
            break;
        }
    }

    // If there are still unmapped gamepad inputs, assign symbols
    let remaining_gamepad_inputs: Vec<_> = remaining_gamepad_inputs
        .iter()
        .filter(|gi| !normal_mapping.contains_key(*gi))
        .cloned()
        .collect();

    let available_symbols: Vec<char> = additional_signs
        .iter()
        .cloned()
        .filter(|c| !used_chars.contains(c))
        .collect();

    let mut symbols_iter = available_symbols.into_iter();

    for gamepad_input in &remaining_gamepad_inputs {
        if let Some(next_char) = symbols_iter.next() {
            normal_mapping.insert(gamepad_input.clone(), Mapping::Character(next_char));
            used_chars.insert(next_char);
            if verbosity >= Verbosity::VeryVerbose {
                println!(
                    "Automatically mapped gamepad input {:?} to symbol '{}'",
                    gamepad_input, next_char
                );
            }
        } else {
            break;
        }
    }

    // Now, create alternate mapping for inputs not already in alternate_mapping

    // Collect all gamepad inputs from normal_mapping
    let unmapped_gamepad_inputs: Vec<_> = all_gamepad_inputs
        .iter()
        .filter(|gi| !alternate_mapping.contains_key(*gi))
        .cloned()
        .collect();

    // Ensure used_chars includes characters from both normal and alternate mappings
    for mapping in normal_mapping.values().chain(alternate_mapping.values()) {
        if let Mapping::Character(c) = mapping {
            used_chars.insert(*c);
        }
    }

    // For gamepad inputs that have a character assigned in normal mapping but not in alternate mapping,
    // assign the opposite character in alternate mapping, if not already used
    for gamepad_input in &unmapped_gamepad_inputs {
        if let Some(Mapping::Character(normal_char)) = normal_mapping.get(gamepad_input) {
            let mut assigned_char = None;

            if normal_char.is_ascii_lowercase() || normal_char.is_ascii_uppercase() {
                let opposite_char = get_reversed_char(*normal_char);
                if !used_chars.contains(&opposite_char) {
                    assigned_char = Some(opposite_char);
                }
            }

            // If opposite character is not assigned, assign next available character
            if assigned_char.is_none() {
                // Try letters first
                let mut available_letters = ('a'..='z')
                    .rev()
                    .filter(|c| !used_chars.contains(c))
                    .collect::<Vec<char>>();

                if let Some(next_char) = available_letters.pop() {
                    assigned_char = Some(next_char);
                } else {
                    // Try numbers
                    let mut available_numbers = ('0'..='9')
                        .filter(|c| !used_chars.contains(c))
                        .collect::<Vec<char>>();

                    if let Some(next_char) = available_numbers.pop() {
                        assigned_char = Some(next_char);
                    } else {
                        // Try symbols
                        let mut available_symbols = additional_signs
                            .iter()
                            .cloned()
                            .filter(|c| !used_chars.contains(c))
                            .collect::<Vec<char>>();

                        if let Some(next_char) = available_symbols.pop() {
                            assigned_char = Some(next_char);
                        }
                    }
                }
            }

            if let Some(c) = assigned_char {
                alternate_mapping.insert(gamepad_input.clone(), Mapping::Character(c));
                used_chars.insert(c);
                if verbosity >= Verbosity::VeryVerbose {
                    println!(
                        "Automatically mapped gamepad input {:?} to character '{}' (alternate)",
                        gamepad_input, c
                    );
                }
            }
        }
    }

    (normal_mapping, used_chars, alternate_mapping)
}

fn get_buttons_and_axes_from_config(
    config_file_path: &str,
) -> std::io::Result<(HashSet<Key>, HashSet<u16>)> {
    let mut file = File::open(config_file_path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    let config = contents.parse::<TomlValue>().map_err(|e| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!("Failed to parse config: {}", e),
        )
    })?;

    let mut buttons_in_config = HashSet::new();
    let mut axes_in_config = HashSet::new();

    // Parse button mappings
    if let Some(buttons) = config.get("buttons").and_then(|v| v.as_table()) {
        for (key_name, _value) in buttons {
            if let Some(gamepad_input) = parse_gamepad_input(key_name) {
                match gamepad_input {
                    GamepadInput::Button(key) => {
                        buttons_in_config.insert(key);
                    }
                    GamepadInput::Axis(axis, _) => {
                        axes_in_config.insert(axis);
                    }
                }
            }
        }
    }

    // Parse axis mappings
    if let Some(axes) = config.get("axes").and_then(|v| v.as_table()) {
        for (axis_name, _) in axes {
            if let Some(axis_type) = axis_name_to_absolute_axis_type(axis_name) {
                axes_in_config.insert(axis_type.0);
            }
        }
    }

    // Do the same for alternate_buttons and alternate_axes
    if let Some(buttons) = config.get("alternate_buttons").and_then(|v| v.as_table()) {
        for (key_name, _value) in buttons {
            if let Some(gamepad_input) = parse_gamepad_input(key_name) {
                match gamepad_input {
                    GamepadInput::Button(key) => {
                        buttons_in_config.insert(key);
                    }
                    GamepadInput::Axis(axis, _) => {
                        axes_in_config.insert(axis);
                    }
                }
            }
        }
    }

    if let Some(axes) = config.get("alternate_axes").and_then(|v| v.as_table()) {
        for (axis_name, _) in axes {
            if let Some(axis_type) = axis_name_to_absolute_axis_type(axis_name) {
                axes_in_config.insert(axis_type.0);
            }
        }
    }

    // Parse modifiers
    if let Some(mods) = config.get("modifiers").and_then(|v| v.as_table()) {
        if let Some(shift_key) = mods.get("shift_key").and_then(|v| v.as_str()) {
            if let Some(gamepad_input) = parse_gamepad_input(shift_key) {
                match gamepad_input {
                    GamepadInput::Button(key) => {
                        buttons_in_config.insert(key);
                    }
                    GamepadInput::Axis(axis, _) => {
                        axes_in_config.insert(axis);
                    }
                }
            }
        }
        if let Some(alternate_key) = mods.get("alternate_key").and_then(|v| v.as_str()) {
            if let Some(gamepad_input) = parse_gamepad_input(alternate_key) {
                match gamepad_input {
                    GamepadInput::Button(key) => {
                        buttons_in_config.insert(key);
                    }
                    GamepadInput::Axis(axis, _) => {
                        axes_in_config.insert(axis);
                    }
                }
            }
        }
    }

    Ok((buttons_in_config, axes_in_config))
}

fn absolute_axis_type_to_name(axis_code: u16) -> Option<&'static str> {
    match AbsoluteAxisType(axis_code) {
        AbsoluteAxisType::ABS_X => Some("ABS_X"),
        AbsoluteAxisType::ABS_Y => Some("ABS_Y"),
        AbsoluteAxisType::ABS_Z => Some("ABS_Z"),
        AbsoluteAxisType::ABS_RX => Some("ABS_RX"),
        AbsoluteAxisType::ABS_RY => Some("ABS_RY"),
        AbsoluteAxisType::ABS_RZ => Some("ABS_RZ"),
        AbsoluteAxisType::ABS_THROTTLE => Some("ABS_THROTTLE"),
        AbsoluteAxisType::ABS_RUDDER => Some("ABS_RUDDER"),
        AbsoluteAxisType::ABS_WHEEL => Some("ABS_WHEEL"),
        AbsoluteAxisType::ABS_GAS => Some("ABS_GAS"),
        AbsoluteAxisType::ABS_BRAKE => Some("ABS_BRAKE"),
        AbsoluteAxisType::ABS_HAT0X => Some("ABS_HAT0X"),
        AbsoluteAxisType::ABS_HAT0Y => Some("ABS_HAT0Y"),
        AbsoluteAxisType::ABS_HAT1X => Some("ABS_HAT1X"),
        AbsoluteAxisType::ABS_HAT1Y => Some("ABS_HAT1Y"),
        AbsoluteAxisType::ABS_HAT2X => Some("ABS_HAT2X"),
        AbsoluteAxisType::ABS_HAT2Y => Some("ABS_HAT2Y"),
        AbsoluteAxisType::ABS_HAT3X => Some("ABS_HAT3X"),
        AbsoluteAxisType::ABS_HAT3Y => Some("ABS_HAT3Y"),
        AbsoluteAxisType::ABS_PRESSURE => Some("ABS_PRESSURE"),
        AbsoluteAxisType::ABS_DISTANCE => Some("ABS_DISTANCE"),
        AbsoluteAxisType::ABS_TILT_X => Some("ABS_TILT_X"),
        AbsoluteAxisType::ABS_TILT_Y => Some("ABS_TILT_Y"),
        AbsoluteAxisType::ABS_TOOL_WIDTH => Some("ABS_TOOL_WIDTH"),
        AbsoluteAxisType::ABS_VOLUME => Some("ABS_VOLUME"),
        AbsoluteAxisType::ABS_MISC => Some("ABS_MISC"),
        _ => None,
    }
}

fn find_devices_with_config(verbosity: Verbosity) -> Vec<(String, String, u16, u16)> {
    let input_dir = "/dev/input";
    let entries = fs::read_dir(input_dir).expect("Failed to read /dev/input");

    let mut devices_with_config = Vec::new();

    for entry in entries {
        if let Ok(entry) = entry {
            let path = entry.path();

            if let Ok(metadata) = fs::metadata(&path) {
                if metadata.file_type().is_char_device() {
                    if let Ok(device) = Device::open(&path) {
                        let name = device.name().unwrap_or("Unknown").to_string();
                        let vendor_id = device.input_id().vendor();
                        let product_id = device.input_id().product();

                        // Possible config file paths
                        let config_paths = vec![
                            format!("/usr/share/deckrypt/{}_{}.toml", vendor_id, product_id),
                            format!("/etc/deckrypt/{}_{}.toml", vendor_id, product_id),
                        ];

                        for config_file_path in config_paths {
                            if fs::metadata(&config_file_path).is_ok() {
                                // Now, get buttons and axes from config
                                if let Ok((buttons_in_config, axes_in_config)) =
                                    get_buttons_and_axes_from_config(&config_file_path)
                                {
                                    // Get supported buttons and axes from device
                                    let mut supported_buttons = HashSet::new();
                                    if let Some(keys) = device.supported_keys() {
                                        for key in keys.iter() {
                                            supported_buttons.insert(key);
                                        }
                                    }

                                    let mut supported_axes = HashSet::new();
                                    if let Some(axes) = device.supported_absolute_axes() {
                                        for axis in axes.iter() {
                                            supported_axes.insert(axis.0);
                                        }
                                    }

                                    // Check if all buttons and axes in config are supported by device
                                    let buttons_supported =
                                        buttons_in_config.is_subset(&supported_buttons);
                                    let axes_supported = axes_in_config.is_subset(&supported_axes);

                                    if buttons_supported && axes_supported {
                                        devices_with_config.push((
                                            path.to_string_lossy().to_string(),
                                            name.clone(),
                                            vendor_id,
                                            product_id,
                                        ));
                                        if verbosity >= Verbosity::Verbose {
                                            println!("Found device with config: {:?}", name);
                                        }
                                        break; // Found a suitable config file for this device
                                    } else {
                                        if verbosity >= Verbosity::VeryVerbose {
                                            println!("Device '{}' does not support all buttons/axes defined in config file '{}', skipping.", name, config_file_path);

                                            // Calculate and print unsupported buttons
                                            let unsupported_buttons =
                                                buttons_in_config.difference(&supported_buttons);
                                            for key in unsupported_buttons {
                                                println!("Unsupported button: {:?}", key);
                                            }

                                            // Calculate and print unsupported axes
                                            let unsupported_axes =
                                                axes_in_config.difference(&supported_axes);
                                            for axis_code in unsupported_axes {
                                                if let Some(axis_name) =
                                                    absolute_axis_type_to_name(*axis_code)
                                                {
                                                    println!("Unsupported axis: {}", axis_name);
                                                } else {
                                                    println!(
                                                        "Unsupported axis code: {}",
                                                        axis_code
                                                    );
                                                }
                                            }
                                        }
                                        // Do not add this device
                                    }
                                } else {
                                    // Failed to parse config file
                                    if verbosity >= Verbosity::Verbose {
                                        println!(
                                            "Failed to parse config file '{}', skipping",
                                            config_file_path
                                        );
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    devices_with_config
}

fn list_connected_devices() -> Vec<(String, String, u16, u16)> {
    let input_dir = "/dev/input";
    let entries = fs::read_dir(input_dir).expect("Failed to read /dev/input");

    let mut devices = Vec::new();

    for entry in entries {
        if let Ok(entry) = entry {
            let path = entry.path();

            if let Ok(metadata) = fs::metadata(&path) {
                if metadata.file_type().is_char_device() {
                    if let Ok(device) = Device::open(&path) {
                        let name = device.name().unwrap_or("Unknown").to_string();
                        let vendor_id = device.input_id().vendor();
                        let product_id = device.input_id().product();
                        devices.push((
                            path.to_string_lossy().to_string(),
                            name,
                            vendor_id,
                            product_id,
                        ));
                    }
                }
            }
        }
    }

    devices
}

// Helper function to parse gamepad input from string
fn parse_gamepad_input(input_str: &str) -> Option<GamepadInput> {
    if let Some(key_code) = key_name_to_key(input_str) {
        Some(GamepadInput::Button(key_code))
    } else if input_str.ends_with("_NEG") || input_str.ends_with("_POS") {
        let parts: Vec<&str> = input_str.rsplitn(2, '_').collect();
        if parts.len() == 2 {
            let direction = if parts[0] == "NEG" {
                Direction::Negative
            } else {
                Direction::Positive
            };
            if let Some(axis_type) = axis_name_to_absolute_axis_type(parts[1]) {
                return Some(GamepadInput::Axis(axis_type.0, direction));
            }
        }
        None
    } else if let Some(axis_type) = axis_name_to_absolute_axis_type(input_str) {
        Some(GamepadInput::Axis(axis_type.0, Direction::Positive))
    } else {
        None
    }
}

// New function to load the configuration file
fn load_controller_config(
    vendor_id: u16,
    product_id: u16,
    verbosity: Verbosity,
) -> (
    Vec<(GamepadInput, Mapping)>,
    Vec<(GamepadInput, Mapping)>,
    Vec<(GamepadInput, Mapping)>, // Alternate manual mappings
    Vec<(GamepadInput, Mapping)>, // Alternate axis mappings
    Modifiers,
) {
    let mut manual_mappings = Vec::new();
    let mut axis_mappings = Vec::new();
    let mut alternate_manual_mappings = Vec::new();
    let mut alternate_axis_mappings = Vec::new();
    let mut modifiers = Modifiers::default();

    // Possible config file paths
    let config_paths = vec![
        format!("/usr/share/deckrypt/{}_{}.toml", vendor_id, product_id),
        format!("/etc/deckrypt/{}_{}.toml", vendor_id, product_id),
    ];

    let mut config_loaded = false;

    for config_file_path in config_paths {
        if let Ok(mut file) = File::open(&config_file_path) {
            let mut contents = String::new();
            if let Ok(_) = file.read_to_string(&mut contents) {
                // Parse the TOML configuration
                if let Ok(config) = contents.parse::<TomlValue>() {
                    // Parse button mappings
                    if let Some(buttons) = config.get("buttons").and_then(|v| v.as_table()) {
                        for (key_name, value) in buttons {
                            if let Some(mapping) = parse_mapping(value) {
                                if let Some(gamepad_input) = parse_gamepad_input(key_name) {
                                    manual_mappings.push((gamepad_input, mapping));
                                }
                            }
                        }
                    }

                    // Parse axis mappings
                    if let Some(axes) = config.get("axes").and_then(|v| v.as_table()) {
                        for (axis_name, value) in axes {
                            if let Some(axis_values) = value.as_table() {
                                if let Some(neg_value) = axis_values.get("negative") {
                                    if let Some(mapping) = parse_mapping(neg_value) {
                                        if let Some(gamepad_input) =
                                            parse_gamepad_input(&format!("{}_NEG", axis_name))
                                        {
                                            axis_mappings.push((gamepad_input, mapping));
                                        }
                                    }
                                }
                                if let Some(pos_value) = axis_values.get("positive") {
                                    if let Some(mapping) = parse_mapping(pos_value) {
                                        if let Some(gamepad_input) =
                                            parse_gamepad_input(&format!("{}_POS", axis_name))
                                        {
                                            axis_mappings.push((gamepad_input, mapping));
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // Parse alternate button mappings
                    if let Some(buttons) =
                        config.get("alternate_buttons").and_then(|v| v.as_table())
                    {
                        for (key_name, value) in buttons {
                            if let Some(mapping) = parse_mapping(value) {
                                if let Some(gamepad_input) = parse_gamepad_input(key_name) {
                                    alternate_manual_mappings.push((gamepad_input, mapping));
                                }
                            }
                        }
                    }

                    // Parse alternate axis mappings
                    if let Some(axes) = config.get("alternate_axes").and_then(|v| v.as_table()) {
                        for (axis_name, value) in axes {
                            if let Some(axis_values) = value.as_table() {
                                if let Some(neg_value) = axis_values.get("negative") {
                                    if let Some(mapping) = parse_mapping(neg_value) {
                                        if let Some(gamepad_input) =
                                            parse_gamepad_input(&format!("{}_NEG", axis_name))
                                        {
                                            alternate_axis_mappings.push((gamepad_input, mapping));
                                        }
                                    }
                                }
                                if let Some(pos_value) = axis_values.get("positive") {
                                    if let Some(mapping) = parse_mapping(pos_value) {
                                        if let Some(gamepad_input) =
                                            parse_gamepad_input(&format!("{}_POS", axis_name))
                                        {
                                            alternate_axis_mappings.push((gamepad_input, mapping));
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // Parse modifiers
                    if let Some(mods) = config.get("modifiers").and_then(|v| v.as_table()) {
                        if let Some(shift_key) = mods.get("shift_key").and_then(|v| v.as_str()) {
                            if let Some(mod_input) = parse_gamepad_input(shift_key) {
                                modifiers.shift_modifier = Some(mod_input);
                            }
                        }
                        if let Some(alternate_key) =
                            mods.get("alternate_key").and_then(|v| v.as_str())
                        {
                            if let Some(mod_input) = parse_gamepad_input(alternate_key) {
                                modifiers.alternate_modifier = Some(mod_input);
                            }
                        }
                    }

                    config_loaded = true;
                    if verbosity >= Verbosity::Verbose {
                        println!("Loaded config file from '{}'", config_file_path);
                    }
                    break; // Config loaded successfully, break the loop
                }
            }
        }
    }

    if !config_loaded && verbosity >= Verbosity::Verbose {
        println!(
            "No config file found for controller {}_{}",
            vendor_id, product_id
        );
    }

    (
        manual_mappings,
        axis_mappings,
        alternate_manual_mappings,
        alternate_axis_mappings,
        modifiers,
    )
}

// Helper function to parse mapping from TOML value
fn parse_mapping(value: &TomlValue) -> Option<Mapping> {
    if let Some(s) = value.as_str() {
        // Try to parse as single character
        if s.len() == 1 {
            return Some(Mapping::Character(s.chars().next().unwrap()));
        } else if let Some(key) = key_name_to_key(s) {
            return Some(Mapping::Key(key));
        }
    }
    None
}

// Helper function to convert key name to Key enum
fn key_name_to_key(name: &str) -> Option<Key> {
    match name {
        "ENTER" => Some(Key::KEY_ENTER),
        "ESCAPE" => Some(Key::KEY_ESC),
        "BTN_SOUTH" => Some(Key::BTN_SOUTH),
        "BTN_NORTH" => Some(Key::BTN_NORTH),
        "BTN_WEST" => Some(Key::BTN_WEST),
        "BTN_EAST" => Some(Key::BTN_EAST),
        "BTN_TL" => Some(Key::BTN_TL),
        "BTN_TR" => Some(Key::BTN_TR),
        "BTN_SELECT" => Some(Key::BTN_SELECT),
        "BTN_START" => Some(Key::BTN_START),
        "BTN_MODE" => Some(Key::BTN_MODE),
        "BTN_THUMBL" => Some(Key::BTN_THUMBL),
        "BTN_THUMBR" => Some(Key::BTN_THUMBR),
        // Add other key mappings as needed
        _ => None,
    }
}

// Helper function to convert axis name to AbsoluteAxisType
fn axis_name_to_absolute_axis_type(name: &str) -> Option<AbsoluteAxisType> {
    match name {
        "ABS_X" => Some(AbsoluteAxisType::ABS_X),
        "ABS_Y" => Some(AbsoluteAxisType::ABS_Y),
        "ABS_Z" => Some(AbsoluteAxisType::ABS_Z),
        "ABS_RX" => Some(AbsoluteAxisType::ABS_RX),
        "ABS_RY" => Some(AbsoluteAxisType::ABS_RY),
        "ABS_RZ" => Some(AbsoluteAxisType::ABS_RZ),
        "ABS_HAT0X" => Some(AbsoluteAxisType::ABS_HAT0X),
        "ABS_HAT0Y" => Some(AbsoluteAxisType::ABS_HAT0Y),
        // Add other axis mappings as needed
        _ => None,
    }
}

#[derive(Parser, Debug)]
#[command(
    name = "Deckrypt",
    version = "0.1.0",
    author = "Tobias Görgens <tobi.goergens@gmail.com>",
    about = "Map gamepad inputs to keyboard events",
    after_help = "Source: https://github.com/TobiPeterG/deckrypt",
    help_template = "\
{before-help}{name} {version}
{author-with-newline}{about-with-newline}
{usage-heading} {usage}

{all-args}{after-help}
"
)]
struct Args {
    /// Increases verbosity level (-v, -vv)
    #[arg(short, action = clap::ArgAction::Count)]
    verbosity: u8,

    /// Use unknown devices and prompt for selection
    #[arg(short, long)]
    unknown: bool,

    /// Automatically select the first device if multiple devices are found (not compatible with -u)
    #[arg(short = 'a', long)]
    auto_select: bool,
}

fn main() -> std::io::Result<()> {
    // Use the 'sudo' crate to escalate privileges if needed
    sudo::escalate_if_needed().expect("Failed to escalate privileges");

    let args = Args::parse();

    // Determine verbosity level based on occurrences of -v
    let verbosity = match args.verbosity {
        0 => Verbosity::Quiet,
        1 => Verbosity::Verbose,
        _ => Verbosity::VeryVerbose,
    };

    if args.unknown && args.auto_select {
        eprintln!("--unknown does not support --auto-select!");
        std::process::exit(1);
    }

    // Check if --unknown flag is set
    let use_unknown = args.unknown;

    let (chrmap, _shifted_chars) = match generate_chrmap() {
        Some(maps) => maps,
        None => {
            eprintln!("Failed to generate character map.");
            std::process::exit(1);
        }
    };

    // Collect additional signs accessible without modifiers, excluding control characters
    let additional_signs: Vec<char> = chrmap
        .iter()
        .filter(|&(c, &(_, modifier))| {
            modifier == 0 && !c.is_ascii_alphanumeric() && !c.is_whitespace() && !c.is_control()
            // Exclude control characters
        })
        .map(|(&c, _)| c)
        .collect();

    // Define allowed characters in order for normal mapping
    let normal_chars = ('a'..='z')
        .chain('0'..='9')
        .chain(additional_signs.clone().into_iter());

    // Collect all keyboard keys used in chrmap
    let all_keyboard_keys: Vec<Key> = chrmap.values().map(|&(key, _)| key).collect();

    // Start of main logic
    let vendor_id;
    let product_id;
    let device_path;

    if use_unknown {
        // List connected devices and prompt the user to select one
        let devices = list_connected_devices();
        if devices.is_empty() {
            eprintln!("No input devices found.");
            std::process::exit(1);
        }

        println!("Available devices:");
        for (i, (path, name, vendor_id, product_id)) in devices.iter().enumerate() {
            println!(
                "{}: {} - {} (Vendor ID: {:04x}, Product ID: {:04x})",
                i, path, name, vendor_id, product_id
            );
        }

        print!("Enter the number of the device to use: ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin().read_line(&mut input)?;
        let selection = input.trim().parse::<usize>();

        match selection {
            Ok(num) if num <= devices.len() => {
                let (ref path, ref name, vid, pid) = devices[num];
                device_path = path.clone();
                vendor_id = vid;
                product_id = pid;

                println!(
                    "Selected device: {} - {} (Vendor ID: {:04x}, Product ID: {:04x})",
                    device_path, name, vendor_id, product_id
                );

                println!(
                    "Config file name would be: {}_{}.toml",
                    vendor_id, product_id
                );

                // Open the device
                let device = Device::open(&device_path)?;

                // Print recognized buttons and axes
                if let Some(keys) = device.supported_keys() {
                    println!("Supported buttons:");
                    for key in keys.iter() {
                        println!("{:?}", key);
                    }
                }
                if let Some(axes) = device.supported_absolute_axes() {
                    println!("Supported axes:");
                    for axis in axes.iter() {
                        println!("{:?}", axis);
                    }
                }
            }
            _ => {
                eprintln!("Invalid selection.");
                std::process::exit(1);
            }
        }
    } else {
        let devices = find_devices_with_config(verbosity);
        if devices.is_empty() {
            eprintln!("No devices with config files found.");
            std::process::exit(1);
        } else if devices.len() == 1 {
            let (path, _, vid, pid) = devices.into_iter().next().unwrap();
            device_path = path;
            vendor_id = vid;
            product_id = pid;
        } else {
            // Multiple devices found
            if args.auto_select {
                let (path, name, vid, pid) = devices[0].clone();
                if verbosity >= Verbosity::Verbose {
                    println!(
                        "Automatically selected device: {} (Vendor ID: {:04x}, Product ID: {:04x})",
                        name, vid, pid
                    );
                }
                device_path = path;
                vendor_id = vid;
                product_id = pid;
            } else {
                // Multiple devices found, prompt the user to select one
                println!("Multiple devices with config files found:");
                for (i, (path, name, vid, pid)) in devices.iter().enumerate() {
                    println!(
                        "{}: {} - {} (Vendor ID: {:04x}, Product ID: {:04x})",
                        i, path, name, vid, pid
                    );
                }

                print!("Enter the number of the device to use: ");
                io::stdout().flush().unwrap();

                let mut input = String::new();
                io::stdin().read_line(&mut input)?;
                let selection = input.trim().parse::<usize>();

                match selection {
                    Ok(num) if num <= devices.len() => {
                        let (path, _, vid, pid) = devices[num].clone();
                        device_path = path;
                        vendor_id = vid;
                        product_id = pid;
                    }
                    _ => {
                        eprintln!("Invalid selection.");
                        std::process::exit(1);
                    }
                }
            }
        }
    }

    // Open the selected device
    let mut gamepad_device = Device::open(&device_path)?;

    // Get absolute axis information and store in a HashMap
    let abs_info = gamepad_device.get_abs_state().ok();
    let abs_info_map = abs_info.as_ref().map(|abs_info| {
        let mut map = HashMap::new();
        for (i, info) in abs_info.iter().enumerate() {
            if info.maximum != 0 || info.minimum != 0 {
                map.insert(i as u16, *info);
            }
        }
        map
    });

    // Load controller-specific mappings and modifiers
    let (
        manual_mappings,
        axis_mappings,
        alternate_manual_mappings,
        alternate_axis_mappings,
        mut modifiers,
    ) = load_controller_config(vendor_id, product_id, verbosity);

    let mut virtual_keyboard =
        create_virtual_keyboard(&all_keyboard_keys).expect("Failed to create virtual keyboard");

    let (normal_mapping, _used_chars, alternate_mapping) = get_mappings(
        &gamepad_device,
        &manual_mappings,
        &axis_mappings,
        &alternate_manual_mappings,
        &alternate_axis_mappings,
        normal_chars.clone(),
        &additional_signs,
        abs_info_map.as_ref(),
        &modifiers,
        verbosity,
    );

    let mut axis_states: HashMap<u16, i32> = HashMap::new();
    let mut pressed_inputs: HashMap<GamepadInput, bool> = HashMap::new();
    let mut pressed_axes: HashMap<GamepadInput, bool> = HashMap::new();

    if verbosity >= Verbosity::Verbose {
        println!("Listening for gamepad events...");
    }

    loop {
        for ev in gamepad_device
            .fetch_events()
            .expect("Failed to fetch events")
        {
            match ev.kind() {
                InputEventKind::Key(key) => {
                    let gamepad_input = GamepadInput::Button(key);

                    // Check if the event is for shift_modifier
                    if Some(gamepad_input.clone()) == modifiers.shift_modifier {
                        // Handle shift modifier
                        let shift_value = ev.value();
                        let shift_event =
                            InputEvent::new(EventType::KEY, Key::KEY_LEFTSHIFT.code(), shift_value);
                        virtual_keyboard.emit(&[shift_event])?;
                        continue;
                    }

                    // Check if the event is for alternate_modifier
                    if Some(gamepad_input.clone()) == modifiers.alternate_modifier {
                        modifiers.alternate_active = ev.value() == 1;
                        continue;
                    }

                    let is_pressed = ev.value() == 1;

                    if is_pressed {
                        // Determine the mapping at the time of press
                        let mapping = if modifiers.alternate_active {
                            &alternate_mapping
                        } else {
                            &normal_mapping
                        };

                        if let Some(mapping_value) = mapping.get(&gamepad_input) {
                            match mapping_value {
                                Mapping::Character(character) => {
                                    if let Some(&(keycode, modifier)) = chrmap.get(character) {
                                        // Handle modifiers if necessary
                                        if modifier == 1 {
                                            let shift_event = InputEvent::new(
                                                EventType::KEY,
                                                Key::KEY_LEFTSHIFT.code(),
                                                1,
                                            );
                                            virtual_keyboard.emit(&[shift_event])?;
                                        }

                                        let event =
                                            InputEvent::new(EventType::KEY, keycode.code(), 1);
                                        virtual_keyboard.emit(&[event])?;

                                        // Record the mapping used
                                        pressed_inputs.insert(
                                            gamepad_input.clone(),
                                            modifiers.alternate_active,
                                        );

                                        if verbosity >= Verbosity::Verbose {
                                            println!(
                                                "Button {:?} pressed, sending character '{}'",
                                                key, character
                                            );
                                        }
                                    }
                                }
                                Mapping::Key(keycode) => {
                                    let event = InputEvent::new(EventType::KEY, keycode.code(), 1);
                                    virtual_keyboard.emit(&[event])?;

                                    // Record the mapping used
                                    pressed_inputs
                                        .insert(gamepad_input.clone(), modifiers.alternate_active);

                                    if verbosity >= Verbosity::Verbose {
                                        println!(
                                            "Button {:?} pressed, sending key '{:?}'",
                                            key, keycode
                                        );
                                    }
                                }
                            }
                        }
                    } else {
                        // On release, use the mapping stored at the time of press
                        if let Some(&was_alternate) = pressed_inputs.get(&gamepad_input) {
                            let mapping = if was_alternate {
                                &alternate_mapping
                            } else {
                                &normal_mapping
                            };

                            if let Some(mapping_value) = mapping.get(&gamepad_input) {
                                match mapping_value {
                                    Mapping::Character(character) => {
                                        if let Some(&(keycode, modifier)) = chrmap.get(character) {
                                            // Handle modifiers if necessary
                                            if modifier == 1 {
                                                let shift_event = InputEvent::new(
                                                    EventType::KEY,
                                                    Key::KEY_LEFTSHIFT.code(),
                                                    0,
                                                );
                                                virtual_keyboard.emit(&[shift_event])?;
                                            }

                                            let event =
                                                InputEvent::new(EventType::KEY, keycode.code(), 0);
                                            virtual_keyboard.emit(&[event])?;

                                            if verbosity >= Verbosity::Verbose {
                                                println!(
                                                    "Button {:?} released, releasing character '{}'",
                                                    key, character
                                                );
                                            }
                                        }
                                    }
                                    Mapping::Key(keycode) => {
                                        let event =
                                            InputEvent::new(EventType::KEY, keycode.code(), 0);
                                        virtual_keyboard.emit(&[event])?;

                                        if verbosity >= Verbosity::Verbose {
                                            println!(
                                                "Button {:?} released, releasing key '{:?}'",
                                                key, keycode
                                            );
                                        }
                                    }
                                }
                            }

                            // Remove the input from the pressed_inputs map
                            pressed_inputs.remove(&gamepad_input);
                        }
                    }
                }
                InputEventKind::AbsAxis(axis) => {
                    let axis_value = ev.value();
                    let previous_value = axis_states.get(&axis.0).cloned().unwrap_or(0);

                    // Check for shift_modifier axis
                    let gamepad_input_positive = GamepadInput::Axis(axis.0, Direction::Positive);
                    let gamepad_input_negative = GamepadInput::Axis(axis.0, Direction::Negative);

                    if Some(gamepad_input_positive.clone()) == modifiers.shift_modifier
                        || Some(gamepad_input_negative.clone()) == modifiers.shift_modifier
                    {
                        // Handle shift modifier
                        let shift_value = if axis_value != 0 { 1 } else { 0 };
                        let shift_event =
                            InputEvent::new(EventType::KEY, Key::KEY_LEFTSHIFT.code(), shift_value);
                        virtual_keyboard.emit(&[shift_event])?;
                        continue;
                    }

                    // Check for alternate_modifier axis
                    if Some(gamepad_input_positive.clone()) == modifiers.alternate_modifier
                        || Some(gamepad_input_negative.clone()) == modifiers.alternate_modifier
                    {
                        modifiers.alternate_active = axis_value != 0;
                        continue;
                    }

                    // Rest of your axis handling
                    if let Some(abs_info) = abs_info_map.as_ref().and_then(|map| map.get(&axis.0)) {
                        let (activation_threshold, release_threshold) =
                            get_axis_thresholds(abs_info);

                        // Determine mapping at the time of activation
                        let mapping_at_activation = if modifiers.alternate_active {
                            &alternate_mapping
                        } else {
                            &normal_mapping
                        };

                        // Handle negative direction activation
                        if abs_info.minimum < 0
                            && axis_value <= release_threshold
                            && previous_value > release_threshold
                        {
                            let neg_input = GamepadInput::Axis(axis.0, Direction::Negative);

                            if let Some(mapping_value) = mapping_at_activation.get(&neg_input) {
                                // Handle mapping similarly to button events
                                handle_mapping_activation(
                                    &neg_input,
                                    mapping_value,
                                    &chrmap,
                                    &mut virtual_keyboard,
                                    verbosity,
                                    &mut pressed_axes,
                                    modifiers.alternate_active,
                                )?;
                            }
                        }
                        // Handle positive direction activation
                        else if abs_info.maximum > 0
                            && axis_value >= activation_threshold
                            && previous_value < activation_threshold
                        {
                            let pos_input = GamepadInput::Axis(axis.0, Direction::Positive);

                            if let Some(mapping_value) = mapping_at_activation.get(&pos_input) {
                                // Handle mapping similarly to button events
                                handle_mapping_activation(
                                    &pos_input,
                                    mapping_value,
                                    &chrmap,
                                    &mut virtual_keyboard,
                                    verbosity,
                                    &mut pressed_axes,
                                    modifiers.alternate_active,
                                )?;
                            }
                        }
                        // Handle axis returning to neutral
                        else if axis_value.abs() < activation_threshold
                            && previous_value.abs() >= activation_threshold
                        {
                            // Handle negative direction release
                            let neg_input = GamepadInput::Axis(axis.0, Direction::Negative);
                            if let Some(&was_alternate) = pressed_axes.get(&neg_input) {
                                let mapping = if was_alternate {
                                    &alternate_mapping
                                } else {
                                    &normal_mapping
                                };

                                if let Some(mapping_value) = mapping.get(&neg_input) {
                                    handle_mapping_release(
                                        &neg_input,
                                        mapping_value,
                                        &chrmap,
                                        &mut virtual_keyboard,
                                        verbosity,
                                    )?;
                                }

                                // Remove from pressed_axes
                                pressed_axes.remove(&neg_input);
                            }

                            // Handle positive direction release
                            let pos_input = GamepadInput::Axis(axis.0, Direction::Positive);
                            if let Some(&was_alternate) = pressed_axes.get(&pos_input) {
                                let mapping = if was_alternate {
                                    &alternate_mapping
                                } else {
                                    &normal_mapping
                                };

                                if let Some(mapping_value) = mapping.get(&pos_input) {
                                    handle_mapping_release(
                                        &pos_input,
                                        mapping_value,
                                        &chrmap,
                                        &mut virtual_keyboard,
                                        verbosity,
                                    )?;
                                }

                                // Remove from pressed_axes
                                pressed_axes.remove(&pos_input);
                            }
                        }

                        axis_states.insert(axis.0, axis_value);
                    }
                }
                _ => (),
            }
        }
    }
}

// Helper function to handle mapping activation
fn handle_mapping_activation(
    gamepad_input: &GamepadInput,
    mapping_value: &Mapping,
    chrmap: &HashMap<char, (Key, u8)>,
    virtual_keyboard: &mut evdev::uinput::VirtualDevice,
    verbosity: Verbosity,
    pressed_inputs: &mut HashMap<GamepadInput, bool>,
    alternate_active: bool,
) -> std::io::Result<()> {
    match mapping_value {
        Mapping::Character(character) => {
            if let Some(&(keycode, modifier)) = chrmap.get(character) {
                // Handle modifiers if necessary
                if modifier == 1 {
                    let shift_event = InputEvent::new(EventType::KEY, Key::KEY_LEFTSHIFT.code(), 1);
                    virtual_keyboard.emit(&[shift_event])?;
                }

                let event = InputEvent::new(EventType::KEY, keycode.code(), 1);
                virtual_keyboard.emit(&[event])?;

                // Record the mapping used
                pressed_inputs.insert(gamepad_input.clone(), alternate_active);

                if verbosity >= Verbosity::Verbose {
                    println!(
                        "{:?} activated, sending character '{}'",
                        gamepad_input, character
                    );
                }
            }
        }
        Mapping::Key(keycode) => {
            let event = InputEvent::new(EventType::KEY, keycode.code(), 1);
            virtual_keyboard.emit(&[event])?;

            // Record the mapping used
            pressed_inputs.insert(gamepad_input.clone(), alternate_active);

            if verbosity >= Verbosity::Verbose {
                println!("{:?} activated, sending key '{:?}'", gamepad_input, keycode);
            }
        }
    }
    Ok(())
}

// Helper function to handle mapping release
fn handle_mapping_release(
    gamepad_input: &GamepadInput,
    mapping_value: &Mapping,
    chrmap: &HashMap<char, (Key, u8)>,
    virtual_keyboard: &mut evdev::uinput::VirtualDevice,
    verbosity: Verbosity,
) -> std::io::Result<()> {
    match mapping_value {
        Mapping::Character(character) => {
            if let Some(&(keycode, modifier)) = chrmap.get(character) {
                // Handle modifiers if necessary
                if modifier == 1 {
                    let shift_event = InputEvent::new(EventType::KEY, Key::KEY_LEFTSHIFT.code(), 0);
                    virtual_keyboard.emit(&[shift_event])?;
                }

                let event = InputEvent::new(EventType::KEY, keycode.code(), 0);
                virtual_keyboard.emit(&[event])?;

                if verbosity >= Verbosity::Verbose {
                    println!(
                        "{:?} deactivated, releasing character '{}'",
                        gamepad_input, character
                    );
                }
            }
        }
        Mapping::Key(keycode) => {
            let event = InputEvent::new(EventType::KEY, keycode.code(), 0);
            virtual_keyboard.emit(&[event])?;

            if verbosity >= Verbosity::Verbose {
                println!(
                    "{:?} deactivated, releasing key '{:?}'",
                    gamepad_input, keycode
                );
            }
        }
    }
    Ok(())
}
