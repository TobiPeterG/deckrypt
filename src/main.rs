use evdev::{
    uinput::VirtualDeviceBuilder, AbsoluteAxisType, AttributeSet, Device, EventType, InputEvent,
    InputEventKind, Key,
};
use libc::{c_int, getuid, input_absinfo};
use std::collections::{HashMap, HashSet};
use std::fs::{self, File};
use std::io::Read;
use std::os::unix::fs::FileTypeExt;
use std::os::unix::io::AsRawFd;
use std::env;

// Include the toml crate in your Cargo.toml dependencies
use toml::Value as TomlValue;

// Include the sudo crate
use sudo;

const AXIS_THRESHOLD_PERCENTAGE: f32 = 0.5;
const KEY_MAX: u16 = 255;

#[derive(Hash, Eq, PartialEq, Debug, Clone)]
enum Direction {
    Positive,
    Negative,
}

#[derive(Hash, Eq, PartialEq, Debug, Clone)]
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

// Function to map gamepad events to characters (both buttons and axes)
fn get_char_mappings(
    device: &Device,
    manual_mappings: &[(GamepadInput, char)],
    axis_mappings: &[(GamepadInput, char)],
    alternate_manual_mappings: &[(GamepadInput, char)],
    alternate_axis_mappings: &[(GamepadInput, char)],
    normal_chars_iter: impl Iterator<Item = char> + Clone,
    additional_signs: &[char],
    abs_info_map: Option<&HashMap<u16, input_absinfo>>,
    modifiers: &Modifiers,
    verbosity: Verbosity,
) -> (
    HashMap<GamepadInput, char>,
    HashSet<char>,
    HashMap<GamepadInput, char>,
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
    for &(ref gamepad_input, character) in manual_mappings {
        normal_mapping.insert(gamepad_input.clone(), character);
        used_chars.insert(character);
    }

    // Map axis_mappings for normal mapping
    for &(ref gamepad_input, character) in axis_mappings {
        normal_mapping.insert(gamepad_input.clone(), character);
        used_chars.insert(character);
    }

    // Map alternate_manual_mappings for alternate mapping
    for &(ref gamepad_input, character) in alternate_manual_mappings {
        alternate_mapping.insert(gamepad_input.clone(), character);
        used_chars.insert(character);
    }

    // Map alternate_axis_mappings for alternate mapping
    for &(ref gamepad_input, character) in alternate_axis_mappings {
        alternate_mapping.insert(gamepad_input.clone(), character);
        used_chars.insert(character);
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
            normal_mapping.insert(gamepad_input.clone(), next_char);
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

    let normal_numbers: Vec<char> = ('0'..='9')
        .filter(|c| !used_chars.contains(c))
        .collect();

    let mut numbers_iter = normal_numbers.into_iter();

    for gamepad_input in &remaining_gamepad_inputs {
        if let Some(next_char) = numbers_iter.next() {
            normal_mapping.insert(gamepad_input.clone(), next_char);
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
            normal_mapping.insert(gamepad_input.clone(), next_char);
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
    used_chars.extend(normal_mapping.values());
    used_chars.extend(alternate_mapping.values());

    // For gamepad inputs that have a character assigned in normal mapping but not in alternate mapping,
    // assign the opposite character in alternate mapping, if not already used
    for gamepad_input in &unmapped_gamepad_inputs {
        if let Some(&normal_char) = normal_mapping.get(gamepad_input) {
            let mut assigned_char = None;

            if normal_char.is_ascii_lowercase() || normal_char.is_ascii_uppercase() {
                let opposite_char = get_reversed_char(normal_char);
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
                alternate_mapping.insert(gamepad_input.clone(), c);
                used_chars.insert(c);
                if verbosity >= Verbosity::VeryVerbose {
                    println!(
                        "Automatically assigned alternate mapping for {:?} to '{}'",
                        gamepad_input, c
                    );
                }
            }
        }
    }

    (normal_mapping, used_chars, alternate_mapping)
}

fn find_first_device_with_btn_south() -> Option<Device> {
    let input_dir = "/dev/input";
    let entries = fs::read_dir(input_dir).ok()?;

    for entry in entries {
        let entry = entry.ok()?;
        let path = entry.path();

        if let Ok(metadata) = fs::metadata(&path) {
            if metadata.file_type().is_char_device() {
                if let Ok(device) = Device::open(&path) {
                    if device
                        .supported_keys()
                        .map_or(false, |keys| keys.contains(evdev::Key::BTN_SOUTH))
                    {
                        return Some(device);
                    }
                }
            }
        }
    }

    None
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
    device: &Device,
    verbosity: Verbosity,
) -> (
    Vec<(GamepadInput, char)>,
    Vec<(GamepadInput, char)>,
    Vec<(GamepadInput, char)>, // Alternate manual mappings
    Vec<(GamepadInput, char)>, // Alternate axis mappings
    Modifiers,
) {
    let mut manual_mappings = Vec::new();
    let mut axis_mappings = Vec::new();
    let mut alternate_manual_mappings = Vec::new();
    let mut alternate_axis_mappings = Vec::new();
    let mut modifiers = Modifiers::default();

    // Identify the controller
    let vendor_id = device.input_id().vendor();
    let product_id = device.input_id().product();

    // Possible config file paths
    let config_paths = vec![
        format!(
            "/usr/share/deckrypt/{}_{}.toml",
            vendor_id, product_id
        ),
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
                            if let Some(character) = value.as_str() {
                                if let Some(gamepad_input) = parse_gamepad_input(key_name) {
                                    if let Some(c) = character.chars().next() {
                                        manual_mappings.push((gamepad_input, c));
                                    }
                                }
                            }
                        }
                    }

                    // Parse axis mappings
                    if let Some(axes) = config.get("axes").and_then(|v| v.as_table()) {
                        for (axis_name, value) in axes {
                            if let Some(axis_values) = value.as_table() {
                                let negative_char = axis_values
                                    .get("negative")
                                    .and_then(|v| v.as_str())
                                    .and_then(|s| s.chars().next());
                                let positive_char = axis_values
                                    .get("positive")
                                    .and_then(|v| v.as_str())
                                    .and_then(|s| s.chars().next());

                                if let Some(neg_char) = negative_char {
                                    if let Some(gamepad_input) =
                                        parse_gamepad_input(&format!("{}_NEG", axis_name))
                                    {
                                        axis_mappings.push((gamepad_input, neg_char));
                                    }
                                }

                                if let Some(pos_char) = positive_char {
                                    if let Some(gamepad_input) =
                                        parse_gamepad_input(&format!("{}_POS", axis_name))
                                    {
                                        axis_mappings.push((gamepad_input, pos_char));
                                    }
                                }
                            }
                        }
                    }

                    // Parse alternate button mappings
                    if let Some(buttons) = config
                        .get("alternate_buttons")
                        .and_then(|v| v.as_table())
                    {
                        for (key_name, value) in buttons {
                            if let Some(character) = value.as_str() {
                                if let Some(gamepad_input) = parse_gamepad_input(key_name) {
                                    if let Some(c) = character.chars().next() {
                                        alternate_manual_mappings.push((gamepad_input, c));
                                    }
                                }
                            }
                        }
                    }

                    // Parse alternate axis mappings
                    if let Some(axes) = config
                        .get("alternate_axes")
                        .and_then(|v| v.as_table())
                    {
                        for (axis_name, value) in axes {
                            if let Some(axis_values) = value.as_table() {
                                let negative_char = axis_values
                                    .get("negative")
                                    .and_then(|v| v.as_str())
                                    .and_then(|s| s.chars().next());
                                let positive_char = axis_values
                                    .get("positive")
                                    .and_then(|v| v.as_str())
                                    .and_then(|s| s.chars().next());

                                if let Some(neg_char) = negative_char {
                                    if let Some(gamepad_input) =
                                        parse_gamepad_input(&format!("{}_NEG", axis_name))
                                    {
                                        alternate_axis_mappings.push((gamepad_input, neg_char));
                                    }
                                }

                                if let Some(pos_char) = positive_char {
                                    if let Some(gamepad_input) =
                                        parse_gamepad_input(&format!("{}_POS", axis_name))
                                    {
                                        alternate_axis_mappings.push((gamepad_input, pos_char));
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
                        if let Some(alternate_key) = mods
                            .get("alternate_key")
                            .and_then(|v| v.as_str())
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

// Helper function to convert key name to Key enum
fn key_name_to_key(name: &str) -> Option<Key> {
    match name {
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

fn main() -> std::io::Result<()> {
    // Use the 'sudo' crate to escalate privileges if needed
    sudo::escalate_if_needed().expect("Failed to escalate privileges");

    // Determine verbosity level based on command-line arguments
    let verbosity = if env::args().any(|arg| arg == "-vv") {
        Verbosity::VeryVerbose
    } else if env::args().any(|arg| arg == "-v") {
        Verbosity::Verbose
    } else {
        Verbosity::Quiet
    };

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
            modifier == 0
                && !c.is_ascii_alphanumeric()
                && !c.is_whitespace()
                && !c.is_control() // Exclude control characters
        })
        .map(|(&c, _)| c)
        .collect();

    // Define allowed characters in order for normal mapping
    let normal_chars = ('a'..='z')
        .chain('0'..='9')
        .chain(additional_signs.clone().into_iter());

    // Collect all keyboard keys used in chrmap
    let all_keyboard_keys: Vec<Key> = chrmap.values().map(|&(key, _)| key).collect();

    loop {
        if let Some(mut gamepad_device) = find_first_device_with_btn_south() {
            if verbosity >= Verbosity::Verbose {
                println!("Listening for gamepad events...");
            }

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
            ) = load_controller_config(&gamepad_device, verbosity);

            let mut virtual_keyboard = create_virtual_keyboard(&all_keyboard_keys)
                .expect("Failed to create virtual keyboard");

            let (normal_mapping, _used_chars, alternate_mapping) = get_char_mappings(
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

            loop {
                for ev in gamepad_device.fetch_events().expect("Failed to fetch events") {
                    match ev.kind() {
                        InputEventKind::Key(key) => {
                            let gamepad_input = GamepadInput::Button(key);

                            // Check if the event is for shift_modifier
                            if Some(gamepad_input.clone()) == modifiers.shift_modifier {
                                // Handle shift modifier
                                let shift_value = ev.value();
                                let shift_event = InputEvent::new(
                                    EventType::KEY,
                                    Key::KEY_LEFTSHIFT.code(),
                                    shift_value,
                                );
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

                                if let Some(&character) = mapping.get(&gamepad_input) {
                                    if let Some(&(keycode, modifier)) = chrmap.get(&character) {
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
                                        pressed_inputs
                                            .insert(gamepad_input.clone(), modifiers.alternate_active);

                                        if verbosity >= Verbosity::VeryVerbose {
                                            println!(
                                                "Button {:?} activated, sending character '{}'",
                                                key, character
                                            );
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

                                    if let Some(&character) = mapping.get(&gamepad_input) {
                                        if let Some(&(keycode, modifier)) = chrmap.get(&character) {
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

                                            if verbosity >= Verbosity::VeryVerbose {
                                                println!(
                                                    "Button {:?} deactivated, releasing character '{}'",
                                                    key, character
                                                );
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
                            let gamepad_input_positive =
                                GamepadInput::Axis(axis.0, Direction::Positive);
                            let gamepad_input_negative =
                                GamepadInput::Axis(axis.0, Direction::Negative);

                            if Some(gamepad_input_positive.clone()) == modifiers.shift_modifier
                                || Some(gamepad_input_negative.clone())
                                    == modifiers.shift_modifier
                            {
                                // Handle shift modifier
                                let shift_value = if axis_value != 0 { 1 } else { 0 };
                                let shift_event = InputEvent::new(
                                    EventType::KEY,
                                    Key::KEY_LEFTSHIFT.code(),
                                    shift_value,
                                );
                                virtual_keyboard.emit(&[shift_event])?;
                                continue;
                            }

                            // Check for alternate_modifier axis
                            if Some(gamepad_input_positive.clone())
                                == modifiers.alternate_modifier
                                || Some(gamepad_input_negative.clone())
                                    == modifiers.alternate_modifier
                            {
                                modifiers.alternate_active = axis_value != 0;
                                continue;
                            }

                            // Rest of your axis handling
                            if let Some(abs_info) =
                                abs_info_map.as_ref().and_then(|map| map.get(&axis.0))
                            {
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

                                    if let Some(&character) = mapping_at_activation.get(&neg_input) {
                                        if let Some(&(keycode, modifier)) = chrmap.get(&character) {
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
                                            pressed_axes
                                                .insert(neg_input.clone(), modifiers.alternate_active);

                                            if verbosity >= Verbosity::VeryVerbose {
                                                println!(
                                                    "Axis {:?} negative activated, sending character '{}'",
                                                    axis, character
                                                );
                                            }
                                        }
                                    }
                                }
                                // Handle positive direction activation
                                else if abs_info.maximum > 0
                                    && axis_value >= activation_threshold
                                    && previous_value < activation_threshold
                                {
                                    let pos_input = GamepadInput::Axis(axis.0, Direction::Positive);

                                    if let Some(&character) = mapping_at_activation.get(&pos_input) {
                                        if let Some(&(keycode, modifier)) = chrmap.get(&character) {
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
                                            pressed_axes
                                                .insert(pos_input.clone(), modifiers.alternate_active);

                                            if verbosity >= Verbosity::VeryVerbose {
                                                println!(
                                                    "Axis {:?} positive activated, sending character '{}'",
                                                    axis, character
                                                );
                                            }
                                        }
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

                                        if let Some(&character) = mapping.get(&neg_input) {
                                            if let Some(&(keycode, modifier)) = chrmap.get(&character) {
                                                // Handle modifiers if necessary
                                                if modifier == 1 {
                                                    let shift_event = InputEvent::new(
                                                        EventType::KEY,
                                                        Key::KEY_LEFTSHIFT.code(),
                                                        0,
                                                    );
                                                    virtual_keyboard.emit(&[shift_event])?;
                                                }

                                                let event = InputEvent::new(
                                                    EventType::KEY,
                                                    keycode.code(),
                                                    0,
                                                );
                                                virtual_keyboard.emit(&[event])?;

                                                if verbosity >= Verbosity::VeryVerbose {
                                                    println!(
                                                        "Axis {:?} negative deactivated, releasing character '{}'",
                                                        axis, character
                                                    );
                                                }
                                            }
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

                                        if let Some(&character) = mapping.get(&pos_input) {
                                            if let Some(&(keycode, modifier)) = chrmap.get(&character) {
                                                // Handle modifiers if necessary
                                                if modifier == 1 {
                                                    let shift_event = InputEvent::new(
                                                        EventType::KEY,
                                                        Key::KEY_LEFTSHIFT.code(),
                                                        0,
                                                    );
                                                    virtual_keyboard.emit(&[shift_event])?;
                                                }

                                                let event = InputEvent::new(
                                                    EventType::KEY,
                                                    keycode.code(),
                                                    0,
                                                );
                                                virtual_keyboard.emit(&[event])?;

                                                if verbosity >= Verbosity::VeryVerbose {
                                                    println!(
                                                        "Axis {:?} positive deactivated, releasing character '{}'",
                                                        axis, character
                                                    );
                                                }
                                            }
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
        } else {
            if verbosity >= Verbosity::Verbose {
                println!("No device with BTN_SOUTH found. Retrying in 1 second...");
            }
            std::thread::sleep(std::time::Duration::from_secs(1));
            continue;
        }
    }
}
