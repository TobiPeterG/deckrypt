use evdev::Key;
use std::{
    collections::{HashMap, HashSet},
    fmt,
};

/// Defines how verbose the logs should be.  
/// - `Quiet`: No logs except errors  
/// - `Verbose`: Show informational logs  
/// - `VeryVerbose`: Show detailed logs and debug information
#[derive(PartialEq, PartialOrd, Clone, Copy)]
pub enum Verbosity {
    Quiet,
    Verbose,
    VeryVerbose,
}

/// Represents the direction for an axis input (e.g., negative or positive).
#[derive(Hash, Eq, PartialEq, Debug, Clone)]
pub enum Direction {
    Positive,
    Negative,
}

/// Represents a gamepad input, which can be either a button (represented by an evdev `Key`)  
/// or an axis code (`u16`) and direction.  
/// Examples:
/// - `GamepadInput::Button(Key::KEY_ENTER)`  
/// - `GamepadInput::Axis(ABS_X, Direction::Negative)`
#[derive(Hash, Eq, PartialEq, Clone)]
pub enum GamepadInput {
    Button(Key),
    /// Axis code (u16) plus a direction (positive or negative)
    Axis(u16, Direction),
}

/// Represents a single mapping from a gamepad input to either a single character or a special key.  
/// For instance, mapping `ABS_X_NEG` to `'a'` or `BTN_SOUTH` to `Key::KEY_ENTER`.
#[derive(Clone, Debug)]
pub enum Mapping {
    Character(char),
    Key(Key),
}

/// Holds optional modifiers (shift, alternate) which are triggered by specific gamepad inputs.  
/// - `shift_modifier`: The input that toggles SHIFT  
/// - `alternate_modifier`: The input that toggles an alternate mapping  
/// - `alternate_active` and `shift_active`: State flags that indicate whether the modifier is currently pressed.
#[derive(Default, Debug, Clone)]
pub struct Modifiers {
    pub shift_modifier: Option<GamepadInput>,
    pub alternate_modifier: Option<GamepadInput>,
    pub alternate_active: bool,
    pub shift_active: bool,
}

/// Represents additional info for an axis currently pressed.  
/// - `keycode`: The associated key in the virtual keyboard  
/// - `level`: The modifier level (0 = none, 1 = shift, 2 = alt, etc.)
#[derive(Clone, Debug)]
pub struct PressedAxisInfo {
    pub keycode: Key,
    pub level: u8,
}

/// Used to track what's currently pressed.  
/// This variant lets us handle either a character that might require SHIFT/ALT or a direct key press.
#[derive(Clone, Debug)]
pub enum PressedMapping {
    Character { keycode: Key, level: u8 },
    Key(Key),
}

/// Determines verbosity level based on a count (e.g., how many times `-v` was specified).
/// Returns `Verbosity::Quiet` for 0, `Verbosity::Verbose` for 1, and `Verbosity::VeryVerbose` otherwise.
pub fn determine_verbosity(count: u8) -> Verbosity {
    match count {
        0 => Verbosity::Quiet,
        1 => Verbosity::Verbose,
        _ => Verbosity::VeryVerbose,
    }
}

/// Custom Debug implementation to print more concise axis info, e.g. `Axis(ABS_X, Negative)` or `Button(KEY_ENTER)`.
impl fmt::Debug for GamepadInput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GamepadInput::Button(key) => {
                write!(f, "Button({:?})", key)
            }
            GamepadInput::Axis(axis_code, direction) => {
                write!(f, "Axis({}, {:?})", axis_code, direction)
            }
        }
    }
}

/// The overall controller config that is parsed from a TOML file.  
/// It includes sets of required buttons/axes, manual mappings, axis mappings, alternate mappings, etc.
#[derive(Clone, Debug)]
pub struct ControllerConfig {
    pub required_buttons: HashSet<Key>,
    pub required_axes: HashSet<u16>,

    pub manual_mappings: Vec<(GamepadInput, Mapping)>,
    pub axis_mappings: Vec<(GamepadInput, Mapping)>,
    pub alternate_manual_mappings: Vec<(GamepadInput, Mapping)>,
    pub alternate_axis_mappings: Vec<(GamepadInput, Mapping)>,
    pub modifiers: Modifiers,
}

/// A fully built set of normal & alternate mappings, along with a special Enter input if one is discovered.  
/// The normal vs. alternate mapping depends on whether the `alternate_modifier` is active.  
/// - `special_enter_input`: If present, that input is treated as an Enter key (or Backspace/Escape if shift/alt is pressed).
pub struct BuiltMappings {
    pub normal_mapping: HashMap<GamepadInput, Mapping>,
    pub alternate_mapping: HashMap<GamepadInput, Mapping>,
    pub special_enter_input: Option<GamepadInput>,
}

/// Represents a single device that has a known config file path, but we have not yet parsed the config file.  
/// This allows us to do a lazy load of the config if the user selects this device.
#[derive(Clone)]
pub struct DeviceWithConfig {
    pub path: String,
    pub name: String,
    pub vendor_id: u16,
    pub product_id: u16,
    pub config: ControllerConfig,
}

/// Represents a "known" device that we've discovered has a config file, but the config hasn't been parsed yet.
/// We only store the path to the config file here.
#[derive(Clone)]
pub struct KnownDeviceUnparsed {
    pub path: String,
    pub name: String,
    pub vendor_id: u16,
    pub product_id: u16,
    // We do *not* store the full parsed config here.
    // We only store the *file path* so we can parse it later if needed.
    pub config_file_path: Option<String>,
}

/// Represents an unknown device (no config found).  
/// The user can choose to map it interactively, but this is not fully implemented in the example.
#[derive(Clone)]
pub struct UnknownDevice {
    pub path: String,
    pub name: String,
    pub vendor_id: u16,
    pub product_id: u16,
}

/// Used to differentiate between a known device with a (potential) config or an unknown device.  
/// This is returned from `attempt_device_selection()`.
pub enum SelectedDevice {
    Known(KnownDeviceUnparsed),
    Unknown(UnknownDevice),
}
