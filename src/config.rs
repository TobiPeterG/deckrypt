use crate::keymap::ALLOWED_CHARACTERS;
use crate::types::{ConfigDevice, ControllerConfig};
use std::collections::HashSet;
use std::io::Read;
use std::{collections::HashMap, fs::File};

use evdev::{AbsoluteAxisType, Key};
use log::{debug, trace, warn};
use toml::Value as TomlValue;

use crate::types::{Direction, GamepadInput, Mapping, Modifiers};

/// Parse a `Mapping` from a TOML value.
pub fn parse_mapping(value: &TomlValue) -> Option<Mapping> {
    if let Some(s) = value.as_str() {
        // Single character or known special key
        if s.len() == 1 {
            let c = s.chars().next().unwrap_or_default();
            if ALLOWED_CHARACTERS.contains(&c) {
                return Some(Mapping::Character(c));
            } else {
                warn!("Character '{}' is not allowed and will be ignored.", c);
                return None;
            }
        } else if let Some(key) = key_name_to_key(s) {
            return Some(Mapping::Key(key));
        }
    }
    None
}

/// Convert a string like "ENTER" -> Key::KEY_ENTER, etc.
pub fn key_name_to_key(name: &str) -> Option<Key> {
    match name {
        "BTN_SOUTH" => Some(Key::BTN_SOUTH),
        "BTN_NORTH" => Some(Key::BTN_NORTH),
        "BTN_WEST" => Some(Key::BTN_WEST),
        "BTN_EAST" => Some(Key::BTN_EAST),
        "BTN_TL" => Some(Key::BTN_TL),
        "BTN_TR" => Some(Key::BTN_TR),
        "BTN_TL2" => Some(Key::BTN_TL2),
        "BTN_TR2" => Some(Key::BTN_TR2),
        "BTN_SELECT" => Some(Key::BTN_SELECT),
        "BTN_START" => Some(Key::BTN_START),
        "BTN_MODE" => Some(Key::BTN_MODE),
        "BTN_BASE" => Some(Key::BTN_BASE),
        "BTN_THUMBL" => Some(Key::BTN_THUMBL),
        "BTN_THUMBR" => Some(Key::BTN_THUMBR),
        "BTN_THUMB" => Some(Key::BTN_THUMB),
        "BTN_THUMB2" => Some(Key::BTN_THUMB2),
        "BTN_DPAD_UP" => Some(Key::BTN_DPAD_UP),
        "BTN_DPAD_DOWN" => Some(Key::BTN_DPAD_DOWN),
        "BTN_DPAD_LEFT" => Some(Key::BTN_DPAD_LEFT),
        "BTN_DPAD_RIGHT" => Some(Key::BTN_DPAD_RIGHT),
        "BTN_TRIGGER_HAPPY1" => Some(Key::BTN_TRIGGER_HAPPY1),
        "BTN_TRIGGER_HAPPY2" => Some(Key::BTN_TRIGGER_HAPPY2),
        "BTN_TRIGGER_HAPPY3" => Some(Key::BTN_TRIGGER_HAPPY3),
        "BTN_TRIGGER_HAPPY4" => Some(Key::BTN_TRIGGER_HAPPY4),
        // Add other key mappings as needed
        _ => None,
    }
}

pub fn axis_name_to_absolute_axis_type(name: &str) -> Option<AbsoluteAxisType> {
    match name {
        "ABS_X" => Some(AbsoluteAxisType::ABS_X),
        "ABS_Y" => Some(AbsoluteAxisType::ABS_Y),
        "ABS_Z" => Some(AbsoluteAxisType::ABS_Z),
        "ABS_RX" => Some(AbsoluteAxisType::ABS_RX),
        "ABS_RY" => Some(AbsoluteAxisType::ABS_RY),
        "ABS_RZ" => Some(AbsoluteAxisType::ABS_RZ),
        "ABS_HAT0X" => Some(AbsoluteAxisType::ABS_HAT0X),
        "ABS_HAT0Y" => Some(AbsoluteAxisType::ABS_HAT0Y),
        "ABS_HAT1X" => Some(AbsoluteAxisType::ABS_HAT1X),
        "ABS_HAT1Y" => Some(AbsoluteAxisType::ABS_HAT1Y),
        "ABS_HAT2X" => Some(AbsoluteAxisType::ABS_HAT2X),
        "ABS_HAT2Y" => Some(AbsoluteAxisType::ABS_HAT2Y),
        "ABS_THROTTLE" => Some(AbsoluteAxisType::ABS_THROTTLE),
        "ABS_RUDDER" => Some(AbsoluteAxisType::ABS_RUDDER),
        "ABS_WHEEL" => Some(AbsoluteAxisType::ABS_WHEEL),
        // Add other axis mappings as needed
        _ => None,
    }
}

/// Helper to parse gamepad input from string (e.g. "ABS_X_NEG" -> Axis(..., Negative))
pub fn parse_gamepad_input(input_str: &str) -> Option<GamepadInput> {
    if let Some(key_code) = key_name_to_key(input_str) {
        Some(GamepadInput::Button(key_code))
    } else if input_str.ends_with("_NEG") || input_str.ends_with("_POS") {
        // e.g. ABS_X_NEG
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
        // e.g. ABS_X only
        Some(GamepadInput::Axis(axis_type.0, Direction::Positive))
    } else {
        None
    }
}

pub fn parse_controller_config(
    vendor_id: u16,
    product_id: u16,
    config_file_path: String,
) -> Option<ControllerConfig> {
    if let Ok(mut file) = File::open(&config_file_path) {
        let mut contents = String::new();
        if let Ok(_) = file.read_to_string(&mut contents) {
            if let Ok(toml_val) = contents.parse::<TomlValue>() {
                // We'll build up the final ControllerConfig
                let mut ctrl_cfg = ControllerConfig {
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

                // parse "buttons"
                if let Some(buttons) = toml_val.get("buttons").and_then(|v| v.as_table()) {
                    for (key_name, value) in buttons {
                        if let Some(g_input) = parse_gamepad_input(key_name) {
                            // add to "required_buttons" or "required_axes"
                            match g_input {
                                GamepadInput::Button(k) => {
                                    ctrl_cfg.required_buttons.insert(k);
                                }
                                GamepadInput::Axis(ax, _) => {
                                    ctrl_cfg.required_axes.insert(ax);
                                }
                            }
                            // also parse the mapping if present
                            if let Some(mapping) = parse_mapping(value) {
                                ctrl_cfg.button_mappings.push((g_input, mapping));
                            } else {
                                warn!("Mapping value for button '{}' unknown or empty.", key_name);
                                ctrl_cfg.button_mappings.push((g_input, Mapping::None));
                            }
                        }
                    }
                }

                // parse "axes"
                if let Some(axes) = toml_val.get("axes").and_then(|v| v.as_table()) {
                    for (axis_key, value) in axes {
                        // Check if the key ends with "_NEG" or "_POS"
                        if axis_key.ends_with("_NEG") || axis_key.ends_with("_POS") {
                            // Split the key into axis name and direction
                            let parts: Vec<&str> = axis_key.rsplitn(2, '_').collect();
                            if parts.len() == 2 {
                                let direction_str = parts[0];
                                let axis_name = parts[1];
                                let direction = match direction_str {
                                    "NEG" => Direction::Negative,
                                    "POS" => Direction::Positive,
                                    _ => {
                                        warn!(
                                            "Invalid direction '{}' in axis key '{}'. Skipping.",
                                            direction_str, axis_key
                                        );
                                        continue;
                                    }
                                };

                                // Convert axis_name to AbsoluteAxisType
                                if let Some(axis_type) = axis_name_to_absolute_axis_type(axis_name)
                                {
                                    let g_input = GamepadInput::Axis(axis_type.0, direction);

                                    // Add to required sets
                                    match g_input {
                                        GamepadInput::Button(k) => {
                                            ctrl_cfg.required_buttons.insert(k);
                                        }
                                        GamepadInput::Axis(ax, _) => {
                                            ctrl_cfg.required_axes.insert(ax);
                                        }
                                    }

                                    // Parse the mapping value
                                    if let Some(mapping) = parse_mapping(value) {
                                        ctrl_cfg.axis_mappings.push((g_input, mapping));
                                    } else {
                                        warn!(
                                            "Mapping value for axis '{}' unknown or empty.",
                                            axis_key
                                        );
                                        ctrl_cfg.axis_mappings.push((g_input, Mapping::None));
                                    }
                                } else {
                                    warn!(
                                        "Unknown axis name '{}' in axis key '{}'. Skipping.",
                                        axis_name, axis_key
                                    );
                                }
                            }
                        } else {
                            warn!(
                                "Axis key '{}' does not specify direction. Skipping.",
                                axis_key
                            );
                        }
                    }
                }

                // parse "alternate_buttons"
                if let Some(buttons) = toml_val.get("alternate_buttons").and_then(|v| v.as_table())
                {
                    for (key_name, value) in buttons {
                        if let Some(g_input) = parse_gamepad_input(key_name) {
                            match g_input {
                                GamepadInput::Button(k) => {
                                    ctrl_cfg.required_buttons.insert(k);
                                }
                                GamepadInput::Axis(ax, _) => {
                                    ctrl_cfg.required_axes.insert(ax);
                                }
                            }
                            if let Some(mapping) = parse_mapping(value) {
                                ctrl_cfg.alternate_button_mappings.push((g_input, mapping));
                            } else {
                                warn!(
                                    "Alternate mapping value for button '{}' unknown or empty.",
                                    key_name
                                );
                                ctrl_cfg
                                    .alternate_button_mappings
                                    .push((g_input, Mapping::None));
                            }
                        }
                    }
                }

                // parse "alternate_axes"
                if let Some(axes) = toml_val.get("alternate_axes").and_then(|v| v.as_table()) {
                    for (axis_key, value) in axes {
                        if axis_key.ends_with("_NEG") || axis_key.ends_with("_POS") {
                            let parts: Vec<&str> = axis_key.rsplitn(2, '_').collect();
                            if parts.len() == 2 {
                                let direction_str = parts[0];
                                let axis_name = parts[1];
                                let direction = match direction_str {
                                    "NEG" => Direction::Negative,
                                    "POS" => Direction::Positive,
                                    _ => {
                                        warn!(
                                            "Invalid direction '{}' in alternate axis key '{}'. Skipping.",
                                            direction_str, axis_key
                                        );
                                        continue;
                                    }
                                };

                                if let Some(axis_type) = axis_name_to_absolute_axis_type(axis_name)
                                {
                                    let g_input = GamepadInput::Axis(axis_type.0, direction);

                                    match g_input {
                                        GamepadInput::Button(k) => {
                                            ctrl_cfg.required_buttons.insert(k);
                                        }
                                        GamepadInput::Axis(ax, _) => {
                                            ctrl_cfg.required_axes.insert(ax);
                                        }
                                    }

                                    if let Some(mapping) = parse_mapping(value) {
                                        ctrl_cfg.alternate_axis_mappings.push((g_input, mapping));
                                    } else {
                                        warn!("Alternate mapping value for axis '{}' unknown or empty.", axis_key);
                                        ctrl_cfg
                                            .alternate_axis_mappings
                                            .push((g_input, Mapping::None));
                                    }
                                } else {
                                    warn!(
                                        "Unknown axis name '{}' in alternate axis key '{}'. Skipping.",
                                        axis_name, axis_key
                                    );
                                }
                            }
                        } else {
                            warn!(
                                "Alternate axis key '{}' does not specify direction. Skipping.",
                                axis_key
                            );
                        }
                    }
                }

                // parse "modifiers"
                if let Some(mods) = toml_val.get("modifiers").and_then(|v| v.as_table()) {
                    if let Some(shift_key) = mods.get("shift_key").and_then(|v| v.as_str()) {
                        if let Some(mod_input) = parse_gamepad_input(shift_key) {
                            // also add it to required sets
                            match mod_input {
                                GamepadInput::Button(k) => {
                                    ctrl_cfg.required_buttons.insert(k);
                                }
                                GamepadInput::Axis(ax, _) => {
                                    ctrl_cfg.required_axes.insert(ax);
                                }
                            }
                            ctrl_cfg.modifiers.shift_modifier = Some(mod_input);
                        }
                    }
                    if let Some(alternate_key) = mods.get("alternate_key").and_then(|v| v.as_str())
                    {
                        if let Some(mod_input) = parse_gamepad_input(alternate_key) {
                            match mod_input {
                                GamepadInput::Button(k) => {
                                    ctrl_cfg.required_buttons.insert(k);
                                }
                                GamepadInput::Axis(ax, _) => {
                                    ctrl_cfg.required_axes.insert(ax);
                                }
                            }
                            ctrl_cfg.modifiers.alternate_modifier = Some(mod_input);
                        }
                    }

                    if let Some(enter_key) = mods.get("enter_key").and_then(|v| v.as_str()) {
                        if let Some(mod_input) = parse_gamepad_input(enter_key) {
                            match mod_input {
                                GamepadInput::Button(k) => {
                                    ctrl_cfg.required_buttons.insert(k);
                                }
                                GamepadInput::Axis(ax, _) => {
                                    ctrl_cfg.required_axes.insert(ax);
                                }
                            }
                            ctrl_cfg.modifiers.enter_modifier = Some(mod_input);
                        }
                    }
                }

                if let Some(friendly_names) =
                    toml_val.get("friendly_names").and_then(|v| v.as_table())
                {
                    for (key_name, value) in friendly_names {
                        if let Some(name_str) = value.as_str() {
                            if let Some(g_input) = parse_gamepad_input(key_name) {
                                ctrl_cfg
                                    .friendly_names
                                    .insert(g_input.clone(), name_str.to_string());
                            }
                        }
                    }
                }

                // parse "devices"
                if let Some(devices_val) = toml_val.get("devices") {
                    if let Some(devices_arr) = devices_val.as_array() {
                        for device_val in devices_arr {
                            if let Some(table) = device_val.as_table() {
                                let vendor =
                                    table.get("vendor").and_then(|v| v.as_str()).unwrap_or("");
                                let product =
                                    table.get("product").and_then(|v| v.as_str()).unwrap_or("");
                                let device =
                                    table.get("device").and_then(|v| v.as_str()).unwrap_or("");
                                let instructions = table
                                    .get("instructions")
                                    .and_then(|v| v.as_str())
                                    .unwrap_or("");
                                // Add to ctrl_cfg
                                if !vendor.is_empty() && !product.is_empty() {
                                    ctrl_cfg.devices.push(ConfigDevice {
                                        vendor: vendor.to_string(),
                                        product: product.to_string(),
                                        device: device.to_string(),
                                        instructions: instructions.to_string(),
                                    });
                                }
                            }
                        }
                    }
                }

                debug!("Loaded config file from '{}'", config_file_path);

                return Some(ctrl_cfg);
            }
        }
    }

    trace!(
        "No config file found for controller {}_{}",
        vendor_id,
        product_id
    );
    None
}
