use crate::types::ControllerConfig;
use std::collections::HashSet;
use std::io::Read;
use std::{collections::HashMap, fs::File};

use evdev::{AbsoluteAxisType, Key};
use log::{debug, trace};
use toml::Value as TomlValue;

use crate::types::{Direction, GamepadInput, Mapping, Modifiers};

/// Parse a `Mapping` from a TOML value.
pub fn parse_mapping(value: &TomlValue) -> Option<Mapping> {
    if let Some(s) = value.as_str() {
        // single character or known special key
        if s.len() == 1 {
            return Some(Mapping::Character(s.chars().next().unwrap()));
        } else if let Some(key) = key_name_to_key(s) {
            return Some(Mapping::Key(key));
        }
    }
    None
}

/// Convert a string like "ENTER" -> Key::KEY_ENTER, etc.
pub fn key_name_to_key(name: &str) -> Option<Key> {
    match name {
        "ENTER" => Some(Key::KEY_ENTER),
        "ESCAPE" => Some(Key::KEY_ESC),
        "BACKSPACE" => Some(Key::KEY_BACKSPACE),
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

pub fn parse_controller_config(vendor_id: u16, product_id: u16) -> Option<ControllerConfig> {
    let config_paths = vec![
        format!("/usr/share/deckrypt/{}_{}.toml", vendor_id, product_id),
        format!("/etc/deckrypt/{}_{}.toml", vendor_id, product_id),
    ];

    for config_file_path in config_paths {
        if let Ok(mut file) = File::open(&config_file_path) {
            let mut contents = String::new();
            if let Ok(_) = file.read_to_string(&mut contents) {
                if let Ok(toml_val) = contents.parse::<TomlValue>() {
                    // We'll build up the final ControllerConfig
                    let mut ctrl_cfg = ControllerConfig {
                        required_buttons: HashSet::new(),
                        required_axes: HashSet::new(),
                        manual_mappings: vec![],
                        axis_mappings: vec![],
                        alternate_manual_mappings: vec![],
                        alternate_axis_mappings: vec![],
                        modifiers: Modifiers::default(),
                        friendly_names: HashMap::new(),
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
                                    ctrl_cfg.manual_mappings.push((g_input, mapping));
                                }
                            }
                        }
                    }

                    // parse "axes"
                    if let Some(axes) = toml_val.get("axes").and_then(|v| v.as_table()) {
                        for (axis_name, value) in axes {
                            if let Some(axis_values) = value.as_table() {
                                // We might have e.g. axes.ABS_X.negative, axes.ABS_X.positive
                                // so parse "negative" => parse_gamepad_input("ABS_X_NEG") etc.
                                if let Some(neg_value) = axis_values.get("negative") {
                                    let neg_input_str = format!("{}_NEG", axis_name);
                                    if let Some(gi) = parse_gamepad_input(&neg_input_str) {
                                        match gi {
                                            GamepadInput::Button(k) => {
                                                ctrl_cfg.required_buttons.insert(k);
                                            }
                                            GamepadInput::Axis(ax, _) => {
                                                ctrl_cfg.required_axes.insert(ax);
                                            }
                                        }
                                        if let Some(mapping) = parse_mapping(neg_value) {
                                            ctrl_cfg.axis_mappings.push((gi, mapping));
                                        }
                                    }
                                }
                                if let Some(pos_value) = axis_values.get("positive") {
                                    let pos_input_str = format!("{}_POS", axis_name);
                                    if let Some(gi) = parse_gamepad_input(&pos_input_str) {
                                        match gi {
                                            GamepadInput::Button(k) => {
                                                ctrl_cfg.required_buttons.insert(k);
                                            }
                                            GamepadInput::Axis(ax, _) => {
                                                ctrl_cfg.required_axes.insert(ax);
                                            }
                                        }
                                        if let Some(mapping) = parse_mapping(pos_value) {
                                            ctrl_cfg.axis_mappings.push((gi, mapping));
                                        }
                                    }
                                }
                            } else {
                                // If it's not an object with "negative"/"positive",
                                // maybe it’s a direct axis name
                                if let Some(axis_type) = axis_name_to_absolute_axis_type(axis_name)
                                {
                                    ctrl_cfg.required_axes.insert(axis_type.0);
                                }
                            }
                        }
                    }

                    // parse "alternate_buttons"
                    if let Some(buttons) =
                        toml_val.get("alternate_buttons").and_then(|v| v.as_table())
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
                                    ctrl_cfg.alternate_manual_mappings.push((g_input, mapping));
                                }
                            }
                        }
                    }

                    // parse "alternate_axes"
                    if let Some(axes) = toml_val.get("alternate_axes").and_then(|v| v.as_table()) {
                        for (axis_name, value) in axes {
                            if let Some(axis_values) = value.as_table() {
                                if let Some(neg_value) = axis_values.get("negative") {
                                    let neg_input_str = format!("{}_NEG", axis_name);
                                    if let Some(gi) = parse_gamepad_input(&neg_input_str) {
                                        match gi {
                                            GamepadInput::Button(k) => {
                                                ctrl_cfg.required_buttons.insert(k);
                                            }
                                            GamepadInput::Axis(ax, _) => {
                                                ctrl_cfg.required_axes.insert(ax);
                                            }
                                        }
                                        if let Some(mapping) = parse_mapping(neg_value) {
                                            ctrl_cfg.alternate_axis_mappings.push((gi, mapping));
                                        }
                                    }
                                }
                                if let Some(pos_value) = axis_values.get("positive") {
                                    let pos_input_str = format!("{}_POS", axis_name);
                                    if let Some(gi) = parse_gamepad_input(&pos_input_str) {
                                        match gi {
                                            GamepadInput::Button(k) => {
                                                ctrl_cfg.required_buttons.insert(k);
                                            }
                                            GamepadInput::Axis(ax, _) => {
                                                ctrl_cfg.required_axes.insert(ax);
                                            }
                                        }
                                        if let Some(mapping) = parse_mapping(pos_value) {
                                            ctrl_cfg.alternate_axis_mappings.push((gi, mapping));
                                        }
                                    }
                                }
                            } else {
                                if let Some(axis_type) = axis_name_to_absolute_axis_type(axis_name)
                                {
                                    ctrl_cfg.required_axes.insert(axis_type.0);
                                }
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
                        if let Some(alternate_key) =
                            mods.get("alternate_key").and_then(|v| v.as_str())
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
                    debug!("Loaded config file from '{}'", config_file_path);

                    return Some(ctrl_cfg);
                }
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
