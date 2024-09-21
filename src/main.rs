use evdev::{
    uinput::VirtualDeviceBuilder, AbsoluteAxisType, AttributeSet, Device, EventType, InputEvent,
    InputEventKind, Key,
};
use libc::input_absinfo;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::os::unix::fs::FileTypeExt;

// Percentage threshold for activation (e.g., 50%)
const AXIS_THRESHOLD_PERCENTAGE: f32 = 0.5;

#[derive(Hash, Eq, PartialEq, Debug)]
enum Direction {
    Positive,
    Negative,
}

#[derive(Hash, Eq, PartialEq, Debug)]
enum GamepadInput {
    Button(Key),
    Axis(u16, Direction), // Use u16 instead of AbsoluteAxisType
}

fn create_virtual_keyboard(
    all_keyboard_keys: &[Key],
) -> std::io::Result<evdev::uinput::VirtualDevice> {
    let mut keys = AttributeSet::<Key>::new();
    for &key in all_keyboard_keys {
        keys.insert(key);
    }

    let device = VirtualDeviceBuilder::new()?
        .name("Virtual Keyboard")
        .with_keys(&keys)?
        .build()?;

    Ok(device)
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
                        println!(
                            "Device '{}' at {:?} supports BTN_SOUTH",
                            device.name().unwrap_or("Unknown"),
                            path
                        );
                        return Some(device);
                    }
                }
            }
        }
    }

    None
}

// Function to get the activation threshold (percentage of maximum) and release threshold (percentage of minimum)
fn get_axis_thresholds(abs_info: &input_absinfo) -> (i32, i32) {
    // Special case for D-pad-like axes with a small range (-1 to 1)
    if abs_info.maximum == 1 && abs_info.minimum == -1 {
        // No threshold needed; directly return the activation and release points
        return (1, -1);
    }

    // For larger axes (like joysticks) use percentage-based thresholds
    let activation_threshold = (AXIS_THRESHOLD_PERCENTAGE * abs_info.maximum as f32) as i32;
    let mut release_threshold = (AXIS_THRESHOLD_PERCENTAGE * abs_info.minimum as f32) as i32;

    if abs_info.minimum == 0 {
        release_threshold = -1;
    }

    (activation_threshold, release_threshold)
}

// Function to get the next available key that hasn't been used yet
fn get_next_available_key(
    mut available_keys: impl Iterator<Item = Key>,
    used_keys: &HashSet<Key>,
) -> Option<Key> {
    // Skip already used keys
    while let Some(next_key) = available_keys.next() {
        if !used_keys.contains(&next_key) {
            return Some(next_key);
        }
    }
    None
}

// Function to map gamepad events to keyboard keys (both buttons and axes)
fn get_key_mapping(
    device: &Device,
    manual_mappings: &[(Key, Key)],
    axis_mappings: &[(AbsoluteAxisType, (Key, Key))],
    mut available_keys: impl Iterator<Item = Key>,
) -> HashMap<GamepadInput, Key> {
    let mut input_mapping = HashMap::new();
    let mut used_keys: HashSet<Key> = HashSet::new();

    // Manually map some gamepad buttons to keyboard keys
    for &(gamepad_key, keyboard_key) in manual_mappings {
        input_mapping.insert(GamepadInput::Button(gamepad_key), keyboard_key);
        used_keys.insert(keyboard_key); // Track used keys
    }

    // Manually map axes to keyboard keys
    for &(axis, (neg_key, pos_key)) in axis_mappings {
        input_mapping.insert(GamepadInput::Axis(axis.0, Direction::Negative), neg_key);
        input_mapping.insert(GamepadInput::Axis(axis.0, Direction::Positive), pos_key);
        used_keys.insert(neg_key); // Track used keys
        used_keys.insert(pos_key); // Track used keys
    }

    // Get the list of supported gamepad actions (buttons)
    if let Some(supported_keys) = device.supported_keys() {
        for key in supported_keys.iter() {
            if !input_mapping.contains_key(&GamepadInput::Button(key)) {
                if let Some(next_key) = get_next_available_key(available_keys.by_ref(), &used_keys)
                {
                    input_mapping.insert(GamepadInput::Button(key), next_key);
                    used_keys.insert(next_key); // Track used keys
                    println!(
                        "Automatically mapped gamepad button {:?} to keyboard key {:?}",
                        key, next_key
                    );
                }
            }
        }
    }

    // Get the list of supported axes and retrieve axis information for threshold calculation
    if let Some(supported_axes) = device.supported_absolute_axes() {
        for axis in supported_axes.iter() {
            if !input_mapping.contains_key(&GamepadInput::Axis(axis.0, Direction::Negative)) {
                if let Some(neg_key) = get_next_available_key(available_keys.by_ref(), &used_keys) {
                    input_mapping.insert(GamepadInput::Axis(axis.0, Direction::Negative), neg_key);
                    used_keys.insert(neg_key); // Track used keys
                    println!(
                        "Automatically mapped axis {:?} (negative) to keyboard key {:?}",
                        axis, neg_key
                    );
                }
            }

            if !input_mapping.contains_key(&GamepadInput::Axis(axis.0, Direction::Positive)) {
                if let Some(pos_key) = get_next_available_key(available_keys.by_ref(), &used_keys) {
                    input_mapping.insert(GamepadInput::Axis(axis.0, Direction::Positive), pos_key);
                    used_keys.insert(pos_key); // Track used keys
                    println!(
                        "Automatically mapped axis {:?} (positive) to keyboard key {:?}",
                        axis, pos_key
                    );
                }
            }
        }
    }

    input_mapping
}

fn main() -> std::io::Result<()> {
    let all_keyboard_keys = [
        Key::KEY_A,
        Key::KEY_B,
        Key::KEY_C,
        Key::KEY_D,
        Key::KEY_E,
        Key::KEY_F,
        Key::KEY_G,
        Key::KEY_H,
        Key::KEY_I,
        Key::KEY_J,
        Key::KEY_K,
        Key::KEY_L,
        Key::KEY_M,
        Key::KEY_N,
        Key::KEY_O,
        Key::KEY_P,
        Key::KEY_Q,
        Key::KEY_R,
        Key::KEY_S,
        Key::KEY_T,
        Key::KEY_U,
        Key::KEY_V,
        Key::KEY_W,
        Key::KEY_X,
        Key::KEY_Y,
        Key::KEY_Z,
        Key::KEY_1,
        Key::KEY_2,
        Key::KEY_3,
        Key::KEY_4,
        Key::KEY_5,
        Key::KEY_6,
        Key::KEY_7,
        Key::KEY_8,
        Key::KEY_9,
        Key::KEY_0,
        Key::KEY_ENTER,
    ];

    let manual_mappings = [
        (Key::BTN_SOUTH, Key::KEY_A),
        (Key::BTN_NORTH, Key::KEY_B),
        (Key::BTN_WEST, Key::KEY_C),
        (Key::BTN_EAST, Key::KEY_D),
    ];

    let axis_mappings = [
        (AbsoluteAxisType::ABS_X, (Key::KEY_A, Key::KEY_D)),
        (AbsoluteAxisType::ABS_Y, (Key::KEY_W, Key::KEY_S)),
    ];

    let mut virtual_keyboard =
        create_virtual_keyboard(&all_keyboard_keys).expect("Failed to create virtual keyboard");

    if let Some(mut gamepad_device) = find_first_device_with_btn_south() {
        println!("Listening for gamepad events...");

        let manually_mapped_keys: HashSet<_> = manual_mappings.iter().map(|&(_, k)| k).collect();

        let available_keys = all_keyboard_keys
            .iter()
            .filter(move |&&k| !manually_mapped_keys.contains(&k))
            .copied();

        let input_mapping = get_key_mapping(
            &gamepad_device,
            &manual_mappings,
            &axis_mappings,
            available_keys,
        );

        let mut axis_states: HashMap<u16, i32> = HashMap::new();
        let mut active_axis = None;

        // Fetch abs_info before entering the loop
        let abs_info_map = gamepad_device.get_abs_state().ok();

        loop {
            for ev in gamepad_device
                .fetch_events()
                .expect("Failed to fetch events")
            {
                match ev.kind() {
                    InputEventKind::Key(key) => {
                        let gamepad_input = GamepadInput::Button(key);
                        if ev.value() == 1 {
                            if let Some(mapped_key) = input_mapping.get(&gamepad_input) {
                                let down_event =
                                    InputEvent::new(EventType::KEY, mapped_key.code(), 1);
                                virtual_keyboard
                                    .emit(&[down_event])
                                    .expect("Failed to press key");
                                println!(
                                    "Mapped gamepad button {:?} to keyboard key {:?}",
                                    key, mapped_key
                                );
                            }
                        } else if ev.value() == 0 {
                            if let Some(mapped_key) = input_mapping.get(&gamepad_input) {
                                let up_event =
                                    InputEvent::new(EventType::KEY, mapped_key.code(), 0);
                                virtual_keyboard
                                    .emit(&[up_event])
                                    .expect("Failed to release key");
                                println!("Released keyboard key {:?}", mapped_key);
                            }
                        }
                    }
                    InputEventKind::AbsAxis(axis) => {
                        let axis_value = ev.value();
                        let previous_value = axis_states.get(&axis.0).cloned().unwrap_or(0);

                        if active_axis.is_none() || active_axis == Some(axis.0) {
                            if let Some(abs_info) =
                                abs_info_map.as_ref().map(|s| &s[axis.0 as usize])
                            {
                                let (activation_threshold, release_threshold) =
                                    get_axis_thresholds(abs_info);

                                // Activation for negative direction
                                if axis_value <= release_threshold
                                    && previous_value > release_threshold
                                {
                                    let neg_input = GamepadInput::Axis(axis.0, Direction::Negative);
                                    if let Some(mapped_key) = input_mapping.get(&neg_input) {
                                        let down_event =
                                            InputEvent::new(EventType::KEY, mapped_key.code(), 1);
                                        virtual_keyboard
                                            .emit(&[down_event])
                                            .expect("Failed to press key");
                                        println!(
                                            "Mapped axis {:?} to negative key {:?}",
                                            axis, mapped_key
                                        );
                                        active_axis = Some(axis.0);
                                    }
                                }
                                // Activation for positive direction
                                else if axis_value >= activation_threshold
                                    && previous_value < activation_threshold
                                {
                                    let pos_input = GamepadInput::Axis(axis.0, Direction::Positive);
                                    if let Some(mapped_key) = input_mapping.get(&pos_input) {
                                        let down_event =
                                            InputEvent::new(EventType::KEY, mapped_key.code(), 1);
                                        virtual_keyboard
                                            .emit(&[down_event])
                                            .expect("Failed to press key");
                                        println!(
                                            "Mapped axis {:?} to positive key {:?}",
                                            axis, mapped_key
                                        );
                                        active_axis = Some(axis.0);
                                    }
                                }
                                // Reset to neutral when axis returns to the neutral zone
                                else if axis_value.abs() < activation_threshold
                                    && previous_value.abs() >= activation_threshold
                                {
                                    let neg_input = GamepadInput::Axis(axis.0, Direction::Negative);
                                    let pos_input = GamepadInput::Axis(axis.0, Direction::Positive);
                                    if let Some(neg_key) = input_mapping.get(&neg_input) {
                                        let neg_up_event =
                                            InputEvent::new(EventType::KEY, neg_key.code(), 0);
                                        virtual_keyboard
                                            .emit(&[neg_up_event])
                                            .expect("Failed to release negative key");
                                    }
                                    if let Some(pos_key) = input_mapping.get(&pos_input) {
                                        let pos_up_event =
                                            InputEvent::new(EventType::KEY, pos_key.code(), 0);
                                        virtual_keyboard
                                            .emit(&[pos_up_event])
                                            .expect("Failed to release positive key");
                                    }
                                    println!("Released keys for axis {:?}", axis);
                                    active_axis = None;
                                }
                                axis_states.insert(axis.0, axis_value);
                            }
                        }
                    }
                    _ => (),
                }
            }
        }
    } else {
        println!("No device with BTN_SOUTH found");
    }

    Ok(())
}
