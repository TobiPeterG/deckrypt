use evdev::Device;
use libc::{getpwnam, passwd};
use log::{debug, error, warn};
use std::collections::HashMap;
use std::ffi::CString;
use std::os::unix::fs::FileTypeExt;
use std::{env, fs, io};

use crate::cli::Args;
use crate::config::parse_controller_config;
use crate::types::{
    ConfigDevice, ControllerConfig, KnownDeviceUnparsed, SelectedDevice, UnknownDevice,
};

/// Scans `/dev/input` for devices and checks if each device has a corresponding Deckrypt config file.
///  
/// Returns two lists:  
/// - `Vec<KnownDeviceUnparsed>` for devices that have a matching config file
/// - `Vec<UnknownDevice>` for devices that do not have a config file
pub fn scan_devices_for_config() -> (Vec<KnownDeviceUnparsed>, Vec<UnknownDevice>) {
    let input_dir = "/dev/input";
    let entries = fs::read_dir(input_dir).expect("Failed to read /dev/input");

    let mut known = Vec::new();
    let mut unknown = Vec::new();

    for entry in entries {
        if let Ok(entry) = entry {
            let path = entry.path();
            if let Ok(metadata) = fs::metadata(&path) {
                if metadata.file_type().is_char_device() {
                    if let Ok(device) = Device::open(&path) {
                        let name = device.name().unwrap_or("Unknown").to_string();
                        let vendor_id = device.input_id().vendor();
                        let product_id = device.input_id().product();

                        let sudo_user = env::var("SUDO_USER").unwrap_or("root".to_string());

                        // Convert user name to C string
                        let c_user = CString::new(sudo_user.clone()).unwrap_or_default();

                        let mut home_str = "/root".to_string();

                        // Call getpwnam
                        unsafe {
                            let pwd: *mut passwd = getpwnam(c_user.as_ptr());
                            if pwd.is_null() {
                                error!("User {} not found", sudo_user);
                            } else {
                                // Access home directory from the returned struct
                                let home_dir = (*pwd).pw_dir;
                                if !home_dir.is_null() {
                                    home_str = std::ffi::CStr::from_ptr(home_dir)
                                        .to_string_lossy()
                                        .into_owned();
                                } else {
                                    error!("HOME directory could not be identified.");
                                }
                            }
                        }

                        // Possible config file paths
                        let config_paths = vec![
                            format!(
                                "{}/.local/share/deckrypt/{}_{}.toml",
                                home_str, vendor_id, product_id
                            ),
                            format!("/etc/deckrypt/{}_{}.toml", vendor_id, product_id),
                            format!("/usr/share/deckrypt/{}_{}.toml", vendor_id, product_id),
                        ];

                        // Check if at least one config file path exists
                        let file_path_found = config_paths
                            .iter()
                            .find(|candidate| fs::metadata(candidate).is_ok());

                        if let Some(cfpath) = file_path_found {
                            // We have a known device, but we do *not* parse the config yet
                            known.push(KnownDeviceUnparsed {
                                path: path.to_string_lossy().to_string(),
                                name: name.clone(),
                                vendor_id,
                                product_id,
                                config_file_path: cfpath.clone(),
                            });
                            debug!("Found known device with config file presence: {}", name);
                        } else {
                            // no file found => unknown
                            unknown.push(UnknownDevice {
                                path: path.to_string_lossy().to_string(),
                                name: name.clone(),
                                vendor_id,
                                product_id,
                            });
                        }
                    }
                }
            }
        }
    }

    (known, unknown)
}

/// Checks if a supported device is connected based on the provided arguments.
///
/// A device is considered supported if:
/// - It has a corresponding config file (known device), or
/// - It's an unknown device and the `unknown` flag is set.
///
/// # Parameters
/// - `args`: Parsed command-line arguments.
///
/// # Returns
/// - `Ok(true)`: If a supported device is found.
/// - `Ok(false)`: If no supported devices are found.
/// - `Err(e)`: If an error occurs during the check.
pub fn is_supported_device_connected(args: &Args) -> io::Result<bool> {
    let (known_devices, unknown_devices) = scan_devices_for_config();

    if args.unknown {
        // Supported devices include known and unknown
        return Ok(!known_devices.is_empty() || !unknown_devices.is_empty());
    } else {
        // Supported devices include only known
        return Ok(!known_devices.is_empty());
    }
}

/// Checks if a device supports all required buttons as per its ControllerConfig.
///
/// # Arguments
///
/// * `device` - The evdev::Device to check.
/// * `config` - The ControllerConfig containing required_buttons.
///
/// # Returns
///
/// * `true` if the device supports all required buttons.
/// * `false` otherwise.
fn device_supports_config(device: &Device, config: &ControllerConfig) -> bool {
    if let Some(supported_keys) = device.supported_keys() {
        config
            .required_buttons
            .iter()
            .all(|k| supported_keys.contains(*k))
    } else {
        false
    }
}

fn load_and_validate(
    config_path: &String,
    devices: &Vec<KnownDeviceUnparsed>,
) -> Option<Vec<KnownDeviceUnparsed>> {
    // Load the configuration
    let config = match parse_controller_config(
        devices[0].vendor_id,
        devices[0].product_id,
        config_path.clone(),
    ) {
        Some(cfg) => cfg,
        None => {
            error!(
                "Failed to parse config file '{}'. Skipping devices with this config.",
                config_path
            );
            return None;
        }
    };

    // Validate each device in the group
    let mut valid_devices = Vec::new();
    for device in devices {
        // Open the device
        let device_path = &device.path;
        match Device::open(device_path) {
            Ok(dev) => {
                if device_supports_config(&dev, &config) {
                    valid_devices.push(device.clone());
                } else {
                    warn!(
                        "Device '{}' (VID {:04x}, PID {:04x}) does not support all required buttons and will be excluded.",
                        device.name, device.vendor_id, device.product_id
                    );
                }
            }
            Err(e) => {
                warn!(
                    "Failed to open device '{}': {}. It will be excluded from selection.",
                    device.path, e
                );
            }
        }
    }

    if valid_devices.is_empty() {
        warn!(
            "No devices found supporting all required buttons for config '{}'.",
            config_path
        );
        return None;
    }

    Some(valid_devices)
}

/// Attempts to select a single device based on user arguments and the discovered devices.
///
/// Enhanced to handle multiple devices per config file by validating device capabilities.
///
/// Returns `Some(SelectedDevice::Known(...))` or `Some(SelectedDevice::Unknown(...))` on success,
/// or `None` if no device could be selected.
pub fn attempt_device_selection(args: &Args) -> Option<SelectedDevice> {
    use std::io::{self, Write};

    let (known_devices, unknown_devices) = scan_devices_for_config();

    // Handle unknown devices if the user specified the -u flag
    if args.unknown {
        if unknown_devices.is_empty() {
            error!("No unknown devices found!");
            return None;
        } else {
            // List unknown devices and allow the user to select one
            println!("Found the following unknown devices:");
            for (i, dev) in unknown_devices.iter().enumerate() {
                println!(
                    "{}: {} - {} (Vendor ID: {:04x}, Product ID: {:04x})",
                    i, dev.path, dev.name, dev.vendor_id, dev.product_id
                );
            }
            print!("Enter the number of the device to use: ");
            io::stdout().flush().unwrap_or_default();
            let mut input = String::new();
            if io::stdin().read_line(&mut input).is_err() {
                error!("Failed to read input.");
                return None;
            }
            let selection = input.trim().parse::<usize>();
            match selection {
                Ok(num) if num < unknown_devices.len() => {
                    let chosen = unknown_devices[num].clone();
                    debug!(
                        "Selected unknown device: {} (VID {:04x}, PID {:04x})",
                        chosen.path, chosen.vendor_id, chosen.product_id
                    );
                    return Some(SelectedDevice::Unknown(chosen));
                }
                _ => {
                    error!("Invalid selection.");
                    return None;
                }
            }
        }
    }

    // Group known devices by their config file path
    let mut config_to_devices: HashMap<String, Vec<KnownDeviceUnparsed>> = HashMap::new();
    for device in known_devices {
        config_to_devices
            .entry(device.config_file_path.clone())
            .or_default()
            .push(device);
    }

    for (config_path, devices) in config_to_devices.iter() {
        // Validate each device in the group
        let pot_valid_devices = load_and_validate(config_path, devices);

        let valid_devices = match pot_valid_devices {
            Some(val_dev) => val_dev,
            None => continue,
        };

        // Selection logic based on the number of valid devices
        if valid_devices.len() == 1 {
            let device = valid_devices[0].clone();
            debug!(
                "Selected device '{}' (VID {:04x}, PID {:04x}) as the only valid device for config '{}'.",
                device.name, device.vendor_id, device.product_id, config_path
            );
            return Some(SelectedDevice::Known(device));
        } else {
            if args.auto_select {
                let device = valid_devices[0].clone();
                debug!(
                    "Automatically selected device '{}' (VID {:04x}, PID {:04x}) for config '{}'.",
                    device.name, device.vendor_id, device.product_id, config_path
                );
                return Some(SelectedDevice::Known(device));
            } else {
                // Prompt the user to select among valid devices
                println!(
                    "Multiple devices match config '{}'. Please select one:",
                    config_path
                );
                for (i, dev) in valid_devices.iter().enumerate() {
                    println!(
                        "{}: {} - {} (Vendor ID: {:04x}, Product ID: {:04x})",
                        i, dev.path, dev.name, dev.vendor_id, dev.product_id
                    );
                }
                print!("Enter the number of the device to use: ");
                io::stdout().flush().unwrap_or_default();
                let mut input = String::new();
                if io::stdin().read_line(&mut input).is_err() {
                    error!("Failed to read input.");
                    return None;
                }
                let selection = input.trim().parse::<usize>();
                match selection {
                    Ok(num) if num < valid_devices.len() => {
                        let chosen = valid_devices[num].clone();
                        debug!(
                            "Selected device '{}' (VID {:04x}, PID {:04x}) for config '{}'.",
                            chosen.name, chosen.vendor_id, chosen.product_id, config_path
                        );
                        return Some(SelectedDevice::Known(chosen));
                    }
                    _ => {
                        error!("Invalid selection.");
                        return None;
                    }
                }
            }
        }
    }
    error!("No supported devices found after validation.");
    None
}

pub fn get_device_ids() -> Vec<ConfigDevice> {
    // We only show devices from config files that have a supported controller connected, then exit.
    // It doesn't make sense to show all devices as we still need a controller
    let (known_devices, _) = scan_devices_for_config();

    // Group known devices by their config file path
    let mut config_to_devices: HashMap<String, Vec<KnownDeviceUnparsed>> = HashMap::new();
    for device in known_devices {
        config_to_devices
            .entry(device.config_file_path.clone())
            .or_default()
            .push(device);
    }

    let mut hardware_devices = Vec::new();
    for (config_path, devices) in config_to_devices.iter() {
        // Validate each device in the group
        let pot_valid_devices = load_and_validate(config_path, devices);

        let valid_devices;
        match pot_valid_devices {
            Some(val_dev) => valid_devices = val_dev,
            None => continue,
        };

        for device in valid_devices {
            let parsed_cfg = match crate::config::parse_controller_config(
                device.vendor_id,
                device.product_id,
                device.config_file_path.clone(),
            ) {
                Some(cfg) => cfg,
                None => {
                    warn!(
                        "Failed to parse config for device {} ({})",
                        device.name, device.config_file_path
                    );
                    continue;
                }
            };
            for hardware_device in parsed_cfg.devices {
                hardware_devices.push(hardware_device);
            }
        }
    }
    hardware_devices
}
