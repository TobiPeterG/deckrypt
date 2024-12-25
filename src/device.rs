use evdev::Device;
use log::{debug, error};
use std::fs;
use std::os::unix::fs::FileTypeExt;

use crate::cli::Args;
use crate::types::{KnownDeviceUnparsed, SelectedDevice, UnknownDevice};

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

                        // Possible config file paths
                        let config_paths = vec![
                            format!("/usr/share/deckrypt/{}_{}.toml", vendor_id, product_id),
                            format!("/etc/deckrypt/{}_{}.toml", vendor_id, product_id),
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
                                config_file_path: Some(cfpath.clone()),
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

/// Attempts to select a single device based on user arguments and the discovered devices.
///  
/// 1) If `args.unknown` is specified, we list unknown devices and let the user pick one.  
/// 2) Otherwise, we look at known devices.  
///   - If there is only one known device, select it.  
///   - If `args.auto_select` is set, automatically pick the first known device.  
///   - Otherwise, prompt the user to select from multiple known devices.  
///  
/// Returns `Some(SelectedDevice::Known(...))` or `Some(SelectedDevice::Unknown(...))` on success,
/// or `None` if no device could be selected.
pub fn attempt_device_selection(args: &Args) -> Option<SelectedDevice> {
    use std::io::{self, Write};

    let (known_devices, unknown_devices) = scan_devices_for_config();

    // if user wants unknown
    if args.unknown {
        if unknown_devices.is_empty() {
            error!("No unknown devices found!");
            return None;
        } else {
            // List them and pick
            println!("Found the following unknown devices:");
            for (i, dev) in unknown_devices.iter().enumerate() {
                println!(
                    "{}: {} - {} (Vendor ID: {:04x}, Product ID: {:04x})",
                    i, dev.path, dev.name, dev.vendor_id, dev.product_id
                );
            }
            print!("Enter the number of the device to use: ");
            io::stdout().flush().unwrap();
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
                    Some(SelectedDevice::Unknown(chosen))
                }
                _ => {
                    error!("Invalid selection.");
                    None
                }
            }
        }
    } else {
        // known devices
        if known_devices.is_empty() {
            return None;
        } else if known_devices.len() == 1 {
            let dev = known_devices[0].clone();
            debug!(
                "Found device with config: {} (Vendor ID: {:04x}, Product ID: {:04x})",
                dev.path, dev.vendor_id, dev.product_id
            );
            Some(SelectedDevice::Known(dev))
        } else {
            // multiple => either auto_select or prompt
            if args.auto_select {
                let dev = known_devices[0].clone();
                debug!(
                    "Automatically selected device: {} (Vendor ID: {:04x}, Product ID: {:04x})",
                    dev.name, dev.vendor_id, dev.product_id
                );
                Some(SelectedDevice::Known(dev))
            } else {
                println!("Multiple known devices with config files found:");
                for (i, dev) in known_devices.iter().enumerate() {
                    println!(
                        "{}: {} - {} (Vendor ID: {:04x}, Product ID: {:04x})",
                        i, dev.path, dev.name, dev.vendor_id, dev.product_id
                    );
                }
                print!("Enter the number of the device to use: ");
                io::stdout().flush().unwrap();
                let mut input = String::new();
                if io::stdin().read_line(&mut input).is_err() {
                    error!("Failed to read input.");
                    return None;
                }
                let selection = input.trim().parse::<usize>();
                match selection {
                    Ok(num) if num < known_devices.len() => {
                        let chosen_dev = known_devices[num].clone();
                        Some(SelectedDevice::Known(chosen_dev))
                    }
                    _ => {
                        error!("Invalid selection.");
                        None
                    }
                }
            }
        }
    }
}
