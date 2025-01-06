// main.rs
use clap::Parser;
use log::{debug, error, info, warn};
use std::io;
use sudo;

mod cli;
mod config;
mod device;
mod input;
mod keymap;
mod types;

fn main() -> io::Result<()> {
    let args = cli::Args::parse();

    // Initialize the logger with the appropriate level
    initialize_logger(args.verbosity);

    if args.test {
        // Perform the device test
        match crate::device::is_supported_device_connected(&args) {
            Ok(true) => {
                info!("Supported device is connected.");
                std::process::exit(0);
            }
            Ok(false) => {
                error!("No supported device is connected.");
                std::process::exit(1);
            }
            Err(e) => {
                error!("Error during device test: {}", e);
                std::process::exit(2);
            }
        }
    }

    if args.show_devices {
        let hardware_devices = crate::device::get_device_ids();
        if hardware_devices.is_empty() {
            std::process::exit(1);
        }
        for device in hardware_devices {
            println!(
                "Vendor: '{}', Product: '{}', Device: '{}', Shift: '{}', Alternate: '{}', Enter: '{}', Instructions: '{}'",
                device.vendor, device.product, device.device, device.shift, device.alternate, device.enter, device.instructions
            )
        }
        std::process::exit(0);
    }

    // Use the 'sudo' crate to escalate privileges if needed
    sudo::escalate_if_needed().expect("Failed to escalate privileges");

    if args.continuously_search {
        loop {
            match input::run_main_loop(&args) {
                Ok(_) => {
                    debug!("deckrypt stopped without error; restarting...");
                    std::thread::sleep(std::time::Duration::from_secs(1));
                }
                Err(e) => {
                    warn!("{} Retrying...", e);
                    std::thread::sleep(std::time::Duration::from_secs(1));
                }
            }
        }
    } else {
        match input::run_main_loop(&args) {
            Ok(_) => {
                info!("deckrypt stopped without error; exiting...");
                std::process::exit(0);
            }
            Err(e) => {
                error!("{} Exiting...", e);
                std::process::exit(1);
            }
        }
    }
}

fn initialize_logger(verbosity: u8) {
    use log::LevelFilter;

    let log_level = match verbosity {
        0 => LevelFilter::Error,
        1 => LevelFilter::Warn,
        2 => LevelFilter::Info,
        3 => LevelFilter::Debug,
        _ => LevelFilter::Trace,
    };

    env_logger::Builder::new().filter(None, log_level).init();
}
