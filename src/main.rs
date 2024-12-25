use clap::Parser;
use std::io;
use sudo;

mod cli;
mod config;
mod device;
mod input;
mod keymap;
mod types;

/// The main function initializes the application, handling privilege escalation if needed
/// and parsing command-line arguments. Based on the arguments, it either continually loops
/// searching for and handling devices or runs a single instance of the main loop.  
///  
/// If `--continuously_search` is used, we keep restarting the device loop until a
/// device is found or an error occurs. Otherwise, we run the device loop once.
fn main() -> io::Result<()> {
    // Use the 'sudo' crate to escalate privileges if needed
    sudo::escalate_if_needed().expect("Failed to escalate privileges");

    let args = cli::Args::parse();
    let verbosity = types::determine_verbosity(args.verbosity);

    if args.continuously_search {
        loop {
            match input::run_main_loop(&args, verbosity) {
                Ok(_) => {
                    if verbosity >= types::Verbosity::Verbose {
                        println!("deckrypt stopped without error; restarting...");
                    }
                    std::thread::sleep(std::time::Duration::from_secs(1));
                }
                Err(e) => {
                    if verbosity >= types::Verbosity::Verbose {
                        eprintln!("{}. Retrying...", e);
                    }
                    std::thread::sleep(std::time::Duration::from_secs(1));
                }
            }
        }
    } else {
        match input::run_main_loop(&args, verbosity) {
            Ok(_) => {
                if verbosity >= types::Verbosity::Verbose {
                    println!("deckrypt stopped without error; exitiing...");
                }
            }
            Err(e) => {
                if verbosity >= types::Verbosity::Verbose {
                    eprintln!("{}. Exiting...", e);
                }
            }
        }
    }

    Ok(())
}
