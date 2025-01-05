use clap::Parser;

/// Command-line argument parsing
#[derive(Parser, Debug)]
#[command(
    name = "Deckrypt",
    version = env!("CARGO_PKG_VERSION"),
    author = "Tobias Görgens <tobiasg-privat@proton.me>",
    about = "Map gamepad inputs to keyboard events",
    after_help = "Source: https://github.com/TobiPeterG/deckrypt",
    help_template = "\
{before-help}{name} {version}
{author-with-newline}{about-with-newline}
{usage-heading} {usage}

{all-args}{after-help}
"
)]
pub struct Args {
    /// Increases verbosity level (-v, ..., -vvvv)
    #[arg(short, action = clap::ArgAction::Count)]
    pub verbosity: u8,

    /// Use unknown devices and prompt for selection (not compatible with -a & -c)
    #[arg(short, long)]
    pub unknown: bool,

    /// Automatically select the first device if multiple devices are found (not compatible with -u)
    #[arg(short, long)]
    pub auto_select: bool,

    /// Continuously search for devices with a config file (not compatible with -u)
    #[arg(short, long)]
    pub continuously_search: bool,

    /// Automatically map unassigned gamepad inputs
    #[arg(short, long)]
    pub mapping: bool,

    /// Enable friendly logging output
    #[arg(short, long)]
    pub friendly: bool,

    /// Test if a supported device is connected
    #[arg(short, long)]
    pub test: bool,

    /// Print connected devices
    #[arg(short = 'd', long = "devices")]
    pub show_devices: bool,
}
