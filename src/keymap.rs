use libc::{c_int, getuid};
use log::error;
use std::collections::HashMap;
use std::fs::File;
use std::os::unix::io::AsRawFd;

use evdev::Key;

/// FFI bindings to libkeymap, which we use to read the kernel's keymap.
///  
/// - `LkCtx`: Opaque struct representing a context.
/// - `lk_init()`: Initialize libkeymap.
/// - `lk_free(ctx)`: Free libkeymap context.
/// - `lk_kernel_keymap(ctx, fd)`: Read the kernel keymap from a file descriptor.
/// - `lk_get_key(ctx, level, code)`: Get the key for a specific level and code.
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

/// Extracts the key type from a key definition.
fn ktyp(x: c_int) -> c_int {
    (x >> 8) & 0xFF
}

/// Extracts the key value from a key definition.
fn kval(x: c_int) -> c_int {
    x & 0xFF
}

const KT_LATIN: c_int = 0;
const KT_LETTER: c_int = 11;

/// Generates a mapping of characters to `(Key, level)` by reading from the kernel keymap.
///  
/// This uses `libkeymap` to read key definitions at different levels (0,1,2).
/// We also detect ENTER (which can show up differently) and build a secondary map
/// of digit -> shifted digit if applicable. Returns `None` if unable to read from any suitable TTY/console.
pub fn generate_chrmap() -> Option<HashMap<char, (Key, u8)>> {
    let mut fd: Option<File> = None;

    // Try environment TTY, /dev/console, or /dev/tty0
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
        error!(
            "Needs to run on tty/console or as root to read the kernel keymap. Cannot read keymap."
        );
        return None;
    }
    let fd = fd.unwrap();

    unsafe {
        let ctx = lk_init();
        if ctx.is_null() {
            error!("Failed to initialize libkeymap context");
            return None;
        }

        if lk_kernel_keymap(ctx, fd.as_raw_fd()) != 0 {
            error!("Failed to read kernel keymap");
            lk_free(ctx);
            return None;
        }

        let mut chrmap = HashMap::new();

        const KEY_MAX: u16 = 255;

        for i in 1..=KEY_MAX as i32 {
            let mut chars = [None, None, None]; // levels 0..2

            for j in 0..3 {
                let code = lk_get_key(ctx, j, i);
                let t = ktyp(code);
                let v = kval(code);
                if t == KT_LATIN || t == KT_LETTER {
                    if let Some(ch) = std::char::from_u32(v as u32) {
                        chars[j as usize] = Some(ch);

                        let keycode = Key::new(i as u16);
                        let modifier = j as u8;
                        chrmap.entry(ch).or_insert((keycode, modifier));
                    }
                } else if code == 513 && !chrmap.contains_key(&'\n') {
                    // handle ENTER
                    let keycode = Key::new(i as u16);
                    let modifier = j as u8;
                    chrmap.insert('\n', (keycode, modifier));
                }
            }
        }

        lk_free(ctx);
        Some(chrmap)
    }
}

/// A list of characters that are allowed to be mapped.
// This list should include all characters handled by `shift_transform`.
pub const ALLOWED_CHARACTERS: &[char] = &[
    // Letters
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's',
    't', 'u', 'v', 'w', 'x', 'y', 'z',
    '1', '2', '3', '4', '5', '6', '7', '8', '9', '0',  // Digits
    '-', '=', '[', ']', '\\', ';', '\'', ',', '.', '/', // Common Symbols
    '`',
    // Add more symbols here if `shift_transform` is expanded
];

/// Transforms a character to its shifted counterpart.
///
/// This function mimics the behavior of holding down the Shift key on a standard US QWERTY keyboard.
/// It handles:
/// - Lowercase letters to uppercase letters.
/// - Numbers to their corresponding symbols.
/// - Common symbols to their shifted versions.
///
/// # Parameters
/// - `c`: The input character to be transformed.
///
/// # Returns
/// - The shifted character if a mapping exists; otherwise, returns the original character.
pub fn shift_transform(c: char) -> char {
    match c {
        // Letters: a-z -> A-Z
        'a'..='z' => (c as u8 - b'a' + b'A') as char,

        // Numbers: 0-9 to corresponding symbols
        '1' => '!',
        '2' => '@',
        '3' => '#',
        '4' => '$',
        '5' => '%',
        '6' => '^',
        '7' => '&',
        '8' => '*',
        '9' => '(',
        '0' => ')',

        // Symbols: Map to their shifted counterparts
        '-' => '_',
        '=' => '+',
        '[' => '{',
        ']' => '}',
        '\\' => '|',
        ';' => ':',
        '\'' => '"',
        ',' => '<',
        '.' => '>',
        '/' => '?',
        '`' => '~',

        // Space and other non-mappable characters remain unchanged
        _ => c,
    }
}
