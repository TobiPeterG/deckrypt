use libc::{c_int, getuid};
use log::{error, info, warn};
use std::collections::HashMap;
use std::fs::File;
use std::os::unix::io::AsRawFd;
use sudo;

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

    // Root permissions are required to read the kernel keymap
    sudo::escalate_if_needed().expect("Failed to escalate privileges");

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

/// Generates a character map appropriate for the current context:
/// - On Wayland, prefer compositor keymap via wl_keyboard.
/// - Otherwise (TTY/Plymouth), fall back to kernel keymap (console layout).
pub fn generate_chrmap_auto() -> Option<HashMap<char, (Key, u8)>> {
    // If we're in a Wayland session, try Wayland first.
    if std::env::var_os("WAYLAND_DISPLAY").is_some() {
        if let Some(m) = generate_chrmap_wayland() {
            info!("Using Wayland compositor keymap.");
            return Some(m);
        }
        warn!("Failed to obtain Wayland keymap; falling back to kernel keymap.");
    }

    // TTY / early boot / Plymouth
    generate_chrmap()
}

fn build_chrmap_from_xkb_state(
    keymap: &xkbcommon::xkb::Keymap,
    state: &mut xkbcommon::xkb::State,
) -> HashMap<char, (Key, u8)> {
    use xkbcommon::xkb;

    // level 0 => no modifier
    // level 1 => Shift
    // level 2 => AltGr (level3)
    let shift_idx = keymap.mod_get_index("Shift");

    // Prefer ISO_Level3_Shift if present; fall back to Mod5.
    let level3_idx = keymap.mod_get_index("ISO_Level3_Shift");
    let mod5_idx = keymap.mod_get_index("Mod5");

    let altgr_idx = if level3_idx != xkb::MOD_INVALID {
        level3_idx
    } else {
        mod5_idx
    };

    let altgr_mask = if altgr_idx != xkb::MOD_INVALID {
        1u32 << altgr_idx
    } else {
        0
    };
    let shift_mask = if shift_idx != xkb::MOD_INVALID {
        1u32 << shift_idx
    } else {
        0
    };

    let mut chrmap: HashMap<char, (Key, u8)> = HashMap::new();

    let min_kc = keymap.min_keycode().raw();
    let max_kc = keymap.max_keycode().raw();

    let mut probe = |kc_raw: u32, depressed: u32, level: u8| {
        let kc = xkb::Keycode::new(kc_raw);

        // Apply modifier mask for lookup
        state.update_mask(depressed, 0, 0, 0, 0, 0);

        let u = state.key_get_utf32(kc);
        if u == 0 {
            return;
        }
        let Some(ch) = std::char::from_u32(u) else {
            return;
        };

        // Ignore control chars except newline
        if ch.is_control() && ch != '\n' {
            return;
        }

        // Linux evdev: XKB keycode == evdev + 8
        if kc_raw < 8 {
            return;
        }
        let evdev_code = (kc_raw - 8) as u16;
        if evdev_code == 0 {
            return;
        }
        let key = Key::new(evdev_code);

        chrmap.entry(ch).or_insert((key, level));
    };

    for kc_raw in min_kc..=max_kc {
        probe(kc_raw, 0, 0);

        if shift_mask != 0 {
            probe(kc_raw, shift_mask, 1);
        }

        if altgr_mask != 0 {
            probe(kc_raw, altgr_mask, 2);
        }

        if shift_mask != 0 && altgr_mask != 0 {
            probe(kc_raw, shift_mask | altgr_mask, 2);
        }
    }

    // Ensure ENTER exists
    chrmap.entry('\n').or_insert((Key::KEY_ENTER, 0));

    chrmap
}

fn generate_chrmap_wayland() -> Option<HashMap<char, (Key, u8)>> {
    use std::cell::RefCell;
    use std::io::Read;
    use std::os::fd::IntoRawFd;
    use std::os::unix::io::FromRawFd;
    use std::rc::Rc;

    use wayland_client::{
        protocol::{wl_keyboard, wl_registry, wl_seat},
        Connection, Dispatch, EventQueue, Proxy, QueueHandle, WEnum,
    };
    use xkbcommon::xkb;

    #[derive(Default)]
    struct StateData {
        seat: Option<wl_seat::WlSeat>,
        keyboard: Option<wl_keyboard::WlKeyboard>,
        keymap_string: Option<String>,
    }

    #[derive(Default)]
    struct App {
        data: Rc<RefCell<StateData>>,
    }

    impl Dispatch<wl_registry::WlRegistry, ()> for App {
        fn event(
            state: &mut Self,
            registry: &wl_registry::WlRegistry,
            event: wl_registry::Event,
            _data: &(),
            _conn: &Connection,
            qh: &QueueHandle<Self>,
        ) {
            match event {
                wl_registry::Event::Global {
                    name,
                    interface,
                    version,
                } => {
                    if interface == wl_seat::WlSeat::interface().name {
                        let ver = std::cmp::min(version, 5);
                        let seat = registry.bind::<wl_seat::WlSeat, _, _>(name, ver, qh, ());
                        state.data.borrow_mut().seat = Some(seat);
                    }
                }
                wl_registry::Event::GlobalRemove { .. } => {}
                _ => {}
            }
        }
    }

    impl Dispatch<wl_seat::WlSeat, ()> for App {
        fn event(
            state: &mut Self,
            seat: &wl_seat::WlSeat,
            event: wl_seat::Event,
            _data: &(),
            _conn: &Connection,
            qh: &QueueHandle<Self>,
        ) {
            match event {
                wl_seat::Event::Capabilities { capabilities } => {
                    // capabilities is WEnum<Capability>
                    if let WEnum::Value(caps) = capabilities {
                        if caps.contains(wl_seat::Capability::Keyboard) {
                            if state.data.borrow().keyboard.is_none() {
                                let kb = seat.get_keyboard(qh, ());
                                state.data.borrow_mut().keyboard = Some(kb);
                            }
                        }
                    }
                }
                _ => {}
            }

            // keep seat alive
            let _ = seat;
        }
    }

    impl Dispatch<wl_keyboard::WlKeyboard, ()> for App {
        fn event(
            state: &mut Self,
            _kb: &wl_keyboard::WlKeyboard,
            event: wl_keyboard::Event,
            _data: &(),
            _conn: &Connection,
            _qh: &QueueHandle<Self>,
        ) {
            match event {
                wl_keyboard::Event::Keymap { format, fd, size } => {
                    if format != WEnum::Value(wl_keyboard::KeymapFormat::XkbV1) {
                        return;
                    }

                    // fd is OwnedFd; convert to raw and read
                    let raw_fd = fd.into_raw_fd();
                    unsafe {
                        let mut file = File::from_raw_fd(raw_fd);
                        let mut buf = Vec::with_capacity(size as usize);
                        let _ = file.read_to_end(&mut buf);
                        if let Ok(s) = String::from_utf8(buf) {
                            state.data.borrow_mut().keymap_string = Some(s);
                        }
                    }
                }
                _ => {}
            }
        }
    }

    let conn = Connection::connect_to_env().ok()?;
    let mut event_queue: EventQueue<App> = conn.new_event_queue();
    let qh = event_queue.handle();

    let mut app = App::default();

    let _registry = conn.display().get_registry(&qh, ());
    // Roundtrip to get globals and seat
    let _ = event_queue.roundtrip(&mut app);

    // Wait a bit for keyboard keymap. We do a few roundtrips; no busy loop.
    for _ in 0..10 {
        if app.data.borrow().keymap_string.is_some() {
            break;
        }
        let _ = event_queue.roundtrip(&mut app);
    }

    let keymap_str = app.data.borrow().keymap_string.clone()?;
    let context = xkb::Context::new(xkb::CONTEXT_NO_FLAGS);

    let keymap = xkb::Keymap::new_from_string(
        &context,
        keymap_str,
        xkb::KEYMAP_FORMAT_TEXT_V1,
        xkb::KEYMAP_COMPILE_NO_FLAGS,
    )?;
    let mut state = xkb::State::new(&keymap);

    Some(build_chrmap_from_xkb_state(&keymap, &mut state))
}

/// A list of characters that are allowed to be mapped.
// This list should include all characters handled by `shift_transform`.
pub const ALLOWED_CHARACTERS: &[char] = &[
    // Letters
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's',
    't', 'u', 'v', 'w', 'x', 'y', 'z', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    '0', // Digits
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
