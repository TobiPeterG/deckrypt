use crate::types::SelectedDevice;
use hidapi::{HidApi, HidDevice};
use log::{info, warn};
use std::ffi::CString;
use std::time::{Duration, Instant};

use super::DropGuard;

const VALVE_VID: u16 = 0x28de;
const STEAMDECK_PID: u16 = 0x1205;

// 0x81 = ID_CLEAR_DIGITAL_MAPPINGS (disable lizard mode)
const ID_CLEAR_DIGITAL_MAPPINGS: u8 = 0x81;

// Steam Deck “vendor” interface is typically usage_page=0xFFFF
const DECK_VENDOR_USAGE_PAGE: u16 = 0xffff;

pub(super) struct LizardModeKeepAlive {
    _device: HidDevice,
}

impl DropGuard for LizardModeKeepAlive {}

pub(super) fn maybe_apply(selection: &SelectedDevice) -> Option<LizardModeKeepAlive> {
    let (vid, pid, name) = match selection {
        SelectedDevice::Known(k) => (k.vendor_id, k.product_id, k.name.as_str()),
        SelectedDevice::Unknown(u) => (u.vendor_id, u.product_id, u.name.as_str()),
    };

    if !is_steam_deck(vid, pid, name) {
        return None;
    }

    info!("Steam Deck detected; disabling lizard mode.");
    match LizardModeKeepAlive::start() {
        Ok(g) => Some(g),
        Err(e) => {
            warn!("Failed to disable Steam Deck lizard mode: {}", e);
            None
        }
    }
}

fn is_steam_deck(vendor_id: u16, product_id: u16, name: &str) -> bool {
    if vendor_id == VALVE_VID && product_id == STEAMDECK_PID {
        return true;
    }
    let n = name.to_lowercase();
    n.contains("steam deck") || n.contains("neptune")
}

impl LizardModeKeepAlive {
    fn start() -> Result<Self, String> {
        let api = HidApi::new().map_err(|e| e.to_string())?;

        let paths = steamdeck_paths_prefer_vendor(&api);
        if paths.is_empty() {
            return Err("No usable Steam Deck HID paths found (28de:1205).".to_string());
        }

        let dev = open_any_path(&api, &paths).map_err(|e| e.to_string())?;

        for _ in 0..30 {
            blast_disable_by_paths(&api, &paths);
            std::thread::sleep(Duration::from_millis(2));
        }

        std::thread::spawn(move || {
            let mut api = HidApi::new().ok();
            let start = Instant::now();

            loop {
                if api.is_none() {
                    api = HidApi::new().ok();
                }
                if let Some(a) = api.as_ref() {
                    blast_disable_by_paths(a, &paths);
                }

                let elapsed = start.elapsed();
                let sleep_ms = if elapsed < Duration::from_secs(5) { 2 } else { 20 };
                std::thread::sleep(Duration::from_millis(sleep_ms));
            }
        });

        Ok(Self { _device: dev })
    }
}

fn steamdeck_paths_prefer_vendor(api: &HidApi) -> Vec<CString> {
    let mut vendor_paths: Vec<CString> = Vec::new();
    let mut any_paths: Vec<CString> = Vec::new();

    for info in api.device_list() {
        if info.vendor_id() == VALVE_VID && info.product_id() == STEAMDECK_PID {
            let bytes = info.path().to_bytes_with_nul().to_vec();
            let path = match CString::from_vec_with_nul(bytes) {
                Ok(p) => p,
                Err(_) => continue,
            };

            if info.usage_page() == DECK_VENDOR_USAGE_PAGE {
                vendor_paths.push(path);
            } else {
                any_paths.push(path);
            }
        }
    }

    if !vendor_paths.is_empty() {
        vendor_paths
    } else {
        any_paths
    }
}

fn open_any_path(api: &HidApi, paths: &[CString]) -> Result<HidDevice, hidapi::HidError> {
    for p in paths {
        if let Ok(dev) = api.open_path(p.as_c_str()) {
            if send_feature_cmd(&dev, ID_CLEAR_DIGITAL_MAPPINGS).is_ok() {
                return Ok(dev);
            }
        }
    }
    api.open_path(paths[0].as_c_str())
}

fn blast_disable_by_paths(api: &HidApi, paths: &[CString]) {
    for p in paths {
        if let Ok(dev) = api.open_path(p.as_c_str()) {
            let _ = send_feature_cmd(&dev, ID_CLEAR_DIGITAL_MAPPINGS);
        }
    }
}

fn send_feature_cmd(dev: &HidDevice, cmd: u8) -> Result<(), hidapi::HidError> {
    let mut buf = [0u8; 65];
    buf[0] = 0x00;
    buf[1] = cmd;
    dev.send_feature_report(&buf)?;
    Ok(())
}
