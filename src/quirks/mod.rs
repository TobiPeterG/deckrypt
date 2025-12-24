mod steam_deck;

use crate::types::SelectedDevice;

pub struct QuirkGuard {
    #[allow(dead_code)]
    guard: Box<dyn DropGuard + Send + 'static>,
}

pub(super) trait DropGuard {}

pub fn apply_for_selected_device(selection: &SelectedDevice) -> Option<QuirkGuard> {
    if let Some(g) = steam_deck::maybe_apply(selection) {
        return Some(QuirkGuard {
            guard: Box::new(g),
        });
    }
    None
}
