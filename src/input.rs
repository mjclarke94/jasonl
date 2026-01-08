use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

use crate::app::{App, Mode};

pub fn handle_key_event(app: &mut App, key: KeyEvent) {
    match app.mode {
        Mode::Normal => handle_normal_mode(app, key),
        Mode::Search => handle_search_mode(app, key),
        Mode::Filter => handle_filter_mode(app, key),
        Mode::Help => handle_help_mode(app, key),
    }
}

fn handle_normal_mode(app: &mut App, key: KeyEvent) {
    match key.code {
        KeyCode::Char('q') => app.should_quit = true,
        KeyCode::Char('j') | KeyCode::Down => app.next_conversation(),
        KeyCode::Char('k') | KeyCode::Up => app.prev_conversation(),
        KeyCode::Char('J') | KeyCode::PageDown => app.scroll_down(10),
        KeyCode::Char('K') | KeyCode::PageUp => app.scroll_up(10),
        KeyCode::Char('g') => app.first_conversation(),
        KeyCode::Char('G') => app.last_conversation(),
        KeyCode::Char('/') => app.enter_search_mode(),
        KeyCode::Char('f') => app.enter_filter_mode(),
        KeyCode::Char('F') => app.clear_filter(),
        KeyCode::Char('n') => app.next_match(),
        KeyCode::Char('N') => app.prev_match(),
        KeyCode::Char('y') => { app.copy_as_jsonl(); }
        KeyCode::Char('Y') => { app.copy_as_formatted(); }
        KeyCode::Char('v') => app.toggle_mark(),
        KeyCode::Char('V') => app.clear_marks(),
        KeyCode::Char('?') => app.toggle_help(),
        KeyCode::Esc => {
            if app.filter.is_active() {
                app.clear_filter();
            } else {
                app.clear_search();
            }
        }
        _ => {}
    }
}

fn handle_search_mode(app: &mut App, key: KeyEvent) {
    match key.code {
        KeyCode::Esc => {
            app.exit_search_mode();
        }
        KeyCode::Enter => {
            app.exit_search_mode();
        }
        KeyCode::Backspace => {
            app.pop_search_char();
        }
        KeyCode::Char(c) => {
            if key.modifiers.contains(KeyModifiers::CONTROL) {
                match c {
                    'c' | 'g' => app.clear_search(),
                    _ => {}
                }
            } else {
                app.push_search_char(c);
            }
        }
        _ => {}
    }
}

fn handle_filter_mode(app: &mut App, key: KeyEvent) {
    match key.code {
        KeyCode::Esc => {
            app.exit_filter_mode();
        }
        KeyCode::Enter => {
            app.apply_filter();
        }
        KeyCode::Backspace => {
            app.pop_filter_char();
        }
        KeyCode::Tab => {
            app.complete_filter();
        }
        KeyCode::Char(c) => {
            if key.modifiers.contains(KeyModifiers::CONTROL) {
                match c {
                    'c' | 'g' => app.clear_filter(),
                    _ => {}
                }
            } else {
                app.push_filter_char(c);
            }
        }
        _ => {}
    }
}

fn handle_help_mode(app: &mut App, key: KeyEvent) {
    match key.code {
        KeyCode::Char('q') => app.should_quit = true,
        KeyCode::Char('?') | KeyCode::Esc => app.toggle_help(),
        _ => {}
    }
}
