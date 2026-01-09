use crossterm::event::{KeyCode, KeyEvent, KeyModifiers, MouseButton, MouseEvent, MouseEventKind};
use ratatui::layout::Rect;

use crate::app::{App, Mode};

pub fn handle_key_event(app: &mut App, key: KeyEvent) {
    match app.mode {
        Mode::Normal => handle_normal_mode(app, key),
        Mode::Search => handle_search_mode(app, key),
        Mode::Filter => handle_filter_mode(app, key),
        Mode::Help => handle_help_mode(app, key),
        Mode::Notes => handle_notes_mode(app, key),
        Mode::TagPicker => handle_tag_picker_mode(app, key),
        Mode::TagManager => handle_tag_manager_mode(app, key),
        Mode::SortPicker => handle_sort_picker_mode(app, key),
        Mode::ConfirmQuit => handle_confirm_quit_mode(app, key),
    }
}

fn handle_normal_mode(app: &mut App, key: KeyEvent) {
    match key.code {
        KeyCode::Char('q') => app.request_quit(),
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
        KeyCode::Char('*') => app.select_all_visible(),
        KeyCode::Char('o') => app.toggle_notes_panel(),
        KeyCode::Char('O') => app.enter_notes_mode(),
        KeyCode::Char('t') => app.enter_tag_picker(),
        KeyCode::Char('T') => app.enter_tag_manager(),
        KeyCode::Char('s') => app.enter_sort_picker(),
        KeyCode::Char('S') => app.clear_sort(),
        KeyCode::Char('>') | KeyCode::Char('.') => app.increase_list_width(),
        KeyCode::Char('<') | KeyCode::Char(',') => app.decrease_list_width(),
        KeyCode::Char('c') => app.cycle_collapse_mode(),
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
        KeyCode::Char('q') => app.request_quit(),
        KeyCode::Char('?') | KeyCode::Esc => app.toggle_help(),
        _ => {}
    }
}

fn handle_confirm_quit_mode(app: &mut App, key: KeyEvent) {
    match key.code {
        KeyCode::Char('y') | KeyCode::Char('Y') => app.confirm_quit_save(),
        KeyCode::Char('n') | KeyCode::Char('N') => app.confirm_quit_discard(),
        KeyCode::Esc => app.cancel_quit(),
        _ => {}
    }
}

fn handle_notes_mode(app: &mut App, key: KeyEvent) {
    match key.code {
        KeyCode::Esc => app.exit_notes_mode(),
        KeyCode::Enter => app.note_newline(),
        KeyCode::Backspace => app.pop_note_char(),
        KeyCode::Char(c) => {
            if key.modifiers.contains(KeyModifiers::CONTROL) {
                if c == 's' {
                    app.exit_notes_mode(); // Save and exit
                }
            } else {
                app.push_note_char(c);
            }
        }
        _ => {}
    }
}

fn handle_tag_picker_mode(app: &mut App, key: KeyEvent) {
    if app.tag_picker.is_creating {
        // Creating a new tag
        match key.code {
            KeyCode::Esc => app.cancel_create_tag(),
            KeyCode::Enter => app.confirm_create_tag(),
            KeyCode::Backspace => app.pop_new_tag_char(),
            KeyCode::Char(c) => app.push_new_tag_char(c),
            _ => {}
        }
    } else {
        // Selecting tags
        match key.code {
            KeyCode::Esc => app.exit_tag_picker(),
            KeyCode::Char('j') | KeyCode::Down => app.tag_picker_next(),
            KeyCode::Char('k') | KeyCode::Up => app.tag_picker_prev(),
            KeyCode::Enter | KeyCode::Char(' ') => app.toggle_selected_tag(),
            KeyCode::Char('a') => app.start_create_tag(), // Add new tag
            _ => {}
        }
    }
}

fn handle_tag_manager_mode(app: &mut App, key: KeyEvent) {
    if app.tag_picker.is_creating {
        // Creating a new tag
        match key.code {
            KeyCode::Esc => app.cancel_create_tag(),
            KeyCode::Enter => app.confirm_create_tag(),
            KeyCode::Backspace => app.pop_new_tag_char(),
            KeyCode::Char(c) => app.push_new_tag_char(c),
            _ => {}
        }
    } else {
        // Managing tags
        match key.code {
            KeyCode::Esc => app.exit_tag_manager(),
            KeyCode::Char('j') | KeyCode::Down => app.tag_picker_next(),
            KeyCode::Char('k') | KeyCode::Up => app.tag_picker_prev(),
            KeyCode::Char('a') => app.start_create_tag(),
            KeyCode::Char('d') | KeyCode::Delete => app.delete_selected_tag(),
            _ => {}
        }
    }
}

fn handle_sort_picker_mode(app: &mut App, key: KeyEvent) {
    match key.code {
        KeyCode::Esc => app.exit_sort_picker(),
        KeyCode::Char('j') | KeyCode::Down => app.sort_picker_next(),
        KeyCode::Char('k') | KeyCode::Up => app.sort_picker_prev(),
        KeyCode::Enter | KeyCode::Char(' ') => app.toggle_selected_sort(),
        KeyCode::Char('c') => {
            app.clear_sort();
            app.exit_sort_picker();
        }
        _ => {}
    }
}

pub fn handle_mouse_event(app: &mut App, mouse: MouseEvent, frame_size: Rect) {
    // Only handle mouse in Normal mode
    if app.mode != Mode::Normal {
        return;
    }

    // Calculate the list panel boundary (accounting for stats bar if visible)
    let show_stats = app.is_filtered_view() && !app.global_stats.fields.is_empty();
    let content_top = if show_stats { 1 } else { 0 };
    let content_height = frame_size.height.saturating_sub(content_top + 1); // -1 for status bar

    let list_width = (frame_size.width as u32 * app.list_panel_width as u32 / 100) as u16;

    match mouse.kind {
        MouseEventKind::ScrollUp => {
            if mouse.column < list_width {
                // Scroll in conversation list
                app.prev_conversation();
            } else {
                // Scroll in message view
                app.scroll_up(3);
            }
        }
        MouseEventKind::ScrollDown => {
            if mouse.column < list_width {
                // Scroll in conversation list
                app.next_conversation();
            } else {
                // Scroll in message view
                app.scroll_down(3);
            }
        }
        MouseEventKind::Down(MouseButton::Left) => {
            // Click to select conversation in list
            if mouse.column < list_width && mouse.row >= content_top && mouse.row < content_top + content_height {
                // Calculate which item was clicked (accounting for borders)
                let click_row = mouse.row.saturating_sub(content_top + 1); // -1 for top border
                app.select_conversation(click_row as usize);
            }
        }
        _ => {}
    }
}
