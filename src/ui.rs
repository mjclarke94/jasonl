use ratatui::{
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span, Text},
    widgets::{Block, Borders, Clear, List, ListItem, ListState, Paragraph, Wrap},
    Frame,
};

use crate::app::{App, CollapseMode, Mode};
use crate::data::Role;

pub fn draw(frame: &mut Frame, app: &App) {
    // Determine if we need stats bar
    let show_stats = app.is_filtered_view() && !app.global_stats.fields.is_empty();

    let chunks = if show_stats {
        Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(1), // Stats bar
                Constraint::Min(1),    // Main content
                Constraint::Length(1), // Status bar
            ])
            .split(frame.area())
    } else {
        Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Min(1), Constraint::Length(1)])
            .split(frame.area())
    };

    let (stats_area, main_area, status_area) = if show_stats {
        (Some(chunks[0]), chunks[1], chunks[2])
    } else {
        (None, chunks[0], chunks[1])
    };

    // Draw stats bar if active
    if let Some(stats_area) = stats_area {
        draw_stats_bar(frame, app, stats_area);
    }

    let main_chunks = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            Constraint::Percentage(app.list_panel_width),
            Constraint::Percentage(100 - app.list_panel_width),
        ])
        .split(main_area);

    draw_conversation_list(frame, app, main_chunks[0]);

    // Split right panel if notes are visible
    if app.notes.visible {
        let right_chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Percentage(70), Constraint::Percentage(30)])
            .split(main_chunks[1]);
        draw_message_view(frame, app, right_chunks[0]);
        draw_notes_panel(frame, app, right_chunks[1]);
    } else {
        draw_message_view(frame, app, main_chunks[1]);
    }

    draw_status_bar(frame, app, status_area);

    // Draw popups on top
    match app.mode {
        Mode::Help => draw_help_popup(frame),
        Mode::TagPicker => draw_tag_picker_popup(frame, app),
        Mode::TagManager => draw_tag_manager_popup(frame, app),
        Mode::SortPicker => draw_sort_picker_popup(frame, app),
        Mode::ConfirmQuit => draw_confirm_quit_popup(frame),
        _ => {}
    }
}

fn draw_conversation_list(frame: &mut Frame, app: &App, area: Rect) {
    let visible = app.visible_conversations();
    let items: Vec<ListItem> = visible
        .iter()
        .enumerate()
        .map(|(display_idx, (actual_idx, conv))| {
            let preview = conv.preview();
            let is_marked = app.is_marked(*actual_idx);
            let (has_tags, has_note) = app.file_data_map
                .get(&conv.file_hash)
                .map(|fd| (fd.has_tags(conv.source_line), fd.has_note(conv.source_line)))
                .unwrap_or((false, false));

            // Build prefix: mark indicator, then tag/note indicators
            let mark_char = if is_marked { "●" } else { " " };
            let tag_char = if has_tags { "T" } else { " " };
            let note_char = if has_note { "N" } else { " " };
            let text = format!("{}{}{} {}", mark_char, tag_char, note_char, preview);

            let mut style = if display_idx == app.selected_index {
                Style::default()
                    .bg(Color::DarkGray)
                    .add_modifier(Modifier::BOLD)
            } else {
                Style::default()
            };

            if is_marked {
                style = style.fg(Color::Yellow);
            }

            ListItem::new(text).style(style)
        })
        .collect();

    let marked_count = app.marked.len();
    let title = if marked_count > 0 {
        format!(
            " Conversations ({}/{}) [{} marked] ",
            if app.total_visible() > 0 {
                app.selected_index + 1
            } else {
                0
            },
            app.total_visible(),
            marked_count
        )
    } else {
        format!(
            " Conversations ({}/{}) ",
            if app.total_visible() > 0 {
                app.selected_index + 1
            } else {
                0
            },
            app.total_visible()
        )
    };

    let list = List::new(items)
        .block(Block::default().borders(Borders::ALL).title(title))
        .highlight_style(Style::default().bg(Color::DarkGray));

    let mut state = ListState::default();
    state.select(Some(app.selected_index));

    frame.render_stateful_widget(list, area, &mut state);
}

fn draw_message_view(frame: &mut Frame, app: &App, area: Rect) {
    let has_metadata = app
        .selected_conversation()
        .map(|c| !c.metadata.is_empty())
        .unwrap_or(false);

    let (metadata_area, content_area) = if has_metadata {
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Length(3), Constraint::Min(1)])
            .split(area);
        (Some(chunks[0]), chunks[1])
    } else {
        (None, area)
    };

    // Draw metadata header if present
    if let Some(meta_area) = metadata_area {
        if let Some(conv) = app.selected_conversation() {
            let spans: Vec<Span> = conv
                .metadata
                .field_order
                .iter()
                .filter_map(|key| {
                    conv.metadata.fields.get(key).map(|value| {
                        vec![
                            Span::styled(
                                format!("{}:", key),
                                Style::default().fg(Color::DarkGray),
                            ),
                            Span::styled(
                                value.to_string(),
                                Style::default().fg(Color::White).add_modifier(Modifier::BOLD),
                            ),
                            Span::raw("  "),
                        ]
                    })
                })
                .flatten()
                .collect();

            let metadata_line = Line::from(spans);
            let metadata_block = Block::default()
                .borders(Borders::ALL)
                .border_style(Style::default().fg(Color::DarkGray))
                .title(" Metadata ");

            let paragraph = Paragraph::new(metadata_line).block(metadata_block);
            frame.render_widget(paragraph, meta_area);
        }
    }

    // Draw message content
    let content = if let Some(conv) = app.selected_conversation() {
        let mut lines: Vec<Line> = Vec::new();

        for msg in &conv.messages {
            let role_style = match msg.role {
                Role::System => Style::default().fg(Color::Yellow),
                Role::User => Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD),
                Role::Assistant => Style::default().fg(Color::Green),
                Role::Unknown => Style::default().fg(Color::Gray),
            };

            // Check if this message should be collapsed
            let is_collapsed = match app.collapse_mode {
                CollapseMode::Expanded => false,
                CollapseMode::SystemOnly => msg.role == Role::System,
                CollapseMode::AllCollapsed => true,
            };

            if is_collapsed {
                // Show collapsed indicator
                let preview: String = msg.content.chars().take(50).collect();
                let preview = preview.replace('\n', " ");
                let suffix = if msg.content.len() > 50 { "..." } else { "" };
                lines.push(Line::from(vec![
                    Span::styled(format!("[{}]", msg.role), role_style),
                    Span::styled(
                        format!(" ▶ {}{}", preview, suffix),
                        Style::default().fg(Color::DarkGray),
                    ),
                ]));
            } else {
                lines.push(Line::from(Span::styled(
                    format!("[{}]", msg.role),
                    role_style,
                )));

                for content_line in msg.content.lines() {
                    let styled_line = if !app.search.query.is_empty() {
                        highlight_matches(content_line, &app.search.query)
                    } else {
                        Line::from(content_line.to_string())
                    };
                    lines.push(styled_line);
                }
            }

            lines.push(Line::from(""));
        }

        Text::from(lines)
    } else {
        Text::from("No conversation selected")
    };

    let title = if let Some(conv) = app.selected_conversation() {
        let file_indicator = if app.file_data_map.len() > 1 {
            // Show shortened filename when multiple files loaded
            let short_name = std::path::Path::new(&conv.source_file)
                .file_name()
                .and_then(|n| n.to_str())
                .unwrap_or(&conv.source_file);
            format!(" {}:", short_name)
        } else {
            String::new()
        };
        if app.collapse_mode != CollapseMode::Expanded {
            format!("{}Line {} [{}] ", file_indicator, conv.source_line, app.collapse_mode.label())
        } else {
            format!("{}Line {} ", file_indicator, conv.source_line)
        }
    } else {
        " Messages ".to_string()
    };

    let paragraph = Paragraph::new(content)
        .block(Block::default().borders(Borders::ALL).title(title))
        .wrap(Wrap { trim: false })
        .scroll((app.scroll_offset, 0));

    frame.render_widget(paragraph, content_area);
}

fn draw_status_bar(frame: &mut Frame, app: &App, area: Rect) {
    let status = match app.mode {
        Mode::Normal => {
            let mut spans = vec![
                Span::styled(format!(" {} ", app.file_path), Style::default().fg(Color::Cyan)),
            ];

            // Show active filter if present
            if app.filter.is_active() {
                spans.push(Span::raw(" | "));
                spans.push(Span::styled(
                    format!("filter: {} ", app.filter.expression),
                    Style::default().fg(Color::Magenta),
                ));
                spans.push(Span::styled(
                    "(F to clear)",
                    Style::default().fg(Color::DarkGray),
                ));
            }

            // Show active sort if present
            if let Some(ref field) = app.sort.field {
                spans.push(Span::raw(" | "));
                spans.push(Span::styled(
                    format!("sort: {}{} ", field, app.sort.order.symbol()),
                    Style::default().fg(Color::Yellow),
                ));
                spans.push(Span::styled(
                    "(S to clear)",
                    Style::default().fg(Color::DarkGray),
                ));
            }

            spans.push(Span::raw(" | "));
            spans.push(Span::styled(
                " j/k:nav  /:search  f:filter  s:sort  ?:help  q:quit ",
                Style::default().fg(Color::DarkGray),
            ));

            Line::from(spans)
        }
        Mode::Search => {
            let search_prompt = format!(" /{}█ ", app.search.query);
            let match_info = if app.search.matches.is_empty() && !app.search.query.is_empty() {
                " (no matches)".to_string()
            } else if !app.search.matches.is_empty() {
                format!(" ({} matches)", app.search.matches.len())
            } else {
                String::new()
            };
            Line::from(vec![
                Span::styled(search_prompt, Style::default().fg(Color::Yellow)),
                Span::styled(match_info, Style::default().fg(Color::DarkGray)),
            ])
        }
        Mode::Filter => {
            let filter_prompt = format!(" filter: {}█ ", app.filter.expression);
            let match_count = app.total_visible();
            let total = app.conversations.len();
            let info = if let Some(ref err) = app.filter.error {
                Span::styled(format!(" {} ", err), Style::default().fg(Color::Red))
            } else {
                Span::styled(
                    format!(" ({}/{} matching) ", match_count, total),
                    Style::default().fg(Color::DarkGray),
                )
            };
            Line::from(vec![
                Span::styled(filter_prompt, Style::default().fg(Color::Magenta)),
                info,
                Span::styled(
                    " e.g. score>90, field<=50 ",
                    Style::default().fg(Color::DarkGray),
                ),
            ])
        }
        Mode::Help => Line::from(Span::styled(
            " Press ? or Esc to close help ",
            Style::default().fg(Color::Yellow),
        )),
        Mode::Notes => Line::from(vec![
            Span::styled(" NOTES ", Style::default().fg(Color::Black).bg(Color::Green)),
            Span::styled(
                " Type to edit | Esc/Ctrl-S to save and exit ",
                Style::default().fg(Color::DarkGray),
            ),
        ]),
        Mode::TagPicker => {
            if app.tag_picker.is_creating {
                Line::from(vec![
                    Span::styled(" NEW TAG: ", Style::default().fg(Color::Black).bg(Color::Cyan)),
                    Span::styled(
                        format!("{}█", app.tag_picker.new_tag_input),
                        Style::default().fg(Color::Cyan),
                    ),
                    Span::styled(
                        " | Enter to create | Esc to cancel ",
                        Style::default().fg(Color::DarkGray),
                    ),
                ])
            } else {
                Line::from(vec![
                    Span::styled(" TAGS ", Style::default().fg(Color::Black).bg(Color::Cyan)),
                    Span::styled(
                        " j/k:nav  Enter/Space:toggle  a:add  Esc:close ",
                        Style::default().fg(Color::DarkGray),
                    ),
                ])
            }
        }
        Mode::TagManager => {
            if app.tag_picker.is_creating {
                Line::from(vec![
                    Span::styled(" NEW TAG: ", Style::default().fg(Color::Black).bg(Color::Magenta)),
                    Span::styled(
                        format!("{}█", app.tag_picker.new_tag_input),
                        Style::default().fg(Color::Magenta),
                    ),
                    Span::styled(
                        " | Enter to create | Esc to cancel ",
                        Style::default().fg(Color::DarkGray),
                    ),
                ])
            } else {
                Line::from(vec![
                    Span::styled(" TAG MANAGER ", Style::default().fg(Color::Black).bg(Color::Magenta)),
                    Span::styled(
                        " j/k:nav  a:add  d:delete  Esc:close ",
                        Style::default().fg(Color::DarkGray),
                    ),
                ])
            }
        }
        Mode::SortPicker => Line::from(vec![
            Span::styled(" SORT ", Style::default().fg(Color::Black).bg(Color::Yellow)),
            Span::styled(
                " j/k:nav  Enter/Space:toggle  c:clear  Esc:close ",
                Style::default().fg(Color::DarkGray),
            ),
        ]),
        Mode::ConfirmQuit => Line::from(vec![
            Span::styled(" UNSAVED CHANGES ", Style::default().fg(Color::Black).bg(Color::Yellow)),
            Span::styled(
                " y:save and quit | n:discard | Esc:cancel ",
                Style::default().fg(Color::DarkGray),
            ),
        ]),
    };

    let paragraph = Paragraph::new(status).style(Style::default().bg(Color::Black));
    frame.render_widget(paragraph, area);
}

fn draw_stats_bar(frame: &mut Frame, app: &App, area: Rect) {
    let current_stats = app.current_stats();
    let mut spans: Vec<Span> = vec![Span::styled(" Stats: ", Style::default().fg(Color::Cyan))];

    // Get fields in a consistent order (use global_stats field order)
    let mut fields: Vec<_> = app.global_stats.fields.keys().collect();
    fields.sort();

    for field in fields {
        let global_avg = app.global_stats.get_avg(field);
        let current_avg = current_stats.get_avg(field);

        if let (Some(global), Some(current)) = (global_avg, current_avg) {
            let diff = current - global;
            let arrow = if diff > 0.5 {
                ("↑", Color::Green)
            } else if diff < -0.5 {
                ("↓", Color::Red)
            } else {
                ("→", Color::DarkGray)
            };

            spans.push(Span::styled(
                format!("{}:", field),
                Style::default().fg(Color::DarkGray),
            ));
            spans.push(Span::styled(
                format!("{:.1}", current),
                Style::default().fg(Color::White).add_modifier(Modifier::BOLD),
            ));
            spans.push(Span::styled(
                format!(" (avg:{:.1}) ", global),
                Style::default().fg(Color::DarkGray),
            ));
            spans.push(Span::styled(
                format!("{}{:+.1}  ", arrow.0, diff),
                Style::default().fg(arrow.1),
            ));
        }
    }

    let line = Line::from(spans);
    let paragraph = Paragraph::new(line).style(Style::default().bg(Color::Black));
    frame.render_widget(paragraph, area);
}

fn draw_help_popup(frame: &mut Frame) {
    let area = centered_rect(60, 70, frame.area());

    let help_text = vec![
        Line::from(Span::styled(
            "jasonl - JSONL Conversation Viewer",
            Style::default().add_modifier(Modifier::BOLD),
        )),
        Line::from(""),
        Line::from(Span::styled("Navigation", Style::default().fg(Color::Yellow))),
        Line::from("  j / ↓       Next conversation"),
        Line::from("  k / ↑       Previous conversation"),
        Line::from("  g           Go to first"),
        Line::from("  G           Go to last"),
        Line::from("  J / PgDn    Scroll message down"),
        Line::from("  K / PgUp    Scroll message up"),
        Line::from(""),
        Line::from(Span::styled("Search", Style::default().fg(Color::Yellow))),
        Line::from("  /           Start search"),
        Line::from("  n           Next match"),
        Line::from("  N           Previous match"),
        Line::from(""),
        Line::from(Span::styled("Filter", Style::default().fg(Color::Magenta))),
        Line::from("  f           Start filter (e.g. score>90)"),
        Line::from("  F           Clear filter"),
        Line::from("  Tab         Autocomplete field name"),
        Line::from("              Operators: > < >= <= = !="),
        Line::from("              Multiple: score>90,other<50"),
        Line::from("              Tags: tag:name, has:tags, no:tags"),
        Line::from("              Notes: has:notes, no:notes"),
        Line::from(""),
        Line::from(Span::styled("Sort", Style::default().fg(Color::Yellow))),
        Line::from("  s           Open sort picker"),
        Line::from("  S           Clear sort"),
        Line::from(""),
        Line::from(Span::styled("Selection", Style::default().fg(Color::Yellow))),
        Line::from("  v           Toggle mark on conversation"),
        Line::from("  V           Clear all marks"),
        Line::from("  *           Mark all visible conversations"),
        Line::from("  y           Copy as JSONL (marked or current)"),
        Line::from("  Y           Copy as formatted text"),
        Line::from(""),
        Line::from(Span::styled("Notes & Tags", Style::default().fg(Color::Cyan))),
        Line::from("  o           Toggle notes panel"),
        Line::from("  O           Edit notes for conversation"),
        Line::from("  t           Tag picker (applies to marked)"),
        Line::from("  T           Tag manager (create/delete tags)"),
        Line::from(""),
        Line::from(Span::styled("General", Style::default().fg(Color::Yellow))),
        Line::from("  c           Cycle collapse mode"),
        Line::from("  > or .      Increase list panel width"),
        Line::from("  < or ,      Decrease list panel width"),
        Line::from("  Esc         Clear filter/search"),
        Line::from("  ?           Toggle this help"),
        Line::from("  q           Quit"),
    ];

    let block = Block::default()
        .title(" Help ")
        .borders(Borders::ALL)
        .style(Style::default().bg(Color::Black));

    let paragraph = Paragraph::new(help_text).block(block);

    frame.render_widget(Clear, area);
    frame.render_widget(paragraph, area);
}

fn draw_notes_panel(frame: &mut Frame, app: &App, area: Rect) {
    let is_editing = app.mode == Mode::Notes;

    let title = if is_editing {
        " Notes (editing) "
    } else {
        " Notes (O to edit) "
    };

    let border_style = if is_editing {
        Style::default().fg(Color::Green)
    } else {
        Style::default().fg(Color::DarkGray)
    };

    let content = if is_editing {
        format!("{}█", app.notes.content)
    } else if app.notes.content.is_empty() {
        "No notes for this conversation".to_string()
    } else {
        app.notes.content.clone()
    };

    let block = Block::default()
        .title(title)
        .borders(Borders::ALL)
        .border_style(border_style);

    let paragraph = Paragraph::new(content)
        .block(block)
        .wrap(Wrap { trim: false });

    frame.render_widget(paragraph, area);
}

fn draw_tag_picker_popup(frame: &mut Frame, app: &App) {
    let area = centered_rect(40, 50, frame.area());

    let conv = app.selected_conversation();
    let current_line = conv.map(|c| c.source_line).unwrap_or(0);
    let current_tag_ids = conv
        .and_then(|c| app.file_data_map.get(&c.file_hash))
        .map(|fd| fd.get_tag_ids(current_line))
        .unwrap_or_default();

    let items: Vec<ListItem> = app
        .tag_picker
        .available_tags
        .iter()
        .enumerate()
        .map(|(idx, tag)| {
            let is_selected = idx == app.tag_picker.selected_index;
            let is_applied = current_tag_ids.contains(&tag.id);

            let checkbox = if is_applied { "[x]" } else { "[ ]" };
            let text = format!("{} {}", checkbox, tag.name);

            let style = if is_selected {
                Style::default()
                    .bg(Color::DarkGray)
                    .add_modifier(Modifier::BOLD)
            } else {
                Style::default()
            };

            ListItem::new(text).style(style)
        })
        .collect();

    let title = format!(" Tags ({}) ", app.tag_picker.available_tags.len());
    let block = Block::default()
        .title(title)
        .borders(Borders::ALL)
        .style(Style::default().bg(Color::Black));

    let list = List::new(items).block(block);

    frame.render_widget(Clear, area);
    frame.render_widget(list, area);
}

fn draw_tag_manager_popup(frame: &mut Frame, app: &App) {
    let area = centered_rect(40, 50, frame.area());

    let items: Vec<ListItem> = app
        .tag_picker
        .available_tags
        .iter()
        .enumerate()
        .map(|(idx, tag)| {
            let is_selected = idx == app.tag_picker.selected_index;
            let text = format!("  {}", tag.name);

            let style = if is_selected {
                Style::default()
                    .bg(Color::DarkGray)
                    .add_modifier(Modifier::BOLD)
            } else {
                Style::default()
            };

            ListItem::new(text).style(style)
        })
        .collect();

    let title = format!(" Tag Manager ({}) ", app.tag_picker.available_tags.len());
    let block = Block::default()
        .title(title)
        .borders(Borders::ALL)
        .border_style(Style::default().fg(Color::Magenta))
        .style(Style::default().bg(Color::Black));

    let list = List::new(items).block(block);

    frame.render_widget(Clear, area);
    frame.render_widget(list, area);
}

fn draw_sort_picker_popup(frame: &mut Frame, app: &App) {
    let area = centered_rect(50, 50, frame.area());

    let items: Vec<ListItem> = app
        .metadata_fields
        .iter()
        .enumerate()
        .map(|(idx, field)| {
            let is_selected = idx == app.sort.selected_index;
            let is_active = app.sort.field.as_ref() == Some(field);

            let symbol = if is_active {
                app.sort.order.symbol()
            } else {
                " "
            };
            let text = format!(" {} {}", symbol, field);

            let style = if is_selected {
                Style::default()
                    .bg(Color::DarkGray)
                    .add_modifier(Modifier::BOLD)
            } else if is_active {
                Style::default().fg(Color::Yellow)
            } else {
                Style::default()
            };

            ListItem::new(text).style(style)
        })
        .collect();

    let title = format!(" Sort by ({}) ", app.metadata_fields.len());
    let block = Block::default()
        .title(title)
        .borders(Borders::ALL)
        .border_style(Style::default().fg(Color::Yellow))
        .style(Style::default().bg(Color::Black));

    let list = List::new(items).block(block);

    frame.render_widget(Clear, area);
    frame.render_widget(list, area);
}

fn draw_confirm_quit_popup(frame: &mut Frame) {
    let area = centered_rect(50, 20, frame.area());

    let text = vec![
        Line::from(""),
        Line::from(Span::styled(
            "You have unsaved changes.",
            Style::default().add_modifier(Modifier::BOLD),
        )),
        Line::from(""),
        Line::from("Save changes before quitting?"),
        Line::from(""),
        Line::from(vec![
            Span::styled("  y", Style::default().fg(Color::Green).add_modifier(Modifier::BOLD)),
            Span::raw(" - Save and quit"),
        ]),
        Line::from(vec![
            Span::styled("  n", Style::default().fg(Color::Red).add_modifier(Modifier::BOLD)),
            Span::raw(" - Discard and quit"),
        ]),
        Line::from(vec![
            Span::styled("Esc", Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)),
            Span::raw(" - Cancel"),
        ]),
    ];

    let block = Block::default()
        .title(" Unsaved Changes ")
        .borders(Borders::ALL)
        .border_style(Style::default().fg(Color::Yellow))
        .style(Style::default().bg(Color::Black));

    let paragraph = Paragraph::new(text).block(block);

    frame.render_widget(Clear, area);
    frame.render_widget(paragraph, area);
}

fn centered_rect(percent_x: u16, percent_y: u16, r: Rect) -> Rect {
    let popup_layout = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Percentage((100 - percent_y) / 2),
            Constraint::Percentage(percent_y),
            Constraint::Percentage((100 - percent_y) / 2),
        ])
        .split(r);

    Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            Constraint::Percentage((100 - percent_x) / 2),
            Constraint::Percentage(percent_x),
            Constraint::Percentage((100 - percent_x) / 2),
        ])
        .split(popup_layout[1])[1]
}

fn highlight_matches(text: &str, query: &str) -> Line<'static> {
    if query.is_empty() {
        return Line::from(text.to_string());
    }

    let query_lower = query.to_lowercase();
    let text_lower = text.to_lowercase();
    let mut spans = Vec::new();
    let mut last_end = 0;

    for (start, _) in text_lower.match_indices(&query_lower) {
        if start > last_end {
            spans.push(Span::raw(text[last_end..start].to_string()));
        }
        spans.push(Span::styled(
            text[start..start + query.len()].to_string(),
            Style::default()
                .bg(Color::Yellow)
                .fg(Color::Black)
                .add_modifier(Modifier::BOLD),
        ));
        last_end = start + query.len();
    }

    if last_end < text.len() {
        spans.push(Span::raw(text[last_end..].to_string()));
    }

    Line::from(spans)
}
