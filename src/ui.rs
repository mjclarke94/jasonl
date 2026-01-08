use ratatui::{
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span, Text},
    widgets::{Block, Borders, Clear, List, ListItem, ListState, Paragraph, Wrap},
    Frame,
};

use crate::app::{App, Mode};
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
        .constraints([Constraint::Percentage(25), Constraint::Percentage(75)])
        .split(main_area);

    draw_conversation_list(frame, app, main_chunks[0]);
    draw_message_view(frame, app, main_chunks[1]);
    draw_status_bar(frame, app, status_area);

    if app.mode == Mode::Help {
        draw_help_popup(frame);
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

            let prefix = if is_marked { "● " } else { "  " };
            let text = format!("{}{}", prefix, preview);

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

            lines.push(Line::from(""));
        }

        Text::from(lines)
    } else {
        Text::from("No conversation selected")
    };

    let title = if let Some(conv) = app.selected_conversation() {
        format!(" Line {} ", conv.source_line)
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

            spans.push(Span::raw(" | "));
            spans.push(Span::styled(
                " j/k:nav  /:search  f:filter  ?:help  q:quit ",
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
        Line::from(Span::styled("Filter (metadata)", Style::default().fg(Color::Magenta))),
        Line::from("  f           Start filter (e.g. score>90)"),
        Line::from("  F           Clear filter"),
        Line::from("  Tab         Autocomplete field name"),
        Line::from("              Operators: > < >= <= = !="),
        Line::from("              Multiple: score>90,other<50"),
        Line::from(""),
        Line::from(Span::styled("Clipboard", Style::default().fg(Color::Yellow))),
        Line::from("  v           Toggle mark on conversation"),
        Line::from("  V           Clear all marks"),
        Line::from("  y           Copy as JSONL (marked or current)"),
        Line::from("  Y           Copy as formatted text"),
        Line::from(""),
        Line::from(Span::styled("General", Style::default().fg(Color::Yellow))),
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
