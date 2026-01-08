use std::collections::HashMap;
use std::io::{self, Write};

use arboard::Clipboard;
use base64::Engine;

use crate::data::Conversation;
use crate::db::{Database, Tag};

/// Check if we're running in an SSH session
fn is_ssh_session() -> bool {
    std::env::var("SSH_TTY").is_ok() || std::env::var("SSH_CLIENT").is_ok()
}

/// Copy text to clipboard using OSC 52 escape sequence (works over SSH in supported terminals)
fn copy_osc52(text: &str) -> bool {
    let encoded = base64::engine::general_purpose::STANDARD.encode(text);
    // OSC 52 sequence: \x1b]52;c;<base64>\x07
    let sequence = format!("\x1b]52;c;{}\x07", encoded);

    let mut stdout = io::stdout();
    if stdout.write_all(sequence.as_bytes()).is_ok() {
        stdout.flush().is_ok()
    } else {
        false
    }
}

/// Copy text to clipboard, using OSC 52 for SSH sessions, arboard otherwise
fn copy_to_clipboard(text: &str) -> bool {
    if is_ssh_session() {
        copy_osc52(text)
    } else {
        Clipboard::new()
            .and_then(|mut cb| cb.set_text(text.to_string()))
            .is_ok()
    }
}

#[derive(Debug, Clone, Default)]
pub struct FieldStats {
    pub count: usize,
    pub sum: f64,
    pub min: f64,
    pub max: f64,
}

impl FieldStats {
    pub fn new() -> Self {
        Self {
            count: 0,
            sum: 0.0,
            min: f64::MAX,
            max: f64::MIN,
        }
    }

    pub fn add(&mut self, value: f64) {
        self.count += 1;
        self.sum += value;
        self.min = self.min.min(value);
        self.max = self.max.max(value);
    }

    pub fn avg(&self) -> Option<f64> {
        if self.count > 0 {
            Some(self.sum / self.count as f64)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct MetadataStats {
    pub fields: HashMap<String, FieldStats>,
}

impl MetadataStats {
    pub fn from_conversations<'a>(convs: impl Iterator<Item = &'a Conversation>) -> Self {
        let mut stats = Self::default();

        for conv in convs {
            for (key, value) in &conv.metadata.fields {
                if let Ok(num) = value.parse::<f64>() {
                    stats
                        .fields
                        .entry(key.clone())
                        .or_insert_with(FieldStats::new)
                        .add(num);
                }
            }
        }

        stats
    }

    pub fn get_avg(&self, field: &str) -> Option<f64> {
        self.fields.get(field).and_then(|s| s.avg())
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Mode {
    Normal,
    Search,
    Filter,
    Help,
    Notes,      // Editing notes for current conversation
    TagPicker,  // Selecting tags for current conversation
    TagManager, // Creating/deleting tags
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FilterOp {
    Gt,
    Lt,
    Gte,
    Lte,
    Eq,
    Neq,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FilterValue {
    Number(f64),
    Bool(bool),
}

#[derive(Debug, Clone)]
pub struct Filter {
    pub field: String,
    pub op: FilterOp,
    pub value: FilterValue,
}

impl Filter {
    pub fn parse(expr: &str) -> Option<Self> {
        let expr = expr.trim();

        // Try each operator (longer ones first to avoid partial matches)
        let operators = [
            (">=", FilterOp::Gte),
            ("<=", FilterOp::Lte),
            ("!=", FilterOp::Neq),
            (">", FilterOp::Gt),
            ("<", FilterOp::Lt),
            ("=", FilterOp::Eq),
        ];

        for (op_str, op) in operators {
            if let Some(pos) = expr.find(op_str) {
                let field = expr[..pos].trim().to_string();
                let value_str = expr[pos + op_str.len()..].trim();

                if field.is_empty() {
                    return None;
                }

                // Try parsing as number first
                if let Ok(value) = value_str.parse::<f64>() {
                    return Some(Filter {
                        field,
                        op,
                        value: FilterValue::Number(value),
                    });
                }

                // Try parsing as boolean
                match value_str.to_lowercase().as_str() {
                    "true" => {
                        return Some(Filter {
                            field,
                            op,
                            value: FilterValue::Bool(true),
                        });
                    }
                    "false" => {
                        return Some(Filter {
                            field,
                            op,
                            value: FilterValue::Bool(false),
                        });
                    }
                    _ => {}
                }
            }
        }
        None
    }

    pub fn matches(&self, conv: &Conversation) -> bool {
        let Some(field_value) = conv.metadata.fields.get(&self.field) else {
            return false;
        };

        match &self.value {
            FilterValue::Number(filter_num) => {
                let Ok(num_value) = field_value.parse::<f64>() else {
                    return false;
                };

                match self.op {
                    FilterOp::Gt => num_value > *filter_num,
                    FilterOp::Lt => num_value < *filter_num,
                    FilterOp::Gte => num_value >= *filter_num,
                    FilterOp::Lte => num_value <= *filter_num,
                    FilterOp::Eq => (num_value - filter_num).abs() < f64::EPSILON,
                    FilterOp::Neq => (num_value - filter_num).abs() >= f64::EPSILON,
                }
            }
            FilterValue::Bool(filter_bool) => {
                let field_bool = match field_value.to_lowercase().as_str() {
                    "true" => true,
                    "false" => false,
                    _ => return false,
                };

                match self.op {
                    FilterOp::Eq => field_bool == *filter_bool,
                    FilterOp::Neq => field_bool != *filter_bool,
                    // Other operators don't make sense for booleans
                    _ => false,
                }
            }
        }
    }
}

/// Special filters for tags and notes
#[derive(Debug, Clone)]
pub enum SpecialFilter {
    HasTag(String),    // tag:name - has specific tag
    HasAnyTag,         // has:tags - has any tag
    HasNote,           // has:notes - has a note
    NoTags,            // no:tags - has no tags
    NoNote,            // no:notes - has no note
}

impl SpecialFilter {
    pub fn parse(expr: &str) -> Option<Self> {
        let expr = expr.trim().to_lowercase();

        if let Some(tag_name) = expr.strip_prefix("tag:") {
            let tag_name = tag_name.trim();
            if !tag_name.is_empty() {
                return Some(SpecialFilter::HasTag(tag_name.to_string()));
            }
        }

        match expr.as_str() {
            "has:tags" | "has:tag" => Some(SpecialFilter::HasAnyTag),
            "has:notes" | "has:note" => Some(SpecialFilter::HasNote),
            "no:tags" | "no:tag" => Some(SpecialFilter::NoTags),
            "no:notes" | "no:note" => Some(SpecialFilter::NoNote),
            _ => None,
        }
    }
}

#[derive(Debug, Default)]
pub struct FilterState {
    pub expression: String,
    pub filters: Vec<Filter>,
    pub special_filters: Vec<SpecialFilter>,
    pub error: Option<String>,
    // Completion state
    completion_prefix: String,
    completion_matches: Vec<String>,
    completion_index: usize,
}

impl FilterState {
    pub fn is_active(&self) -> bool {
        !self.filters.is_empty() || !self.special_filters.is_empty()
    }

    pub fn update(&mut self) {
        self.filters.clear();
        self.special_filters.clear();
        self.error = None;

        if self.expression.trim().is_empty() {
            return;
        }

        // Split by AND (case-insensitive) or comma
        let parts: Vec<&str> = self.expression
            .split([',', '&'])
            .map(|s| s.trim())
            .filter(|s| !s.is_empty() && *s != "AND" && *s != "and")
            .collect();

        for part in parts {
            // Try special filter first
            if let Some(special) = SpecialFilter::parse(part) {
                self.special_filters.push(special);
            } else if let Some(filter) = Filter::parse(part) {
                self.filters.push(filter);
            } else if !part.is_empty() {
                self.error = Some(format!("Invalid filter: {}", part));
                return;
            }
        }
    }

    pub fn matches(&self, conv: &Conversation) -> bool {
        if self.filters.is_empty() && self.special_filters.is_empty() {
            return true;
        }
        self.filters.iter().all(|f| f.matches(conv))
        // Note: special filters are checked separately in App::filter_matches
    }

    pub fn has_special_filters(&self) -> bool {
        !self.special_filters.is_empty()
    }

    /// Reset completion state (call when expression changes via typing)
    pub fn reset_completion(&mut self) {
        self.completion_prefix.clear();
        self.completion_matches.clear();
        self.completion_index = 0;
    }

    /// Try to complete the current partial field name. Returns true if completion happened.
    pub fn try_complete(&mut self, available_fields: &[String]) -> bool {
        // Find the start of the current field name (after last comma or &)
        let last_sep = self.expression.rfind([',', '&']).map(|i| i + 1).unwrap_or(0);
        let current_part = self.expression[last_sep..].trim_start();

        // Find operator position if any
        let op_pos = current_part.find(['>', '<', '=', '!']);

        // Extract the partial field name
        let partial = if let Some(pos) = op_pos {
            &current_part[..pos]
        } else {
            current_part
        };

        if partial.is_empty() {
            return false;
        }

        // Check if we're continuing a previous completion cycle
        let is_cycling = !self.completion_matches.is_empty()
            && self.completion_matches.contains(&partial.to_string());

        if is_cycling {
            // Cycle to next match
            self.completion_index = (self.completion_index + 1) % self.completion_matches.len();
        } else {
            // New completion - find all matches
            self.completion_prefix = partial.to_string();
            self.completion_matches = available_fields
                .iter()
                .filter(|f| f.starts_with(partial))
                .cloned()
                .collect();
            self.completion_index = 0;
        }

        if self.completion_matches.is_empty() {
            return false;
        }

        // Replace the partial with the completion
        let completion = &self.completion_matches[self.completion_index];
        let prefix_in_expr = &self.expression[..last_sep];
        let suffix = if let Some(pos) = op_pos {
            &current_part[pos..]
        } else {
            ""
        };

        let leading_space = if last_sep > 0 && !prefix_in_expr.is_empty() { " " } else { "" };
        self.expression = format!("{}{}{}{}", prefix_in_expr, leading_space, completion, suffix);

        true
    }
}

#[derive(Debug, Default)]
pub struct SearchState {
    pub query: String,
    pub matches: Vec<usize>,
    pub current_match: usize,
}

/// State for notes editing
#[derive(Debug, Default)]
pub struct NotesState {
    pub content: String,
    pub visible: bool,
}

/// State for tag picker
#[derive(Debug, Default)]
pub struct TagPickerState {
    pub available_tags: Vec<Tag>,
    pub selected_index: usize,
    pub new_tag_input: String,
    pub is_creating: bool, // If true, we're in "create new tag" mode
}

pub struct App {
    pub conversations: Vec<Conversation>,
    pub selected_index: usize,
    pub scroll_offset: u16,
    pub mode: Mode,
    pub file_path: String,
    pub file_hash: String,
    pub search: SearchState,
    pub filter: FilterState,
    pub should_quit: bool,
    // Cached indices of conversations matching current filter
    filtered_indices: Vec<usize>,
    // Global statistics for all conversations
    pub global_stats: MetadataStats,
    // All unique metadata field names for autocompletion
    pub metadata_fields: Vec<String>,
    // Multi-select: indices of marked conversations
    pub marked: std::collections::HashSet<usize>,
    // Database for notes and tags
    pub db: Database,
    // Notes state
    pub notes: NotesState,
    // Tag picker state
    pub tag_picker: TagPickerState,
    // Cache of which lines have tags/notes (for display indicators)
    pub tagged_lines: std::collections::HashSet<usize>,
    pub noted_lines: std::collections::HashSet<usize>,
}

impl App {
    pub fn new(conversations: Vec<Conversation>, file_path: String, file_hash: String, db: Database) -> Self {
        let filtered_indices: Vec<usize> = (0..conversations.len()).collect();
        let global_stats = MetadataStats::from_conversations(conversations.iter());

        // Collect unique metadata field names for autocompletion
        let mut metadata_fields: Vec<String> = conversations
            .iter()
            .flat_map(|c| c.metadata.fields.keys().cloned())
            .collect::<std::collections::HashSet<_>>()
            .into_iter()
            .collect();
        metadata_fields.sort();

        // Load cached tag/note indicators
        let tagged_lines = db.get_tagged_lines(&file_hash).unwrap_or_default();
        let noted_lines = db.get_noted_lines(&file_hash).unwrap_or_default();

        Self {
            conversations,
            selected_index: 0,
            scroll_offset: 0,
            mode: Mode::Normal,
            file_path,
            file_hash,
            search: SearchState::default(),
            filter: FilterState::default(),
            should_quit: false,
            filtered_indices,
            global_stats,
            metadata_fields,
            marked: std::collections::HashSet::new(),
            db,
            notes: NotesState::default(),
            tag_picker: TagPickerState::default(),
            tagged_lines,
            noted_lines,
        }
    }

    /// Calculate statistics for currently visible conversations
    pub fn current_stats(&self) -> MetadataStats {
        let visible_convs = self.effective_indices();
        MetadataStats::from_conversations(visible_convs.iter().map(|&i| &self.conversations[i]))
    }

    /// Check if we're viewing a subset (filter or search active)
    pub fn is_filtered_view(&self) -> bool {
        !self.search.query.is_empty() || self.filter.is_active()
    }

    fn update_filtered_indices(&mut self) {
        self.filtered_indices = self
            .conversations
            .iter()
            .enumerate()
            .filter(|(_, c)| {
                // Check regular metadata filters
                if !self.filter.matches(c) {
                    return false;
                }

                // Check special filters
                for special in &self.filter.special_filters {
                    let matches = match special {
                        SpecialFilter::HasAnyTag => self.tagged_lines.contains(&c.source_line),
                        SpecialFilter::HasNote => self.noted_lines.contains(&c.source_line),
                        SpecialFilter::NoTags => !self.tagged_lines.contains(&c.source_line),
                        SpecialFilter::NoNote => !self.noted_lines.contains(&c.source_line),
                        SpecialFilter::HasTag(name) => {
                            // Check if conversation has the specific tag
                            if let Ok(tags) = self.db.get_conversation_tags(&self.file_hash, c.source_line) {
                                tags.iter().any(|t| t.name.to_lowercase() == *name)
                            } else {
                                false
                            }
                        }
                    };
                    if !matches {
                        return false;
                    }
                }

                true
            })
            .map(|(i, _)| i)
            .collect();
    }

    pub fn selected_conversation(&self) -> Option<&Conversation> {
        let indices = self.effective_indices();
        indices
            .get(self.selected_index)
            .and_then(|&i| self.conversations.get(i))
    }

    fn effective_indices(&self) -> Vec<usize> {
        if self.search.query.is_empty() {
            self.filtered_indices.clone()
        } else {
            // Intersection of filtered and search matches
            self.search
                .matches
                .iter()
                .filter(|i| self.filtered_indices.contains(i))
                .copied()
                .collect()
        }
    }

    pub fn visible_conversations(&self) -> Vec<(usize, &Conversation)> {
        self.effective_indices()
            .iter()
            .map(|&i| (i, &self.conversations[i]))
            .collect()
    }

    pub fn total_visible(&self) -> usize {
        self.effective_indices().len()
    }

    pub fn next_conversation(&mut self) {
        let total = self.total_visible();
        if total > 0 && self.selected_index < total - 1 {
            self.selected_index += 1;
            self.scroll_offset = 0;
            if self.notes.visible {
                self.load_current_note();
            }
        }
    }

    pub fn prev_conversation(&mut self) {
        if self.selected_index > 0 {
            self.selected_index -= 1;
            self.scroll_offset = 0;
            if self.notes.visible {
                self.load_current_note();
            }
        }
    }

    pub fn first_conversation(&mut self) {
        self.selected_index = 0;
        self.scroll_offset = 0;
        if self.notes.visible {
            self.load_current_note();
        }
    }

    pub fn last_conversation(&mut self) {
        let total = self.total_visible();
        if total > 0 {
            self.selected_index = total - 1;
            self.scroll_offset = 0;
            if self.notes.visible {
                self.load_current_note();
            }
        }
    }

    pub fn scroll_down(&mut self, amount: u16) {
        self.scroll_offset = self.scroll_offset.saturating_add(amount);
    }

    pub fn scroll_up(&mut self, amount: u16) {
        self.scroll_offset = self.scroll_offset.saturating_sub(amount);
    }

    // Search methods
    pub fn enter_search_mode(&mut self) {
        self.mode = Mode::Search;
        self.search.query.clear();
    }

    pub fn exit_search_mode(&mut self) {
        self.mode = Mode::Normal;
    }

    pub fn clear_search(&mut self) {
        self.search.query.clear();
        self.search.matches.clear();
        self.search.current_match = 0;
        self.selected_index = 0;
        self.mode = Mode::Normal;
    }

    pub fn update_search(&mut self) {
        if self.search.query.is_empty() {
            self.search.matches.clear();
            return;
        }

        self.search.matches = self
            .conversations
            .iter()
            .enumerate()
            .filter(|(_, c)| c.contains(&self.search.query))
            .map(|(i, _)| i)
            .collect();

        self.selected_index = 0;
        self.search.current_match = 0;
    }

    pub fn next_match(&mut self) {
        let total = self.total_visible();
        if total == 0 {
            return;
        }
        self.selected_index = (self.selected_index + 1) % total;
        self.scroll_offset = 0;
    }

    pub fn prev_match(&mut self) {
        let total = self.total_visible();
        if total == 0 {
            return;
        }
        self.selected_index = if self.selected_index == 0 {
            total - 1
        } else {
            self.selected_index - 1
        };
        self.scroll_offset = 0;
    }

    pub fn push_search_char(&mut self, c: char) {
        self.search.query.push(c);
        self.update_search();
    }

    pub fn pop_search_char(&mut self) {
        self.search.query.pop();
        self.update_search();
    }

    // Filter methods
    pub fn enter_filter_mode(&mut self) {
        self.mode = Mode::Filter;
    }

    pub fn exit_filter_mode(&mut self) {
        self.mode = Mode::Normal;
    }

    pub fn clear_filter(&mut self) {
        self.filter.expression.clear();
        self.filter.filters.clear();
        self.filter.error = None;
        self.update_filtered_indices();
        self.selected_index = 0;
        self.mode = Mode::Normal;
    }

    pub fn apply_filter(&mut self) {
        self.filter.update();
        self.update_filtered_indices();
        self.selected_index = 0;
        self.scroll_offset = 0;
        self.mode = Mode::Normal;
    }

    pub fn push_filter_char(&mut self, c: char) {
        self.filter.expression.push(c);
        self.filter.reset_completion();
        // Live preview - update as user types
        self.filter.update();
        self.update_filtered_indices();
        if self.selected_index >= self.total_visible() {
            self.selected_index = 0;
        }
    }

    pub fn pop_filter_char(&mut self) {
        self.filter.expression.pop();
        self.filter.reset_completion();
        self.filter.update();
        self.update_filtered_indices();
        if self.selected_index >= self.total_visible() {
            self.selected_index = 0;
        }
    }

    pub fn complete_filter(&mut self) {
        if self.filter.try_complete(&self.metadata_fields) {
            // Update filter after completion
            self.filter.update();
            self.update_filtered_indices();
            if self.selected_index >= self.total_visible() {
                self.selected_index = 0;
            }
        }
    }

    // Multi-select
    pub fn toggle_mark(&mut self) {
        let indices = self.effective_indices();
        if let Some(&actual_index) = indices.get(self.selected_index) {
            if self.marked.contains(&actual_index) {
                self.marked.remove(&actual_index);
            } else {
                self.marked.insert(actual_index);
            }
        }
    }

    pub fn clear_marks(&mut self) {
        self.marked.clear();
    }

    /// Mark all currently visible conversations
    pub fn select_all_visible(&mut self) {
        let indices = self.effective_indices();
        for idx in indices {
            self.marked.insert(idx);
        }
    }

    /// Check if a conversation (by actual index) is marked
    pub fn is_marked(&self, actual_index: usize) -> bool {
        self.marked.contains(&actual_index)
    }

    // Clipboard
    pub fn copy_as_jsonl(&self) -> bool {
        // If there are marked conversations, copy all of them; otherwise copy current
        let text = if self.marked.is_empty() {
            self.selected_conversation()
                .map(|c| c.to_jsonl())
                .unwrap_or_default()
        } else {
            let mut indices: Vec<_> = self.marked.iter().copied().collect();
            indices.sort();
            indices
                .iter()
                .filter_map(|&i| self.conversations.get(i))
                .map(|c| c.to_jsonl())
                .collect::<Vec<_>>()
                .join("\n")
        };

        if text.is_empty() {
            return false;
        }

        copy_to_clipboard(&text)
    }

    pub fn copy_as_formatted(&self) -> bool {
        // If there are marked conversations, copy all of them; otherwise copy current
        let text = if self.marked.is_empty() {
            self.selected_conversation()
                .map(|c| c.to_formatted())
                .unwrap_or_default()
        } else {
            let mut indices: Vec<_> = self.marked.iter().copied().collect();
            indices.sort();
            indices
                .iter()
                .filter_map(|&i| self.conversations.get(i))
                .map(|c| c.to_formatted())
                .collect::<Vec<_>>()
                .join("\n\n---\n\n")
        };

        if text.is_empty() {
            return false;
        }

        copy_to_clipboard(&text)
    }

    // Notes
    pub fn toggle_notes_panel(&mut self) {
        self.notes.visible = !self.notes.visible;
        if self.notes.visible {
            self.load_current_note();
        }
    }

    pub fn enter_notes_mode(&mut self) {
        self.notes.visible = true;
        self.load_current_note();
        self.mode = Mode::Notes;
    }

    pub fn exit_notes_mode(&mut self) {
        self.save_current_note();
        self.mode = Mode::Normal;
    }

    fn current_line_number(&self) -> Option<usize> {
        self.selected_conversation().map(|c| c.source_line)
    }

    fn load_current_note(&mut self) {
        if let Some(line) = self.current_line_number() {
            self.notes.content = self
                .db
                .get_note(&self.file_hash, line)
                .ok()
                .flatten()
                .map(|n| n.content)
                .unwrap_or_default();
        }
    }

    fn save_current_note(&mut self) {
        if let Some(line) = self.current_line_number() {
            let _ = self.db.set_note(&self.file_hash, line, &self.notes.content);
            // Update cache
            if self.notes.content.trim().is_empty() {
                self.noted_lines.remove(&line);
            } else {
                self.noted_lines.insert(line);
            }
        }
    }

    pub fn push_note_char(&mut self, c: char) {
        self.notes.content.push(c);
    }

    pub fn pop_note_char(&mut self) {
        self.notes.content.pop();
    }

    pub fn note_newline(&mut self) {
        self.notes.content.push('\n');
    }

    // Tags
    pub fn enter_tag_picker(&mut self) {
        self.tag_picker.available_tags = self.db.get_all_tags().unwrap_or_default();
        self.tag_picker.selected_index = 0;
        self.tag_picker.is_creating = false;
        self.tag_picker.new_tag_input.clear();
        self.mode = Mode::TagPicker;
    }

    pub fn exit_tag_picker(&mut self) {
        self.mode = Mode::Normal;
    }

    pub fn enter_tag_manager(&mut self) {
        self.tag_picker.available_tags = self.db.get_all_tags().unwrap_or_default();
        self.tag_picker.selected_index = 0;
        self.tag_picker.is_creating = false;
        self.tag_picker.new_tag_input.clear();
        self.mode = Mode::TagManager;
    }

    pub fn exit_tag_manager(&mut self) {
        self.mode = Mode::Normal;
    }

    pub fn tag_picker_next(&mut self) {
        let len = self.tag_picker.available_tags.len();
        if len > 0 {
            self.tag_picker.selected_index = (self.tag_picker.selected_index + 1) % len;
        }
    }

    pub fn tag_picker_prev(&mut self) {
        let len = self.tag_picker.available_tags.len();
        if len > 0 {
            self.tag_picker.selected_index = if self.tag_picker.selected_index == 0 {
                len - 1
            } else {
                self.tag_picker.selected_index - 1
            };
        }
    }

    pub fn toggle_selected_tag(&mut self) {
        let Some(tag) = self.tag_picker.available_tags.get(self.tag_picker.selected_index).cloned() else {
            return;
        };

        // Get line numbers to operate on: marked conversations or current
        let lines: Vec<usize> = if self.marked.is_empty() {
            self.current_line_number().into_iter().collect()
        } else {
            self.marked
                .iter()
                .filter_map(|&idx| self.conversations.get(idx).map(|c| c.source_line))
                .collect()
        };

        for line in lines {
            if let Ok(added) = self.db.toggle_conversation_tag(&self.file_hash, line, tag.id) {
                // Update cache
                if added {
                    self.tagged_lines.insert(line);
                } else {
                    // Check if any tags remain
                    if let Ok(tags) = self.db.get_conversation_tag_ids(&self.file_hash, line) {
                        if tags.is_empty() {
                            self.tagged_lines.remove(&line);
                        }
                    }
                }
            }
        }
    }

    pub fn get_current_tags(&self) -> Vec<Tag> {
        if let Some(line) = self.current_line_number() {
            self.db.get_conversation_tags(&self.file_hash, line).unwrap_or_default()
        } else {
            vec![]
        }
    }

    pub fn get_current_tag_ids(&self) -> std::collections::HashSet<i64> {
        if let Some(line) = self.current_line_number() {
            self.db.get_conversation_tag_ids(&self.file_hash, line).unwrap_or_default()
        } else {
            std::collections::HashSet::new()
        }
    }

    pub fn start_create_tag(&mut self) {
        self.tag_picker.is_creating = true;
        self.tag_picker.new_tag_input.clear();
    }

    pub fn cancel_create_tag(&mut self) {
        self.tag_picker.is_creating = false;
        self.tag_picker.new_tag_input.clear();
    }

    pub fn push_new_tag_char(&mut self, c: char) {
        self.tag_picker.new_tag_input.push(c);
    }

    pub fn pop_new_tag_char(&mut self) {
        self.tag_picker.new_tag_input.pop();
    }

    pub fn confirm_create_tag(&mut self) {
        let name = self.tag_picker.new_tag_input.trim();
        if !name.is_empty() {
            if let Ok(tag) = self.db.create_tag(name) {
                self.tag_picker.available_tags.push(tag);
                self.tag_picker.available_tags.sort_by(|a, b| a.name.cmp(&b.name));
            }
        }
        self.tag_picker.is_creating = false;
        self.tag_picker.new_tag_input.clear();
    }

    pub fn delete_selected_tag(&mut self) {
        if let Some(tag) = self.tag_picker.available_tags.get(self.tag_picker.selected_index).cloned() {
            if self.db.delete_tag(tag.id).is_ok() {
                self.tag_picker.available_tags.remove(self.tag_picker.selected_index);
                if self.tag_picker.selected_index >= self.tag_picker.available_tags.len()
                    && self.tag_picker.selected_index > 0
                {
                    self.tag_picker.selected_index -= 1;
                }
                // Refresh tagged_lines cache
                self.tagged_lines = self.db.get_tagged_lines(&self.file_hash).unwrap_or_default();
            }
        }
    }

    pub fn has_tags(&self, line: usize) -> bool {
        self.tagged_lines.contains(&line)
    }

    pub fn has_note(&self, line: usize) -> bool {
        self.noted_lines.contains(&line)
    }

    // Help
    pub fn toggle_help(&mut self) {
        self.mode = if self.mode == Mode::Help {
            Mode::Normal
        } else {
            Mode::Help
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_filter_parse_boolean() {
        let filter = Filter::parse("is_refusal=true").unwrap();
        assert_eq!(filter.field, "is_refusal");
        assert_eq!(filter.op, FilterOp::Eq);
        assert_eq!(filter.value, FilterValue::Bool(true));

        let filter = Filter::parse("is_truncated=false").unwrap();
        assert_eq!(filter.field, "is_truncated");
        assert_eq!(filter.value, FilterValue::Bool(false));

        let filter = Filter::parse("field!=TRUE").unwrap();
        assert_eq!(filter.op, FilterOp::Neq);
        assert_eq!(filter.value, FilterValue::Bool(true));
    }

    #[test]
    fn test_filter_parse_number() {
        let filter = Filter::parse("score>90").unwrap();
        assert_eq!(filter.field, "score");
        assert_eq!(filter.op, FilterOp::Gt);
        assert_eq!(filter.value, FilterValue::Number(90.0));
    }

    #[test]
    fn test_filter_matches_boolean() {
        use crate::data::{Conversation, Message, Metadata, Role};

        let mut metadata = Metadata::new();
        metadata.insert("is_refusal".to_string(), "true".to_string());

        let conv = Conversation {
            messages: vec![Message {
                role: Role::User,
                content: "test".to_string(),
            }],
            source_line: 1,
            metadata,
            preview_text: None,
        };

        let filter_true = Filter::parse("is_refusal=true").unwrap();
        let filter_false = Filter::parse("is_refusal=false").unwrap();
        let filter_neq = Filter::parse("is_refusal!=true").unwrap();

        assert!(filter_true.matches(&conv));
        assert!(!filter_false.matches(&conv));
        assert!(!filter_neq.matches(&conv));
    }

    #[test]
    fn test_filter_completion() {
        let fields = vec![
            "evaluation.alignment_score".to_string(),
            "evaluation.coherence_score".to_string(),
            "metadata.condition".to_string(),
        ];

        let mut filter = FilterState::default();

        // Type partial field name
        filter.expression = "eval".to_string();
        assert!(filter.try_complete(&fields));
        assert_eq!(filter.expression, "evaluation.alignment_score");

        // Tab again to cycle
        assert!(filter.try_complete(&fields));
        assert_eq!(filter.expression, "evaluation.coherence_score");

        // Tab again to wrap around
        assert!(filter.try_complete(&fields));
        assert_eq!(filter.expression, "evaluation.alignment_score");
    }

    #[test]
    fn test_filter_completion_with_operator() {
        let fields = vec![
            "score".to_string(),
            "score_alt".to_string(),
        ];

        let mut filter = FilterState::default();

        // Partial field with operator already typed
        filter.expression = "sco>90".to_string();
        assert!(filter.try_complete(&fields));
        assert_eq!(filter.expression, "score>90");
    }

    #[test]
    fn test_filter_completion_multi_filter() {
        let fields = vec![
            "alpha".to_string(),
            "beta".to_string(),
        ];

        let mut filter = FilterState::default();

        // Complete second filter in expression
        filter.expression = "alpha>50,be".to_string();
        assert!(filter.try_complete(&fields));
        assert_eq!(filter.expression, "alpha>50, beta");
    }
}
