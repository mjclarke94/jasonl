use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::io::{self, Write};

use arboard::Clipboard;
use base64::Engine;

use crate::data::Conversation;
use crate::db::{Database, FileData, Tag};
use crate::filter_expr::{CompareOp, FilterExpr, Value};
use crate::search_index::{SortedFieldIndex, TrigramIndex, TrigramIndexBuilder};

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

    /// Update stats with a single conversation (for incremental loading)
    pub fn update_with_conversation(&mut self, conv: &Conversation) {
        for (key, value) in &conv.metadata.fields {
            if let Ok(num) = value.parse::<f64>() {
                self.fields
                    .entry(key.clone())
                    .or_insert_with(FieldStats::new)
                    .add(num);
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Mode {
    Normal,
    Search,
    Filter,
    Help,
    Notes,       // Editing notes for current conversation
    TagPicker,   // Selecting tags for current conversation
    TagManager,  // Creating/deleting tags
    SortPicker,  // Selecting sort field
    ConfirmQuit, // Confirm quit when unsaved changes
}

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub enum SortOrder {
    #[default]
    None,
    Ascending,
    Descending,
}

impl SortOrder {
    pub fn toggle(self) -> Self {
        match self {
            SortOrder::None => SortOrder::Ascending,
            SortOrder::Ascending => SortOrder::Descending,
            SortOrder::Descending => SortOrder::None,
        }
    }

    pub fn symbol(self) -> &'static str {
        match self {
            SortOrder::None => "",
            SortOrder::Ascending => "↑",
            SortOrder::Descending => "↓",
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct SortState {
    pub field: Option<String>,
    pub order: SortOrder,
    pub selected_index: usize,
}

#[derive(Debug, Default)]
pub struct FilterState {
    pub expression: String,
    /// Parsed filter expression (None if empty or error)
    pub parsed: Option<crate::filter_expr::FilterExpr>,
    pub error: Option<String>,
    // Completion state
    completion_prefix: String,
    completion_matches: Vec<String>,
    completion_index: usize,
}

impl FilterState {
    pub fn is_active(&self) -> bool {
        self.parsed.as_ref().map_or(false, |p| !matches!(p, crate::filter_expr::FilterExpr::True))
    }

    pub fn update(&mut self) {
        self.parsed = None;
        self.error = None;

        if self.expression.trim().is_empty() {
            return;
        }

        match crate::filter_expr::FilterExpr::parse(&self.expression) {
            Ok(expr) => {
                self.parsed = Some(expr);
            }
            Err(e) => {
                self.error = Some(e);
            }
        }
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
    /// Pre-lowercased query for fast comparison
    pub query_lower: String,
    pub matches: Vec<usize>,
    pub current_match: usize,
    /// Current search progress (index into conversations), None = search complete
    pub search_progress: Option<usize>,
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

/// Message collapse mode
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub enum CollapseMode {
    #[default]
    Expanded,     // All messages expanded
    SystemOnly,   // Only system messages collapsed
    AllCollapsed, // All messages collapsed
}

impl CollapseMode {
    pub fn cycle(self) -> Self {
        match self {
            CollapseMode::Expanded => CollapseMode::SystemOnly,
            CollapseMode::SystemOnly => CollapseMode::AllCollapsed,
            CollapseMode::AllCollapsed => CollapseMode::Expanded,
        }
    }

    pub fn label(self) -> &'static str {
        match self {
            CollapseMode::Expanded => "expanded",
            CollapseMode::SystemOnly => "sys collapsed",
            CollapseMode::AllCollapsed => "collapsed",
        }
    }
}

pub struct App {
    pub conversations: Vec<Conversation>,
    pub selected_index: usize,
    pub scroll_offset: u16,
    pub mode: Mode,
    pub file_path: String,
    pub search: SearchState,
    pub filter: FilterState,
    pub sort: SortState,
    pub should_quit: bool,
    // Cached indices of conversations matching current filter (HashSet for O(1) lookup)
    filtered_indices: HashSet<usize>,
    /// Current filter progress (index into conversations), None = filter complete
    filter_progress: Option<usize>,
    // Cached effective indices (sorted, filtered, searched) - uses Rc for cheap cloning
    cached_effective_indices: Option<Rc<Vec<usize>>>,
    // Cached stats for current filter/search view (only recomputed when filter/search changes)
    cached_current_stats: Option<MetadataStats>,
    // Global statistics for all conversations
    pub global_stats: MetadataStats,
    // All unique metadata field names for autocompletion
    pub metadata_fields: Vec<String>,
    // Multi-select: indices of marked conversations
    pub marked: HashSet<usize>,
    // Database for notes and tags
    pub db: Database,
    // In-memory cache of tag/note data per file (keyed by file_hash)
    pub file_data_map: HashMap<String, FileData>,
    // Notes state (for UI editing)
    pub notes: NotesState,
    // Tag picker state
    pub tag_picker: TagPickerState,
    // UI panel widths (as percentages)
    pub list_panel_width: u16, // Default 25%
    // Cached list viewport height for page up/down (updated by UI)
    pub list_viewport_height: usize,
    // Message collapse mode
    pub collapse_mode: CollapseMode,
    // Trigram search index (optional, built on demand)
    trigram_index: Option<TrigramIndex>,
    // Index builder (when building in progress)
    index_builder: Option<TrigramIndexBuilder>,
    // Sorted field indices for O(log n) range queries (built on demand)
    field_indices: HashMap<String, SortedFieldIndex>,
}

impl App {
    /// Create an empty App for streaming loading
    pub fn new_empty(file_path: String, db: Database) -> Self {
        Self {
            conversations: Vec::new(),
            selected_index: 0,
            scroll_offset: 0,
            mode: Mode::Normal,
            file_path,
            search: SearchState::default(),
            filter: FilterState::default(),
            sort: SortState::default(),
            should_quit: false,
            filtered_indices: HashSet::new(),
            filter_progress: None,
            cached_effective_indices: None,
            cached_current_stats: None,
            global_stats: MetadataStats::default(),
            metadata_fields: Vec::new(),
            marked: HashSet::new(),
            db,
            file_data_map: HashMap::new(),
            notes: NotesState::default(),
            tag_picker: TagPickerState::default(),
            list_panel_width: 25,
            list_viewport_height: 20, // Default, updated by UI
            collapse_mode: CollapseMode::default(),
            trigram_index: None,
            index_builder: None,
            field_indices: HashMap::new(),
        }
    }

    /// Add new conversations (from streaming loader)
    pub fn add_conversations(&mut self, new_convs: Vec<Conversation>, _file_hashes: &HashMap<String, String>) {
        let _start_idx = self.conversations.len();

        for conv in new_convs {
            let idx = self.conversations.len();

            // Load file data for new file hashes
            if !self.file_data_map.contains_key(&conv.file_hash) {
                let mut file_data = self.db.load_file_data(&conv.file_hash).unwrap_or_else(|_| {
                    FileData::new(vec![], HashMap::new(), HashMap::new())
                });
                file_data.snapshot_original();
                self.file_data_map.insert(conv.file_hash.clone(), file_data);
            }

            // Collect new metadata fields
            for key in conv.metadata.fields.keys() {
                if !self.metadata_fields.contains(key) {
                    self.metadata_fields.push(key.clone());
                }
            }

            // Update global stats incrementally
            self.global_stats.update_with_conversation(&conv);

            self.conversations.push(conv);

            // Add to filtered indices if no filter active, or if matches filter
            if !self.filter.is_active() || self.conversation_matches_filter(&self.conversations[idx]) {
                self.filtered_indices.insert(idx);
            }
        }

        // Sort metadata fields
        self.metadata_fields.sort();

        // Update caches since indices changed
        self.invalidate_field_indices();
        self.update_effective_cache();
    }

    pub fn new(conversations: Vec<Conversation>, file_path: String, file_hashes: HashMap<String, String>, db: Database) -> Self {
        let filtered_indices: HashSet<usize> = (0..conversations.len()).collect();
        let global_stats = MetadataStats::from_conversations(conversations.iter());

        // Collect unique metadata field names for autocompletion
        let mut metadata_fields: Vec<String> = conversations
            .iter()
            .flat_map(|c| c.metadata.fields.keys().cloned())
            .collect::<HashSet<_>>()
            .into_iter()
            .collect();
        metadata_fields.sort();

        // Load all tag/note data into memory for each unique file hash
        let mut file_data_map: HashMap<String, FileData> = HashMap::new();
        let unique_hashes: HashSet<_> = file_hashes.values().cloned().collect();
        for hash in unique_hashes {
            let mut file_data = db.load_file_data(&hash).unwrap_or_else(|_| {
                FileData::new(vec![], HashMap::new(), HashMap::new())
            });
            file_data.snapshot_original();
            file_data_map.insert(hash, file_data);
        }

        Self {
            conversations,
            selected_index: 0,
            scroll_offset: 0,
            mode: Mode::Normal,
            file_path,
            search: SearchState::default(),
            filter: FilterState::default(),
            sort: SortState::default(),
            should_quit: false,
            filtered_indices,
            filter_progress: None,
            cached_effective_indices: None,
            cached_current_stats: None,
            global_stats,
            metadata_fields,
            marked: HashSet::new(),
            db,
            file_data_map,
            notes: NotesState::default(),
            tag_picker: TagPickerState::default(),
            list_panel_width: 25,
            list_viewport_height: 20, // Default, updated by UI
            collapse_mode: CollapseMode::default(),
            trigram_index: None,
            index_builder: None,
            field_indices: HashMap::new(),
        }
    }

    /// Save any modified data back to the database
    pub fn save(&self) {
        for (hash, file_data) in &self.file_data_map {
            if file_data.is_dirty() {
                if let Err(e) = self.db.save_file_data(hash, file_data) {
                    eprintln!("Warning: Failed to save data for {}: {}", hash, e);
                }
            }
        }
    }

    /// Check if any file has unsaved changes
    pub fn has_unsaved_changes(&self) -> bool {
        self.file_data_map.values().any(|fd| fd.is_dirty())
    }

    /// Get statistics for currently visible conversations (cached, only recomputed when filter/search changes)
    pub fn current_stats(&self) -> MetadataStats {
        // Return cached stats if available
        if let Some(ref cached) = self.cached_current_stats {
            return cached.clone();
        }
        // Fallback to computing (shouldn't happen if update_effective_cache is called properly)
        let visible_convs = self.effective_indices();
        MetadataStats::from_conversations(visible_convs.iter().map(|&i| &self.conversations[i]))
    }

    /// Check if we're viewing a subset (filter or search active)
    pub fn is_filtered_view(&self) -> bool {
        !self.search.query.is_empty() || self.filter.is_active()
    }

    /// Reset filter to include all conversations (for incremental rebuilding)
    fn reset_filter(&mut self) {
        self.filtered_indices.clear();
        self.filter_progress = Some(0);
        self.invalidate_effective_cache();
    }

    /// Check if a conversation matches current filters
    fn conversation_matches_filter(&self, conv: &Conversation) -> bool {
        // If no parsed filter, include all
        let Some(ref expr) = self.filter.parsed else {
            return true;
        };

        // Get file data for this conversation (for tag/note filters)
        let file_data = self.file_data_map.get(&conv.file_hash);

        expr.matches(conv, file_data)
    }

    /// Continue incremental filter processing. Returns true if more work remains.
    pub fn continue_filter(&mut self, batch_size: usize) -> bool {
        let Some(start) = self.filter_progress else {
            return false;
        };

        let end = (start + batch_size).min(self.conversations.len());

        for i in start..end {
            if self.conversation_matches_filter(&self.conversations[i]) {
                self.filtered_indices.insert(i);
            }
        }

        if end >= self.conversations.len() {
            self.filter_progress = None;
            self.update_effective_cache(); // Rebuild cache on completion
            false
        } else {
            self.filter_progress = Some(end);
            self.invalidate_effective_cache(); // Invalidate during incremental work
            true
        }
    }

    /// Full (blocking) filter update - used when we need immediate results
    fn update_filtered_indices(&mut self) {
        self.filtered_indices = self
            .conversations
            .iter()
            .enumerate()
            .filter(|(_, c)| self.conversation_matches_filter(c))
            .map(|(i, _)| i)
            .collect();
        self.filter_progress = None;
        self.update_effective_cache(); // Rebuild cache after full update
    }

    pub fn selected_conversation(&self) -> Option<&Conversation> {
        let indices = self.effective_indices();
        indices
            .get(self.selected_index)
            .and_then(|&i| self.conversations.get(i))
    }

    /// Invalidate the cached effective indices and stats (call when filter/search/sort changes)
    fn invalidate_effective_cache(&mut self) {
        self.cached_effective_indices = None;
        self.cached_current_stats = None;
    }

    /// Get effective indices, using cache when possible (Rc for cheap cloning)
    fn effective_indices(&self) -> Rc<Vec<usize>> {
        // Return cached version if available (Rc clone is O(1))
        if let Some(ref cached) = self.cached_effective_indices {
            return Rc::clone(cached);
        }

        // Compute indices
        let mut indices: Vec<usize> = if self.search.query.is_empty() {
            self.filtered_indices.iter().copied().collect()
        } else {
            // Intersection of filtered and search matches (O(1) lookup per item)
            self.search
                .matches
                .iter()
                .filter(|i| self.filtered_indices.contains(i))
                .copied()
                .collect()
        };

        // Apply sorting if active
        if let (Some(field), order) = (&self.sort.field, self.sort.order) {
            if order != SortOrder::None {
                indices.sort_by(|&a, &b| {
                    let val_a = self.conversations[a].metadata.fields.get(field)
                        .and_then(|v| v.parse::<f64>().ok());
                    let val_b = self.conversations[b].metadata.fields.get(field)
                        .and_then(|v| v.parse::<f64>().ok());

                    let cmp = match (val_a, val_b) {
                        (Some(a), Some(b)) => a.partial_cmp(&b).unwrap_or(std::cmp::Ordering::Equal),
                        (Some(_), None) => std::cmp::Ordering::Less, // Values before None
                        (None, Some(_)) => std::cmp::Ordering::Greater,
                        (None, None) => std::cmp::Ordering::Equal,
                    };

                    match order {
                        SortOrder::Ascending => cmp,
                        SortOrder::Descending => cmp.reverse(),
                        SortOrder::None => std::cmp::Ordering::Equal,
                    }
                });
            }
        }

        Rc::new(indices)
    }

    /// Update the effective indices cache (call after search/filter/sort changes stabilize)
    pub fn update_effective_cache(&mut self) {
        self.cached_effective_indices = None; // Clear first to avoid borrow issues
        self.cached_current_stats = None;
        let indices = self.effective_indices();
        // Compute stats while we have the indices
        let stats = MetadataStats::from_conversations(indices.iter().map(|&i| &self.conversations[i]));
        self.cached_effective_indices = Some(indices);
        self.cached_current_stats = Some(stats);
    }

    /// Get only the conversations visible in the viewport (O(viewport_height) instead of O(n))
    /// Returns (start_index, Vec of (display_idx, actual_idx, &Conversation))
    pub fn viewport_conversations(&self, viewport_height: usize) -> (usize, Vec<(usize, usize, &Conversation)>) {
        let indices = self.effective_indices();
        let total = indices.len();

        if total == 0 || viewport_height == 0 {
            return (0, Vec::new());
        }

        // Calculate scroll offset to keep selected item visible (same logic as ratatui List)
        let selected = self.selected_index.min(total.saturating_sub(1));
        let start = if selected >= viewport_height {
            // Selected item is below the viewport, scroll down
            selected.saturating_sub(viewport_height - 1)
        } else {
            0
        };
        let end = (start + viewport_height).min(total);

        let items = (start..end)
            .map(|display_idx| {
                let actual_idx = indices[display_idx];
                (display_idx, actual_idx, &self.conversations[actual_idx])
            })
            .collect();

        (start, items)
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

    /// Move down by a full page in the conversation list
    pub fn page_down_conversations(&mut self, page_size: usize) {
        let total = self.total_visible();
        if total > 0 {
            self.selected_index = (self.selected_index + page_size).min(total - 1);
            self.scroll_offset = 0;
            if self.notes.visible {
                self.load_current_note();
            }
        }
    }

    /// Move up by a full page in the conversation list
    pub fn page_up_conversations(&mut self, page_size: usize) {
        self.selected_index = self.selected_index.saturating_sub(page_size);
        self.scroll_offset = 0;
        if self.notes.visible {
            self.load_current_note();
        }
    }

    /// Select a conversation by display index (for mouse clicks)
    pub fn select_conversation(&mut self, index: usize) {
        let total = self.total_visible();
        if index < total {
            self.selected_index = index;
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
        self.search.query_lower.clear();
        self.search.matches.clear();
        self.search.search_progress = None;
        self.update_effective_cache();
    }

    pub fn exit_search_mode(&mut self) {
        self.mode = Mode::Normal;
    }

    pub fn clear_search(&mut self) {
        self.search.query.clear();
        self.search.query_lower.clear();
        self.search.matches.clear();
        self.search.current_match = 0;
        self.search.search_progress = None;
        self.selected_index = 0;
        self.update_effective_cache();
        self.mode = Mode::Normal;
    }

    /// Start an incremental search (called when query changes)
    fn start_search(&mut self) {
        self.search.matches.clear();
        self.search.query_lower = self.search.query.to_lowercase();
        if self.search.query.is_empty() {
            self.search.search_progress = None;
        } else {
            self.search.search_progress = Some(0);
        }
        self.selected_index = 0;
        self.search.current_match = 0;
        self.invalidate_effective_cache();
    }

    /// Continue incremental search processing. Returns true if more work remains.
    pub fn continue_search(&mut self, batch_size: usize) -> bool {
        let Some(start) = self.search.search_progress else {
            return false;
        };

        if self.search.query_lower.is_empty() {
            self.search.search_progress = None;
            return false;
        }

        // If trigram index is available and query is long enough, use it for instant results
        if start == 0 {
            if let Some(ref index) = self.trigram_index {
                if let Some(candidates) = index.search_candidates(&self.search.query_lower) {
                    // Verify candidates with actual substring match
                    self.search.matches = candidates
                        .into_iter()
                        .filter(|&i| self.conversations[i].contains(&self.search.query_lower))
                        .collect();
                    self.search.search_progress = None;
                    self.update_effective_cache(); // Rebuild cache on completion
                    return false;
                }
            }
        }

        // Fall back to linear scan
        let end = (start + batch_size).min(self.conversations.len());

        for i in start..end {
            if self.conversations[i].contains(&self.search.query_lower) {
                self.search.matches.push(i);
            }
        }

        if end >= self.conversations.len() {
            self.search.search_progress = None;
            self.update_effective_cache(); // Rebuild cache on completion
            false
        } else {
            self.search.search_progress = Some(end);
            // Don't invalidate cache every batch - results are appending
            true
        }
    }

    /// Check if search is in progress
    pub fn is_search_in_progress(&self) -> bool {
        self.search.search_progress.is_some()
    }

    /// Get search progress as (current, total)
    pub fn search_progress(&self) -> Option<(usize, usize)> {
        self.search.search_progress.map(|p| (p, self.conversations.len()))
    }

    // Index methods

    /// Start building the trigram search index
    pub fn start_index_build(&mut self) {
        if self.trigram_index.is_some() || self.index_builder.is_some() {
            return; // Already built or building
        }
        self.index_builder = Some(TrigramIndexBuilder::new(self.conversations.len()));
    }

    /// Continue building the index. Returns true if more work remains.
    pub fn continue_index_build(&mut self, batch_size: usize) -> bool {
        let Some(ref mut builder) = self.index_builder else {
            return false;
        };

        let has_more = builder.process_batch(&self.conversations, batch_size);

        if !has_more {
            // Finished building - move index to final storage
            let builder = self.index_builder.take().unwrap();
            self.trigram_index = Some(builder.finish());
        }

        has_more
    }

    /// Check if index is being built
    pub fn is_index_building(&self) -> bool {
        self.index_builder.is_some()
    }

    /// Get index build progress as (current, total)
    pub fn index_build_progress(&self) -> Option<(usize, usize)> {
        self.index_builder.as_ref().map(|b| b.progress())
    }

    /// Check if trigram index is available
    pub fn has_search_index(&self) -> bool {
        self.trigram_index.is_some()
    }

    /// Get index memory usage in bytes
    pub fn index_memory_usage(&self) -> Option<usize> {
        self.trigram_index.as_ref().map(|i| i.memory_usage())
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
        self.start_search();
    }

    pub fn pop_search_char(&mut self) {
        self.search.query.pop();
        self.start_search();
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
        self.filter.parsed = None;
        self.filter.error = None;
        // Reset to all indices
        self.filtered_indices = (0..self.conversations.len()).collect();
        self.filter_progress = None;
        self.selected_index = 0;
        self.update_effective_cache();
        self.mode = Mode::Normal;
    }

    /// Start incremental filter (called when filter expression changes)
    fn start_filter(&mut self) {
        self.filter.update();
        if !self.filter.is_active() {
            // No filter active - include all
            self.filtered_indices = (0..self.conversations.len()).collect();
            self.filter_progress = None;
            self.update_effective_cache();
        } else if let Some(ref expr) = self.filter.parsed.clone() {
            // Try index-based evaluation first (O(log n) for numeric comparisons)
            if let Some(matches) = self.evaluate_filter_with_index(&expr) {
                self.filtered_indices = matches;
                self.filter_progress = None;
                self.update_effective_cache();
            } else {
                // Fall back to incremental filter for complex expressions
                self.reset_filter();
            }
        } else {
            // Parse error or empty - include all
            self.filtered_indices = (0..self.conversations.len()).collect();
            self.filter_progress = None;
            self.update_effective_cache();
        }
        self.selected_index = 0;
    }

    /// Check if filter is in progress
    pub fn is_filter_in_progress(&self) -> bool {
        self.filter_progress.is_some()
    }

    /// Get filter progress as (current, total)
    pub fn filter_progress(&self) -> Option<(usize, usize)> {
        self.filter_progress.map(|p| (p, self.conversations.len()))
    }

    /// Ensure a field index exists, building it if necessary
    fn ensure_field_index(&mut self, field: &str) {
        if !self.field_indices.contains_key(field) {
            let index = SortedFieldIndex::build(&self.conversations, field);
            self.field_indices.insert(field.to_string(), index);
        }
    }

    /// Collect all numeric fields referenced in a filter expression
    fn collect_numeric_fields(expr: &FilterExpr, fields: &mut Vec<String>) {
        match expr {
            FilterExpr::Comparison { field, value: Value::Number(_), .. } => {
                if !fields.contains(field) {
                    fields.push(field.clone());
                }
            }
            FilterExpr::And(left, right) | FilterExpr::Or(left, right) => {
                Self::collect_numeric_fields(left, fields);
                Self::collect_numeric_fields(right, fields);
            }
            FilterExpr::Not(inner) => {
                Self::collect_numeric_fields(inner, fields);
            }
            _ => {}
        }
    }

    /// Try to evaluate a filter expression using indices for O(log n) performance.
    /// Returns Some(matching_indices) if index-based evaluation was possible,
    /// None if we need to fall back to linear scan.
    fn evaluate_filter_with_index(&mut self, expr: &FilterExpr) -> Option<HashSet<usize>> {
        // First, build all needed indices
        let mut fields = Vec::new();
        Self::collect_numeric_fields(expr, &mut fields);
        for field in &fields {
            self.ensure_field_index(field);
        }

        // Now evaluate using immutable borrows
        let conv_count = self.conversations.len();
        self.evaluate_filter_expr_with_index(expr, conv_count)
    }

    /// Internal evaluation using pre-built indices (no mutable borrows)
    fn evaluate_filter_expr_with_index(&self, expr: &FilterExpr, conv_count: usize) -> Option<HashSet<usize>> {
        match expr {
            FilterExpr::True => {
                // All conversations match
                Some((0..conv_count).collect())
            }
            FilterExpr::Comparison { field, op, value } => {
                // Only numeric comparisons can use the index
                let Value::Number(threshold) = value else {
                    return None;
                };

                let index = self.field_indices.get(field)?;
                let matches: HashSet<usize> = match op {
                    CompareOp::Gt => index.greater_than(*threshold).iter().map(|(_, i)| *i).collect(),
                    CompareOp::Gte => index.greater_or_equal(*threshold).iter().map(|(_, i)| *i).collect(),
                    CompareOp::Lt => index.less_than(*threshold).iter().map(|(_, i)| *i).collect(),
                    CompareOp::Lte => index.less_or_equal(*threshold).iter().map(|(_, i)| *i).collect(),
                    CompareOp::Eq => {
                        // For equality, find exact matches in the sorted range
                        let gte = index.greater_or_equal(*threshold);
                        gte.iter()
                            .take_while(|(v, _)| (*v - threshold).abs() < f64::EPSILON)
                            .map(|(_, i)| *i)
                            .collect()
                    }
                    CompareOp::Neq => {
                        // All except those equal to threshold
                        let all: HashSet<usize> = (0..conv_count).collect();
                        let eq: HashSet<usize> = {
                            let gte = index.greater_or_equal(*threshold);
                            gte.iter()
                                .take_while(|(v, _)| (*v - threshold).abs() < f64::EPSILON)
                                .map(|(_, i)| *i)
                                .collect()
                        };
                        all.difference(&eq).copied().collect()
                    }
                };
                Some(matches)
            }
            FilterExpr::And(left, right) => {
                // Try to evaluate both sides with indices
                let left_matches = self.evaluate_filter_expr_with_index(left, conv_count)?;
                let right_matches = self.evaluate_filter_expr_with_index(right, conv_count)?;
                Some(left_matches.intersection(&right_matches).copied().collect())
            }
            FilterExpr::Or(left, right) => {
                // Try to evaluate both sides with indices
                let left_matches = self.evaluate_filter_expr_with_index(left, conv_count)?;
                let right_matches = self.evaluate_filter_expr_with_index(right, conv_count)?;
                Some(left_matches.union(&right_matches).copied().collect())
            }
            FilterExpr::Not(inner) => {
                // Try to evaluate inner with index
                let inner_matches = self.evaluate_filter_expr_with_index(inner, conv_count)?;
                let all: HashSet<usize> = (0..conv_count).collect();
                Some(all.difference(&inner_matches).copied().collect())
            }
            // Special filters (tags/notes) require per-conversation evaluation
            FilterExpr::Special(_) => None,
        }
    }

    /// Clear field indices (call when conversations change)
    fn invalidate_field_indices(&mut self) {
        self.field_indices.clear();
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
        // Live preview - start incremental filter
        self.start_filter();
    }

    pub fn pop_filter_char(&mut self) {
        self.filter.expression.pop();
        self.filter.reset_completion();
        self.start_filter();
    }

    pub fn complete_filter(&mut self) {
        if self.filter.try_complete(&self.metadata_fields) {
            // Update filter after completion
            self.start_filter();
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
        for &idx in indices.iter() {
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

    fn load_current_note(&mut self) {
        if let Some(conv) = self.selected_conversation() {
            let line = conv.source_line;
            let hash = conv.file_hash.clone();
            self.notes.content = self
                .file_data_map
                .get(&hash)
                .and_then(|fd| fd.get_note(line))
                .map(|s| s.to_string())
                .unwrap_or_default();
        }
    }

    fn save_current_note(&mut self) {
        if let Some(conv) = self.selected_conversation() {
            let line = conv.source_line;
            let hash = conv.file_hash.clone();
            let content = self.notes.content.clone();
            if let Some(fd) = self.file_data_map.get_mut(&hash) {
                fd.set_note(line, content);
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

    /// Get global tags (from first file_data - they should all be the same)
    fn get_global_tags(&self) -> Vec<Tag> {
        self.file_data_map
            .values()
            .next()
            .map(|fd| fd.tags.clone())
            .unwrap_or_default()
    }

    pub fn enter_tag_picker(&mut self) {
        self.tag_picker.available_tags = self.get_global_tags();
        self.tag_picker.selected_index = 0;
        self.tag_picker.is_creating = false;
        self.tag_picker.new_tag_input.clear();
        self.mode = Mode::TagPicker;
    }

    pub fn exit_tag_picker(&mut self) {
        self.mode = Mode::Normal;
    }

    pub fn enter_tag_manager(&mut self) {
        self.tag_picker.available_tags = self.get_global_tags();
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

        // Get (file_hash, line) pairs to operate on: marked conversations or current
        let targets: Vec<(String, usize)> = if self.marked.is_empty() {
            self.selected_conversation()
                .map(|c| vec![(c.file_hash.clone(), c.source_line)])
                .unwrap_or_default()
        } else {
            self.marked
                .iter()
                .filter_map(|&idx| self.conversations.get(idx).map(|c| (c.file_hash.clone(), c.source_line)))
                .collect()
        };

        for (hash, line) in targets {
            if let Some(fd) = self.file_data_map.get_mut(&hash) {
                fd.toggle_tag(line, tag.id);
            }
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
            // Tags are global, so write to DB immediately
            if let Ok(tag) = self.db.create_tag(name) {
                self.tag_picker.available_tags.push(tag.clone());
                self.tag_picker.available_tags.sort_by(|a, b| a.name.cmp(&b.name));
                // Update all file_data tags
                for fd in self.file_data_map.values_mut() {
                    fd.tags.push(tag.clone());
                    fd.tags.sort_by(|a, b| a.name.cmp(&b.name));
                }
            }
        }
        self.tag_picker.is_creating = false;
        self.tag_picker.new_tag_input.clear();
    }

    pub fn delete_selected_tag(&mut self) {
        if let Some(tag) = self.tag_picker.available_tags.get(self.tag_picker.selected_index).cloned() {
            // Tags are global, so delete from DB immediately
            if self.db.delete_tag(tag.id).is_ok() {
                self.tag_picker.available_tags.remove(self.tag_picker.selected_index);
                if self.tag_picker.selected_index >= self.tag_picker.available_tags.len()
                    && self.tag_picker.selected_index > 0
                {
                    self.tag_picker.selected_index -= 1;
                }
                // Remove from all file_data
                for fd in self.file_data_map.values_mut() {
                    fd.tags.retain(|t| t.id != tag.id);
                    for tags in fd.conversation_tags.values_mut() {
                        tags.remove(&tag.id);
                    }
                }
            }
        }
    }

    // Sort
    pub fn enter_sort_picker(&mut self) {
        self.sort.selected_index = 0;
        // Pre-select current sort field if any
        if let Some(ref field) = self.sort.field {
            if let Some(pos) = self.metadata_fields.iter().position(|f| f == field) {
                self.sort.selected_index = pos;
            }
        }
        self.mode = Mode::SortPicker;
    }

    pub fn exit_sort_picker(&mut self) {
        self.mode = Mode::Normal;
    }

    pub fn sort_picker_next(&mut self) {
        let len = self.metadata_fields.len();
        if len > 0 {
            self.sort.selected_index = (self.sort.selected_index + 1) % len;
        }
    }

    pub fn sort_picker_prev(&mut self) {
        let len = self.metadata_fields.len();
        if len > 0 {
            self.sort.selected_index = if self.sort.selected_index == 0 {
                len - 1
            } else {
                self.sort.selected_index - 1
            };
        }
    }

    pub fn toggle_selected_sort(&mut self) {
        if let Some(field) = self.metadata_fields.get(self.sort.selected_index).cloned() {
            if self.sort.field.as_ref() == Some(&field) {
                // Same field - toggle order
                self.sort.order = self.sort.order.toggle();
                if self.sort.order == SortOrder::None {
                    self.sort.field = None;
                }
            } else {
                // Different field - start ascending
                self.sort.field = Some(field);
                self.sort.order = SortOrder::Ascending;
            }
            self.selected_index = 0;
            self.update_effective_cache();
        }
    }

    pub fn clear_sort(&mut self) {
        self.sort.field = None;
        self.sort.order = SortOrder::None;
        self.selected_index = 0;
        self.update_effective_cache();
    }

    // Collapse mode
    pub fn cycle_collapse_mode(&mut self) {
        self.collapse_mode = self.collapse_mode.cycle();
    }

    // Panel resize
    pub fn increase_list_width(&mut self) {
        if self.list_panel_width < 50 {
            self.list_panel_width += 5;
        }
    }

    pub fn decrease_list_width(&mut self) {
        if self.list_panel_width > 15 {
            self.list_panel_width -= 5;
        }
    }

    // Quit
    pub fn request_quit(&mut self) {
        if self.has_unsaved_changes() {
            self.mode = Mode::ConfirmQuit;
        } else {
            self.should_quit = true;
        }
    }

    pub fn confirm_quit_save(&mut self) {
        self.save();
        self.should_quit = true;
    }

    pub fn confirm_quit_discard(&mut self) {
        self.should_quit = true;
    }

    pub fn cancel_quit(&mut self) {
        self.mode = Mode::Normal;
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
    use crate::filter_expr::{FilterExpr, CompareOp, Value};

    #[test]
    fn test_filter_parse_boolean() {
        let filter = FilterExpr::parse("is_refusal=true").unwrap();
        if let FilterExpr::Comparison { field, op, value } = filter {
            assert_eq!(field, "is_refusal");
            assert_eq!(op, CompareOp::Eq);
            assert_eq!(value, Value::Bool(true));
        } else {
            panic!("Expected Comparison");
        }

        let filter = FilterExpr::parse("is_truncated=false").unwrap();
        if let FilterExpr::Comparison { value, .. } = filter {
            assert_eq!(value, Value::Bool(false));
        } else {
            panic!("Expected Comparison");
        }

        let filter = FilterExpr::parse("field!=true").unwrap();
        if let FilterExpr::Comparison { op, value, .. } = filter {
            assert_eq!(op, CompareOp::Neq);
            assert_eq!(value, Value::Bool(true));
        } else {
            panic!("Expected Comparison");
        }
    }

    #[test]
    fn test_filter_parse_number() {
        let filter = FilterExpr::parse("score>90").unwrap();
        if let FilterExpr::Comparison { field, op, value } = filter {
            assert_eq!(field, "score");
            assert_eq!(op, CompareOp::Gt);
            assert_eq!(value, Value::Number(90.0));
        } else {
            panic!("Expected Comparison");
        }
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
            source_file: "test.jsonl".to_string(),
            file_hash: "abc123".to_string(),
            metadata,
            preview_text: None,
            search_text: "test".to_string(),
        };

        let filter_true = FilterExpr::parse("is_refusal=true").unwrap();
        let filter_false = FilterExpr::parse("is_refusal=false").unwrap();
        let filter_neq = FilterExpr::parse("is_refusal!=true").unwrap();

        assert!(filter_true.matches(&conv, None));
        assert!(!filter_false.matches(&conv, None));
        assert!(!filter_neq.matches(&conv, None));
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
