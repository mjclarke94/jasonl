use std::collections::{HashMap, HashSet};

use crate::data::Conversation;

/// SIMD-optimized substring search using memchr
pub fn contains_fast(haystack: &str, needle: &str) -> bool {
    if needle.is_empty() {
        return true;
    }
    if needle.len() > haystack.len() {
        return false;
    }

    let needle_bytes = needle.as_bytes();
    let haystack_bytes = haystack.as_bytes();

    // Use memchr to find first byte, then verify rest
    let first_byte = needle_bytes[0];
    let mut start = 0;

    while let Some(pos) = memchr::memchr(first_byte, &haystack_bytes[start..]) {
        let abs_pos = start + pos;
        if abs_pos + needle_bytes.len() > haystack_bytes.len() {
            return false;
        }
        if &haystack_bytes[abs_pos..abs_pos + needle_bytes.len()] == needle_bytes {
            return true;
        }
        start = abs_pos + 1;
    }

    false
}

/// Trigram-based search index for fast substring search on rare terms
pub struct TrigramIndex {
    /// Maps each trigram to sorted list of conversation indices containing it
    index: HashMap<[u8; 3], Vec<usize>>,
    /// Number of conversations indexed
    pub conversation_count: usize,
}

impl TrigramIndex {
    /// Create an empty index
    pub fn new() -> Self {
        Self {
            index: HashMap::new(),
            conversation_count: 0,
        }
    }

    /// Check if the index is empty
    pub fn is_empty(&self) -> bool {
        self.index.is_empty()
    }

    /// Get approximate memory usage in bytes
    pub fn memory_usage(&self) -> usize {
        let mut total = 0;
        for list in self.index.values() {
            total += list.capacity() * std::mem::size_of::<usize>();
        }
        total += self.index.capacity()
            * (std::mem::size_of::<[u8; 3]>() + std::mem::size_of::<Vec<usize>>());
        total
    }

    /// Search for a query string, returning candidate indices
    /// These candidates are guaranteed to contain all trigrams from the query,
    /// but must still be verified with actual substring search
    pub fn search_candidates(&self, query_lower: &str) -> Option<Vec<usize>> {
        let bytes = query_lower.as_bytes();

        if bytes.len() < 3 {
            // Can't use trigram index for queries shorter than 3 chars
            return None;
        }

        // Get all trigrams from query
        let trigrams: Vec<[u8; 3]> = bytes
            .windows(3)
            .map(|w| w.try_into().unwrap())
            .collect();

        // Find the smallest posting list to minimize work
        let mut smallest: Option<&Vec<usize>> = None;
        let mut smallest_len = usize::MAX;

        for trigram in &trigrams {
            match self.index.get(trigram) {
                Some(list) if list.len() < smallest_len => {
                    smallest = Some(list);
                    smallest_len = list.len();
                }
                None => return Some(vec![]), // Trigram not found - no matches
                _ => {}
            }
        }

        smallest.cloned()
    }
}

/// Builder for incrementally constructing a trigram index
pub struct TrigramIndexBuilder {
    /// Index being built
    index: HashMap<[u8; 3], Vec<usize>>,
    /// Current progress (next conversation index to process)
    progress: usize,
    /// Total conversations to index
    total: usize,
}

impl TrigramIndexBuilder {
    /// Start building an index for the given number of conversations
    pub fn new(total: usize) -> Self {
        Self {
            index: HashMap::new(),
            progress: 0,
            total,
        }
    }

    /// Process a batch of conversations. Returns true if more work remains.
    pub fn process_batch(&mut self, conversations: &[Conversation], batch_size: usize) -> bool {
        let end = (self.progress + batch_size).min(self.total);

        for i in self.progress..end {
            let conv = &conversations[i];
            let bytes = conv.search_text.as_bytes();

            if bytes.len() < 3 {
                continue;
            }

            // Track seen trigrams to avoid duplicates within same conversation
            let mut seen: HashSet<[u8; 3]> = HashSet::new();

            for window in bytes.windows(3) {
                let trigram: [u8; 3] = window.try_into().unwrap();
                if seen.insert(trigram) {
                    self.index.entry(trigram).or_default().push(i);
                }
            }
        }

        self.progress = end;
        self.progress < self.total
    }

    /// Get current progress as (current, total)
    pub fn progress(&self) -> (usize, usize) {
        (self.progress, self.total)
    }

    /// Check if building is complete
    pub fn is_complete(&self) -> bool {
        self.progress >= self.total
    }

    /// Finish building and return the completed index
    pub fn finish(self) -> TrigramIndex {
        TrigramIndex {
            index: self.index,
            conversation_count: self.total,
        }
    }
}

/// Pre-sorted index for a numeric metadata field (enables O(log n) range queries)
pub struct SortedFieldIndex {
    /// (value, conversation_index) pairs sorted by value
    entries: Vec<(f64, usize)>,
}

impl SortedFieldIndex {
    /// Build index from conversations for a specific field
    pub fn build(conversations: &[Conversation], field: &str) -> Self {
        let mut entries: Vec<(f64, usize)> = conversations
            .iter()
            .enumerate()
            .filter_map(|(i, c)| {
                c.metadata
                    .fields
                    .get(field)
                    .and_then(|v| v.parse::<f64>().ok())
                    .map(|v| (v, i))
            })
            .collect();

        entries.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap_or(std::cmp::Ordering::Equal));

        Self { entries }
    }

    /// Get indices where field > threshold (O(log n))
    pub fn greater_than(&self, threshold: f64) -> &[(f64, usize)] {
        let pos = self.entries.partition_point(|(v, _)| *v <= threshold);
        &self.entries[pos..]
    }

    /// Get indices where field >= threshold (O(log n))
    pub fn greater_or_equal(&self, threshold: f64) -> &[(f64, usize)] {
        let pos = self.entries.partition_point(|(v, _)| *v < threshold);
        &self.entries[pos..]
    }

    /// Get indices where field < threshold (O(log n))
    pub fn less_than(&self, threshold: f64) -> &[(f64, usize)] {
        let pos = self.entries.partition_point(|(v, _)| *v < threshold);
        &self.entries[..pos]
    }

    /// Get indices where field <= threshold (O(log n))
    pub fn less_or_equal(&self, threshold: f64) -> &[(f64, usize)] {
        let pos = self.entries.partition_point(|(v, _)| *v <= threshold);
        &self.entries[..pos]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::data::{Conversation, Message, Metadata, Role};

    #[test]
    fn test_contains_fast() {
        assert!(contains_fast("hello world", "world"));
        assert!(contains_fast("hello world", "hello"));
        assert!(contains_fast("hello world", "lo wo"));
        assert!(!contains_fast("hello world", "foo"));
        assert!(!contains_fast("hello", "hello world"));
        assert!(contains_fast("hello", ""));
        assert!(contains_fast("", ""));
    }

    #[test]
    fn test_contains_fast_repeated() {
        assert!(contains_fast("aaaaa", "aaa"));
        assert!(contains_fast("abababab", "bab"));
    }

    fn make_conv_with_score(score: f64, idx: usize) -> Conversation {
        let mut metadata = Metadata::new();
        metadata.insert("score".to_string(), score.to_string());
        Conversation {
            messages: vec![Message {
                role: Role::User,
                content: format!("test {}", idx),
            }],
            source_line: idx,
            source_file: "test.jsonl".to_string(),
            file_hash: "abc123".to_string(),
            metadata,
            preview_text: None,
            search_text: format!("test {}", idx),
        }
    }

    #[test]
    fn test_sorted_field_index() {
        // Create conversations with scores: 10, 30, 50, 70, 90
        let conversations: Vec<Conversation> = vec![
            make_conv_with_score(50.0, 0),
            make_conv_with_score(10.0, 1),
            make_conv_with_score(90.0, 2),
            make_conv_with_score(30.0, 3),
            make_conv_with_score(70.0, 4),
        ];

        let index = SortedFieldIndex::build(&conversations, "score");

        // Test greater_than
        let gt_50: Vec<usize> = index.greater_than(50.0).iter().map(|(_, i)| *i).collect();
        assert_eq!(gt_50.len(), 2); // 70, 90
        assert!(gt_50.contains(&2)); // score=90
        assert!(gt_50.contains(&4)); // score=70

        // Test greater_or_equal
        let gte_50: Vec<usize> = index.greater_or_equal(50.0).iter().map(|(_, i)| *i).collect();
        assert_eq!(gte_50.len(), 3); // 50, 70, 90
        assert!(gte_50.contains(&0)); // score=50
        assert!(gte_50.contains(&2)); // score=90
        assert!(gte_50.contains(&4)); // score=70

        // Test less_than
        let lt_50: Vec<usize> = index.less_than(50.0).iter().map(|(_, i)| *i).collect();
        assert_eq!(lt_50.len(), 2); // 10, 30
        assert!(lt_50.contains(&1)); // score=10
        assert!(lt_50.contains(&3)); // score=30

        // Test less_or_equal
        let lte_50: Vec<usize> = index.less_or_equal(50.0).iter().map(|(_, i)| *i).collect();
        assert_eq!(lte_50.len(), 3); // 10, 30, 50
        assert!(lte_50.contains(&0)); // score=50
        assert!(lte_50.contains(&1)); // score=10
        assert!(lte_50.contains(&3)); // score=30
    }
}
