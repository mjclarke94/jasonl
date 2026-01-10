//! Streaming/incremental file loader for non-blocking startup
//!
//! Loads JSONL files incrementally in batches, allowing the UI to remain
//! responsive during loading of large files.

use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader, Lines};
use std::path::PathBuf;

use glob::glob;
use sha2::{Digest, Sha256};

use crate::data::{Conversation, Message, Metadata, Role, Schema};

/// Tracks loading progress and errors
#[derive(Debug, Default)]
pub struct LoadingStats {
    pub files_total: usize,
    pub files_loaded: usize,
    pub lines_processed: usize,
    pub conversations_loaded: usize,
    pub errors: Vec<LoadError>,
}

#[derive(Debug)]
pub struct LoadError {
    pub file: String,
    pub line: Option<usize>,
    pub message: String,
}

/// State for incrementally loading files
pub struct StreamingLoader {
    /// Files remaining to be loaded (path, hash)
    pending_files: Vec<(PathBuf, String)>,
    /// Current file being read
    current_reader: Option<CurrentFile>,
    /// Schema for custom format parsing
    schema: Option<Schema>,
    /// Metadata fields to extract
    metadata_fields: Vec<String>,
    /// Loading statistics
    pub stats: LoadingStats,
    /// File path to hash mapping (for database lookups)
    pub file_hashes: HashMap<String, String>,
}

struct CurrentFile {
    path: PathBuf,
    hash: String,
    reader: Lines<BufReader<File>>,
    line_num: usize,
}

impl StreamingLoader {
    /// Create a new streaming loader from file patterns (supports globs)
    pub fn new(
        patterns: Vec<String>,
        schema: Option<Schema>,
        metadata_fields: Vec<String>,
    ) -> Result<Self, String> {
        let mut pending_files = Vec::new();
        let mut file_hashes = HashMap::new();

        for pattern in &patterns {
            // Check if it's a glob pattern
            if pattern.contains('*') || pattern.contains('?') || pattern.contains('[') {
                match glob(pattern) {
                    Ok(paths) => {
                        for entry in paths {
                            match entry {
                                Ok(path) => {
                                    if path.is_file() {
                                        let hash = hash_file(&path)?;
                                        file_hashes.insert(path.display().to_string(), hash.clone());
                                        pending_files.push((path, hash));
                                    }
                                }
                                Err(e) => {
                                    return Err(format!("Glob error: {}", e));
                                }
                            }
                        }
                    }
                    Err(e) => {
                        return Err(format!("Invalid glob pattern '{}': {}", pattern, e));
                    }
                }
            } else {
                // Regular file path
                let path = PathBuf::from(pattern);
                if path.exists() {
                    let hash = hash_file(&path)?;
                    file_hashes.insert(path.display().to_string(), hash.clone());
                    pending_files.push((path, hash));
                } else {
                    return Err(format!("File not found: {}", pattern));
                }
            }
        }

        if pending_files.is_empty() {
            return Err("No files found matching the given patterns".to_string());
        }

        let stats = LoadingStats {
            files_total: pending_files.len(),
            ..Default::default()
        };

        Ok(Self {
            pending_files,
            current_reader: None,
            schema,
            metadata_fields,
            stats,
            file_hashes,
        })
    }

    /// Check if loading is complete
    pub fn is_complete(&self) -> bool {
        self.current_reader.is_none() && self.pending_files.is_empty()
    }

    /// Get loading progress as (files_done, files_total, lines_processed)
    pub fn progress(&self) -> (usize, usize, usize) {
        (
            self.stats.files_loaded,
            self.stats.files_total,
            self.stats.lines_processed,
        )
    }

    /// Load a batch of lines, returning new conversations.
    /// Returns (new_conversations, has_more_work)
    pub fn load_batch(&mut self, batch_size: usize) -> (Vec<Conversation>, bool) {
        let mut conversations = Vec::new();
        let mut lines_processed = 0;

        while lines_processed < batch_size {
            // Ensure we have a current reader
            if self.current_reader.is_none() {
                if let Some((path, hash)) = self.pending_files.pop() {
                    match File::open(&path) {
                        Ok(file) => {
                            self.current_reader = Some(CurrentFile {
                                path: path.clone(),
                                hash,
                                reader: BufReader::new(file).lines(),
                                line_num: 0,
                            });
                        }
                        Err(e) => {
                            self.stats.errors.push(LoadError {
                                file: path.display().to_string(),
                                line: None,
                                message: format!("Failed to open: {}", e),
                            });
                            continue;
                        }
                    }
                } else {
                    // No more files
                    break;
                }
            }

            let current = self.current_reader.as_mut().unwrap();

            match current.reader.next() {
                Some(Ok(line)) => {
                    current.line_num += 1;
                    self.stats.lines_processed += 1;
                    lines_processed += 1;

                    let line = line.trim();
                    if line.is_empty() {
                        continue;
                    }

                    let result = if let Some(ref schema) = self.schema {
                        parse_custom_format(
                            line,
                            current.line_num,
                            schema,
                            &current.path.display().to_string(),
                            &current.hash,
                        )
                    } else {
                        parse_standard_format(
                            line,
                            current.line_num,
                            &self.metadata_fields,
                            &current.path.display().to_string(),
                            &current.hash,
                        )
                    };

                    match result {
                        Ok(conv) => {
                            self.stats.conversations_loaded += 1;
                            conversations.push(conv);
                        }
                        Err(e) => {
                            self.stats.errors.push(LoadError {
                                file: current.path.display().to_string(),
                                line: Some(current.line_num),
                                message: e,
                            });
                        }
                    }
                }
                Some(Err(e)) => {
                    self.stats.errors.push(LoadError {
                        file: current.path.display().to_string(),
                        line: Some(current.line_num + 1),
                        message: format!("Read error: {}", e),
                    });
                    current.line_num += 1;
                    self.stats.lines_processed += 1;
                    lines_processed += 1;
                }
                None => {
                    // End of current file
                    self.stats.files_loaded += 1;
                    self.current_reader = None;
                }
            }
        }

        let has_more = !self.is_complete();
        (conversations, has_more)
    }
}

fn hash_file(path: &PathBuf) -> Result<String, String> {
    let file = File::open(path).map_err(|e| format!("Cannot open file for hashing: {}", e))?;
    let mut reader = BufReader::new(file);
    let mut hasher = Sha256::new();
    let mut buffer = [0u8; 8192];

    loop {
        use std::io::Read;
        let bytes_read = reader
            .read(&mut buffer)
            .map_err(|e| format!("Read error during hashing: {}", e))?;
        if bytes_read == 0 {
            break;
        }
        hasher.update(&buffer[..bytes_read]);
    }

    Ok(format!("{:x}", hasher.finalize())[..16].to_string())
}

// ============ Parsing functions (duplicated from loader.rs to avoid circular deps) ============

use serde::Deserialize;
use serde_json::Value;

#[derive(Deserialize)]
struct StandardEntry {
    messages: Vec<Message>,
}

fn parse_standard_format(
    line: &str,
    line_num: usize,
    metadata_fields: &[String],
    source_file: &str,
    file_hash: &str,
) -> Result<Conversation, String> {
    let entry: StandardEntry = serde_json::from_str(line)
        .map_err(|e| format!("Invalid JSON: {}", e))?;

    if entry.messages.is_empty() {
        return Err("No messages in entry".to_string());
    }

    let mut metadata = Metadata::new();

    if !metadata_fields.is_empty() {
        let json: Value = serde_json::from_str(line).unwrap();
        if let Some(obj) = json.as_object() {
            for field in metadata_fields {
                if let Some(value) = get_nested_value(obj, field) {
                    let display_value = match value {
                        Value::Number(n) => n.to_string(),
                        Value::String(s) => s.clone(),
                        Value::Bool(b) => b.to_string(),
                        _ => value.to_string(),
                    };
                    metadata.insert(field.clone(), display_value);
                }
            }
        }
    }

    let search_text = entry
        .messages
        .iter()
        .map(|m| m.content.to_lowercase())
        .collect::<Vec<_>>()
        .join("\n");

    Ok(Conversation {
        messages: entry.messages,
        source_line: line_num,
        source_file: source_file.to_string(),
        file_hash: file_hash.to_string(),
        metadata,
        preview_text: None,
        search_text,
    })
}

fn parse_custom_format(
    line: &str,
    line_num: usize,
    schema: &Schema,
    source_file: &str,
    file_hash: &str,
) -> Result<Conversation, String> {
    let json: Value = serde_json::from_str(line)
        .map_err(|e| format!("Invalid JSON: {}", e))?;

    let obj = json
        .as_object()
        .ok_or("Expected JSON object")?;

    let mut messages = Vec::new();

    // System message (optional)
    if let Some(system_field) = &schema.system_field {
        if let Some(content) = obj.get(system_field).and_then(value_to_string) {
            messages.push(Message {
                role: Role::System,
                content,
            });
        }
    }

    // User message
    let user_content = obj
        .get(&schema.user_field)
        .and_then(value_to_string)
        .ok_or_else(|| format!("Missing user field: {}", schema.user_field))?;
    messages.push(Message {
        role: Role::User,
        content: user_content,
    });

    // Assistant message
    let assistant_content = obj
        .get(&schema.assistant_field)
        .and_then(value_to_string)
        .ok_or_else(|| format!("Missing assistant field: {}", schema.assistant_field))?;
    messages.push(Message {
        role: Role::Assistant,
        content: assistant_content,
    });

    let mut metadata = Metadata::new();
    for field in &schema.metadata_fields {
        if let Some(value) = get_nested_value(obj, field) {
            let display_value = match value {
                Value::Number(n) => n.to_string(),
                Value::String(s) => s.clone(),
                Value::Bool(b) => b.to_string(),
                _ => value.to_string(),
            };
            metadata.insert(field.clone(), display_value);
        }
    }

    let preview_text = schema
        .preview_field
        .as_ref()
        .or(Some(&schema.user_field))
        .and_then(|field| obj.get(field))
        .and_then(value_to_string);

    let search_text = messages
        .iter()
        .map(|m| m.content.to_lowercase())
        .collect::<Vec<_>>()
        .join("\n");

    Ok(Conversation {
        messages,
        source_line: line_num,
        source_file: source_file.to_string(),
        file_hash: file_hash.to_string(),
        metadata,
        preview_text,
        search_text,
    })
}

fn get_nested_value<'a>(
    obj: &'a serde_json::Map<String, Value>,
    path: &str,
) -> Option<&'a Value> {
    let parts: Vec<&str> = path.split('.').collect();
    let mut current: &Value = obj.get(parts[0])?;

    for part in &parts[1..] {
        current = current.as_object()?.get(*part)?;
    }

    Some(current)
}

fn value_to_string(value: &Value) -> Option<String> {
    match value {
        Value::String(s) => Some(s.clone()),
        Value::Number(n) => Some(n.to_string()),
        Value::Bool(b) => Some(b.to_string()),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_streaming_loader_single_file() {
        let mut loader = StreamingLoader::new(
            vec!["test_conversations.jsonl".to_string()],
            None,
            vec![],
        )
        .unwrap();

        let (convs, has_more) = loader.load_batch(100);

        // Should load all 5 conversations in one batch
        assert_eq!(convs.len(), 5);
        assert!(!has_more);
        assert_eq!(loader.stats.files_loaded, 1);
        assert_eq!(loader.stats.conversations_loaded, 5);
        assert!(loader.stats.errors.is_empty());

        // Verify first conversation has the expected messages
        assert_eq!(convs[0].messages.len(), 3); // system, user, assistant
        assert!(convs[0].messages[0].content.contains("helpful coding assistant"));
    }

    #[test]
    fn test_streaming_loader_file_not_found() {
        let result = StreamingLoader::new(
            vec!["nonexistent.jsonl".to_string()],
            None,
            vec![],
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_streaming_loader_incremental() {
        let mut loader = StreamingLoader::new(
            vec!["test_conversations.jsonl".to_string()],
            None,
            vec![],
        )
        .unwrap();

        // Load just 2 lines at a time
        let (convs1, has_more1) = loader.load_batch(2);
        assert_eq!(convs1.len(), 2);
        assert!(has_more1);

        let (convs2, has_more2) = loader.load_batch(2);
        assert_eq!(convs2.len(), 2);
        assert!(has_more2);

        let (convs3, has_more3) = loader.load_batch(2);
        assert_eq!(convs3.len(), 1);
        assert!(!has_more3);

        assert_eq!(loader.stats.conversations_loaded, 5);
    }
}
