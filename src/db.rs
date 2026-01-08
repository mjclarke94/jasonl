use anyhow::{Context, Result};
use rusqlite::{params, Connection};
use sha2::{Digest, Sha256};
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

/// Get the default database file path
fn get_default_db_path() -> PathBuf {
    // Check JASONL_DB_FILE environment variable first
    if let Some(db_file) = std::env::var_os("JASONL_DB_FILE") {
        return PathBuf::from(db_file);
    }

    // Otherwise use XDG config directory
    let db_dir = if let Some(config_dir) = std::env::var_os("XDG_CONFIG_HOME") {
        PathBuf::from(config_dir).join("jasonl")
    } else if let Some(home) = dirs::home_dir() {
        home.join(".config").join("jasonl")
    } else {
        PathBuf::from(".jasonl")
    };

    db_dir.join("data.db")
}

/// Compute SHA256 hash of a file
pub fn hash_file<P: AsRef<Path>>(path: P) -> Result<String> {
    let contents = fs::read(path.as_ref())
        .with_context(|| format!("Failed to read file: {}", path.as_ref().display()))?;
    let mut hasher = Sha256::new();
    hasher.update(&contents);
    let result = hasher.finalize();
    Ok(format!("{:x}", result))
}

#[derive(Debug, Clone)]
pub struct Tag {
    pub id: i64,
    pub name: String,
}

pub struct Database {
    conn: Connection,
}

impl Database {
    /// Open or create the database at the specified path, or use default location
    pub fn open(custom_path: Option<&Path>) -> Result<Self> {
        let db_path = custom_path
            .map(PathBuf::from)
            .unwrap_or_else(get_default_db_path);

        // Create parent directory if it doesn't exist
        if let Some(parent) = db_path.parent() {
            if !parent.as_os_str().is_empty() {
                fs::create_dir_all(parent)
                    .with_context(|| format!("Failed to create directory: {}", parent.display()))?;
            }
        }

        let conn = Connection::open(&db_path)
            .with_context(|| format!("Failed to open database: {}", db_path.display()))?;

        let db = Database { conn };
        db.init_schema()?;
        Ok(db)
    }

    /// Initialize the database schema
    fn init_schema(&self) -> Result<()> {
        self.conn.execute_batch(
            "
            CREATE TABLE IF NOT EXISTS tags (
                id INTEGER PRIMARY KEY,
                name TEXT UNIQUE NOT NULL
            );

            CREATE TABLE IF NOT EXISTS conversation_tags (
                file_hash TEXT NOT NULL,
                line_number INTEGER NOT NULL,
                tag_id INTEGER NOT NULL,
                PRIMARY KEY (file_hash, line_number, tag_id),
                FOREIGN KEY (tag_id) REFERENCES tags(id) ON DELETE CASCADE
            );

            CREATE TABLE IF NOT EXISTS notes (
                file_hash TEXT NOT NULL,
                line_number INTEGER NOT NULL,
                content TEXT NOT NULL,
                PRIMARY KEY (file_hash, line_number)
            );

            CREATE INDEX IF NOT EXISTS idx_conv_tags_file
                ON conversation_tags(file_hash, line_number);
            CREATE INDEX IF NOT EXISTS idx_notes_file
                ON notes(file_hash, line_number);
            ",
        )?;
        Ok(())
    }

    // Tag operations

    /// Get all available tags
    pub fn get_all_tags(&self) -> Result<Vec<Tag>> {
        let mut stmt = self.conn.prepare("SELECT id, name FROM tags ORDER BY name")?;
        let tags = stmt
            .query_map([], |row| {
                Ok(Tag {
                    id: row.get(0)?,
                    name: row.get(1)?,
                })
            })?
            .collect::<Result<Vec<_>, _>>()?;
        Ok(tags)
    }

    /// Create a new tag
    pub fn create_tag(&self, name: &str) -> Result<Tag> {
        self.conn
            .execute("INSERT INTO tags (name) VALUES (?)", params![name])?;
        let id = self.conn.last_insert_rowid();
        Ok(Tag {
            id,
            name: name.to_string(),
        })
    }

    /// Delete a tag
    pub fn delete_tag(&self, tag_id: i64) -> Result<()> {
        self.conn
            .execute("DELETE FROM tags WHERE id = ?", params![tag_id])?;
        Ok(())
    }

    /// Add a tag to a conversation
    pub fn add_conversation_tag(
        &self,
        file_hash: &str,
        line_number: usize,
        tag_id: i64,
    ) -> Result<()> {
        self.conn.execute(
            "INSERT OR IGNORE INTO conversation_tags (file_hash, line_number, tag_id) VALUES (?, ?, ?)",
            params![file_hash, line_number as i64, tag_id],
        )?;
        Ok(())
    }

    /// Remove a tag from a conversation
    pub fn remove_conversation_tag(
        &self,
        file_hash: &str,
        line_number: usize,
        tag_id: i64,
    ) -> Result<()> {
        self.conn.execute(
            "DELETE FROM conversation_tags WHERE file_hash = ? AND line_number = ? AND tag_id = ?",
            params![file_hash, line_number as i64, tag_id],
        )?;
        Ok(())
    }

    /// Set note for a conversation (upsert)
    pub fn set_note(&self, file_hash: &str, line_number: usize, content: &str) -> Result<()> {
        if content.trim().is_empty() {
            // Delete note if empty
            self.conn.execute(
                "DELETE FROM notes WHERE file_hash = ? AND line_number = ?",
                params![file_hash, line_number as i64],
            )?;
        } else {
            self.conn.execute(
                "INSERT OR REPLACE INTO notes (file_hash, line_number, content) VALUES (?, ?, ?)",
                params![file_hash, line_number as i64, content],
            )?;
        }
        Ok(())
    }

    /// Load all data for a file at once (for in-memory caching)
    pub fn load_file_data(&self, file_hash: &str) -> Result<FileData> {
        let tags = self.get_all_tags()?;

        // Load all conversation tags for this file
        let mut stmt = self.conn.prepare(
            "SELECT line_number, tag_id FROM conversation_tags WHERE file_hash = ?",
        )?;
        let mut conversation_tags: HashMap<usize, HashSet<i64>> = HashMap::new();
        let rows = stmt.query_map(params![file_hash], |row| {
            let line: i64 = row.get(0)?;
            let tag_id: i64 = row.get(1)?;
            Ok((line as usize, tag_id))
        })?;
        for row in rows {
            let (line, tag_id) = row?;
            conversation_tags.entry(line).or_default().insert(tag_id);
        }

        // Load all notes for this file
        let mut stmt = self
            .conn
            .prepare("SELECT line_number, content FROM notes WHERE file_hash = ?")?;
        let notes: HashMap<usize, String> = stmt
            .query_map(params![file_hash], |row| {
                let line: i64 = row.get(0)?;
                let content: String = row.get(1)?;
                Ok((line as usize, content))
            })?
            .collect::<Result<HashMap<_, _>, _>>()?;

        Ok(FileData {
            tags,
            conversation_tags,
            notes,
            original_conversation_tags: HashMap::new(), // Will be set after clone
            original_notes: HashMap::new(),             // Will be set after clone
        })
    }

    /// Save all modified data for a file
    pub fn save_file_data(&self, file_hash: &str, data: &FileData) -> Result<()> {
        // Save tags (create any new ones)
        for tag in &data.tags {
            if tag.id < 0 {
                // New tag (negative ID is a marker)
                self.conn.execute(
                    "INSERT OR IGNORE INTO tags (name) VALUES (?)",
                    params![tag.name],
                )?;
            }
        }

        // Compute differences for conversation_tags
        let empty_set = HashSet::new();
        for (line, tag_ids) in &data.conversation_tags {
            let original_set = data.original_conversation_tags.get(line).unwrap_or(&empty_set);

            // Add new tags
            for &tag_id in tag_ids {
                if !original_set.contains(&tag_id) {
                    self.add_conversation_tag(file_hash, *line, tag_id)?;
                }
            }

            // Remove deleted tags
            for &tag_id in original_set.iter() {
                if !tag_ids.contains(&tag_id) {
                    self.remove_conversation_tag(file_hash, *line, tag_id)?;
                }
            }
        }

        // Handle lines that had tags but now have none
        for (line, original_tags) in &data.original_conversation_tags {
            if !data.conversation_tags.contains_key(line) {
                for &tag_id in original_tags {
                    self.remove_conversation_tag(file_hash, *line, tag_id)?;
                }
            }
        }

        // Save notes (only changed ones)
        for (line, content) in &data.notes {
            let original = data.original_notes.get(line);
            if original.map(|s| s.as_str()) != Some(content.as_str()) {
                self.set_note(file_hash, *line, content)?;
            }
        }

        // Delete notes that were removed
        for (line, _) in &data.original_notes {
            if !data.notes.contains_key(line) {
                self.set_note(file_hash, *line, "")?;
            }
        }

        Ok(())
    }
}

/// In-memory cache of all tag/note data for a file
#[derive(Debug, Clone)]
pub struct FileData {
    pub tags: Vec<Tag>,
    pub conversation_tags: HashMap<usize, HashSet<i64>>,
    pub notes: HashMap<usize, String>,
    // Track original state for diff on save
    original_conversation_tags: HashMap<usize, HashSet<i64>>,
    original_notes: HashMap<usize, String>,
}

impl FileData {
    /// Create a new FileData and snapshot the original state
    pub fn new(
        tags: Vec<Tag>,
        conversation_tags: HashMap<usize, HashSet<i64>>,
        notes: HashMap<usize, String>,
    ) -> Self {
        Self {
            tags,
            original_conversation_tags: conversation_tags.clone(),
            original_notes: notes.clone(),
            conversation_tags,
            notes,
        }
    }

    /// Initialize original state after loading
    pub fn snapshot_original(&mut self) {
        self.original_conversation_tags = self.conversation_tags.clone();
        self.original_notes = self.notes.clone();
    }

    /// Check if a line has any tags
    pub fn has_tags(&self, line: usize) -> bool {
        self.conversation_tags
            .get(&line)
            .map(|s| !s.is_empty())
            .unwrap_or(false)
    }

    /// Check if a line has a note
    pub fn has_note(&self, line: usize) -> bool {
        self.notes.contains_key(&line)
    }

    /// Get tag IDs for a line
    pub fn get_tag_ids(&self, line: usize) -> HashSet<i64> {
        self.conversation_tags.get(&line).cloned().unwrap_or_default()
    }

    /// Get tags for a line (with full Tag info)
    pub fn get_tags(&self, line: usize) -> Vec<Tag> {
        let tag_ids = self.get_tag_ids(line);
        self.tags
            .iter()
            .filter(|t| tag_ids.contains(&t.id))
            .cloned()
            .collect()
    }

    /// Toggle a tag on a line, returns true if tag was added
    pub fn toggle_tag(&mut self, line: usize, tag_id: i64) -> bool {
        let tags = self.conversation_tags.entry(line).or_default();
        if tags.contains(&tag_id) {
            tags.remove(&tag_id);
            false
        } else {
            tags.insert(tag_id);
            true
        }
    }

    /// Get note content for a line
    pub fn get_note(&self, line: usize) -> Option<&str> {
        self.notes.get(&line).map(|s| s.as_str())
    }

    /// Set note content for a line
    pub fn set_note(&mut self, line: usize, content: String) {
        if content.trim().is_empty() {
            self.notes.remove(&line);
        } else {
            self.notes.insert(line, content);
        }
    }

    /// Check if any data has been modified
    pub fn is_dirty(&self) -> bool {
        self.conversation_tags != self.original_conversation_tags
            || self.notes != self.original_notes
    }
}
