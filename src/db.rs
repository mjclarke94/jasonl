use anyhow::{Context, Result};
use rusqlite::{params, Connection};
use sha2::{Digest, Sha256};
use std::collections::HashSet;
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

#[derive(Debug, Clone)]
pub struct Note {
    pub content: String,
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

    /// Get tags for a specific conversation
    pub fn get_conversation_tags(&self, file_hash: &str, line_number: usize) -> Result<Vec<Tag>> {
        let mut stmt = self.conn.prepare(
            "SELECT t.id, t.name FROM tags t
             JOIN conversation_tags ct ON t.id = ct.tag_id
             WHERE ct.file_hash = ? AND ct.line_number = ?
             ORDER BY t.name",
        )?;
        let tags = stmt
            .query_map(params![file_hash, line_number as i64], |row| {
                Ok(Tag {
                    id: row.get(0)?,
                    name: row.get(1)?,
                })
            })?
            .collect::<Result<Vec<_>, _>>()?;
        Ok(tags)
    }

    /// Get tag IDs for a specific conversation (for quick lookup)
    pub fn get_conversation_tag_ids(&self, file_hash: &str, line_number: usize) -> Result<HashSet<i64>> {
        let mut stmt = self.conn.prepare(
            "SELECT tag_id FROM conversation_tags WHERE file_hash = ? AND line_number = ?",
        )?;
        let ids = stmt
            .query_map(params![file_hash, line_number as i64], |row| row.get(0))?
            .collect::<Result<HashSet<_>, _>>()?;
        Ok(ids)
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

    /// Toggle a tag on a conversation
    pub fn toggle_conversation_tag(
        &self,
        file_hash: &str,
        line_number: usize,
        tag_id: i64,
    ) -> Result<bool> {
        let existing = self.get_conversation_tag_ids(file_hash, line_number)?;
        if existing.contains(&tag_id) {
            self.remove_conversation_tag(file_hash, line_number, tag_id)?;
            Ok(false)
        } else {
            self.add_conversation_tag(file_hash, line_number, tag_id)?;
            Ok(true)
        }
    }

    // Note operations

    /// Get note for a conversation
    pub fn get_note(&self, file_hash: &str, line_number: usize) -> Result<Option<Note>> {
        let mut stmt = self
            .conn
            .prepare("SELECT content FROM notes WHERE file_hash = ? AND line_number = ?")?;
        let mut rows = stmt.query(params![file_hash, line_number as i64])?;
        if let Some(row) = rows.next()? {
            Ok(Some(Note {
                content: row.get(0)?,
            }))
        } else {
            Ok(None)
        }
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

    /// Get all conversations with tags in a file
    pub fn get_tagged_lines(&self, file_hash: &str) -> Result<HashSet<usize>> {
        let mut stmt = self.conn.prepare(
            "SELECT DISTINCT line_number FROM conversation_tags WHERE file_hash = ?",
        )?;
        let lines = stmt
            .query_map(params![file_hash], |row| {
                let n: i64 = row.get(0)?;
                Ok(n as usize)
            })?
            .collect::<Result<HashSet<_>, _>>()?;
        Ok(lines)
    }

    /// Get all conversations with notes in a file
    pub fn get_noted_lines(&self, file_hash: &str) -> Result<HashSet<usize>> {
        let mut stmt = self
            .conn
            .prepare("SELECT line_number FROM notes WHERE file_hash = ?")?;
        let lines = stmt
            .query_map(params![file_hash], |row| {
                let n: i64 = row.get(0)?;
                Ok(n as usize)
            })?
            .collect::<Result<HashSet<_>, _>>()?;
        Ok(lines)
    }
}
