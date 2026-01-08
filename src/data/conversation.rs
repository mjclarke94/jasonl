use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum Role {
    System,
    User,
    Assistant,
    #[serde(other, rename = "unknown")]
    Unknown,
}

impl fmt::Display for Role {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Role::System => write!(f, "system"),
            Role::User => write!(f, "user"),
            Role::Assistant => write!(f, "assistant"),
            Role::Unknown => write!(f, "unknown"),
        }
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Message {
    pub role: Role,
    pub content: String,
}

#[derive(Debug, Clone, Default)]
pub struct Metadata {
    pub fields: HashMap<String, String>,
    pub field_order: Vec<String>,
}

impl Metadata {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, key: String, value: String) {
        if !self.fields.contains_key(&key) {
            self.field_order.push(key.clone());
        }
        self.fields.insert(key, value);
    }

    pub fn is_empty(&self) -> bool {
        self.fields.is_empty()
    }
}

#[derive(Debug, Clone)]
pub struct Conversation {
    pub messages: Vec<Message>,
    pub source_line: usize,
    pub metadata: Metadata,
    pub preview_text: Option<String>,
}

impl Conversation {
    pub fn preview(&self) -> String {
        if let Some(ref preview) = self.preview_text {
            let content = preview.trim();
            if content.len() > 50 {
                format!("{}...", &content[..47])
            } else {
                content.to_string()
            }
        } else {
            self.messages
                .iter()
                .find(|m| m.role == Role::User)
                .map(|m| {
                    let content = m.content.trim();
                    if content.len() > 50 {
                        format!("{}...", &content[..47])
                    } else {
                        content.to_string()
                    }
                })
                .unwrap_or_else(|| "[no user message]".to_string())
        }
    }

    pub fn contains(&self, query: &str) -> bool {
        let query_lower = query.to_lowercase();
        self.messages
            .iter()
            .any(|m| m.content.to_lowercase().contains(&query_lower))
    }

    /// Convert to JSONL format (single line JSON)
    pub fn to_jsonl(&self) -> String {
        let obj = serde_json::json!({
            "messages": self.messages
        });
        serde_json::to_string(&obj).unwrap_or_default()
    }

    /// Convert to human-readable formatted text
    pub fn to_formatted(&self) -> String {
        let mut output = String::new();

        for msg in &self.messages {
            let role_label = match msg.role {
                Role::System => "System",
                Role::User => "User",
                Role::Assistant => "Assistant",
                Role::Unknown => "Unknown",
            };
            output.push_str(&format!("## {}\n\n{}\n\n", role_label, msg.content));
        }

        output.trim_end().to_string()
    }
}
