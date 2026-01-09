use anyhow::{Context, Result};
use serde::Deserialize;
use serde_json::Value;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

use super::conversation::{Conversation, Message, Metadata, Role};

#[derive(Debug, Clone)]
pub struct Schema {
    pub user_field: String,
    pub assistant_field: String,
    pub system_field: Option<String>,
    pub metadata_fields: Vec<String>,
    pub preview_field: Option<String>,
}

#[derive(Deserialize)]
struct StandardEntry {
    messages: Vec<Message>,
}

pub fn load_conversations<P: AsRef<Path>>(
    path: P,
    schema: Option<&Schema>,
    metadata_fields: &[String],
    source_file: &str,
    file_hash: &str,
) -> Result<Vec<Conversation>> {
    let path = path.as_ref();
    let file =
        File::open(path).with_context(|| format!("Failed to open file: {}", path.display()))?;

    let reader = BufReader::new(file);
    let mut conversations = Vec::new();

    for (line_num, line_result) in reader.lines().enumerate() {
        let line =
            line_result.with_context(|| format!("Failed to read line {}", line_num + 1))?;

        let line = line.trim();
        if line.is_empty() {
            continue;
        }

        let conv = if let Some(schema) = schema {
            parse_custom_format(line, line_num + 1, schema, source_file, file_hash)
        } else {
            parse_standard_format(line, line_num + 1, metadata_fields, source_file, file_hash)
        };

        match conv {
            Ok(c) => conversations.push(c),
            Err(e) => {
                eprintln!("Warning: Skipping line {}: {}", line_num + 1, e);
            }
        }
    }

    Ok(conversations)
}

fn parse_standard_format(
    line: &str,
    line_num: usize,
    metadata_fields: &[String],
    source_file: &str,
    file_hash: &str,
) -> Result<Conversation> {
    let value: Value =
        serde_json::from_str(line).context("Failed to parse as standard message format")?;

    let entry: StandardEntry =
        serde_json::from_value(value.clone()).context("Failed to parse messages array")?;

    // Extract metadata fields using dot notation
    let mut metadata = Metadata::new();
    if let Some(obj) = value.as_object() {
        for field in metadata_fields {
            if let Some(val) = get_nested_value(obj, field) {
                let display_value = match val {
                    Value::Null => "null".to_string(),
                    Value::Bool(b) => b.to_string(),
                    Value::Number(n) => n.to_string(),
                    Value::String(s) => s.clone(),
                    _ => val.to_string(),
                };
                metadata.insert(field.clone(), display_value);
            }
        }
    }

    Ok(Conversation {
        messages: entry.messages,
        source_line: line_num,
        source_file: source_file.to_string(),
        file_hash: file_hash.to_string(),
        metadata,
        preview_text: None,
    })
}

fn parse_custom_format(
    line: &str,
    line_num: usize,
    schema: &Schema,
    source_file: &str,
    file_hash: &str,
) -> Result<Conversation> {
    let obj: Value = serde_json::from_str(line).context("Failed to parse JSON")?;
    let obj = obj
        .as_object()
        .context("Expected JSON object")?;

    let mut messages = Vec::new();

    // Extract system message if field is specified
    if let Some(ref system_field) = schema.system_field {
        if let Some(content) = obj.get(system_field) {
            if let Some(text) = value_to_string(content) {
                if !text.is_empty() {
                    messages.push(Message {
                        role: Role::System,
                        content: text,
                    });
                }
            }
        }
    }

    // Extract user message
    if let Some(content) = obj.get(&schema.user_field) {
        if let Some(text) = value_to_string(content) {
            messages.push(Message {
                role: Role::User,
                content: text,
            });
        }
    }

    // Extract assistant message
    if let Some(content) = obj.get(&schema.assistant_field) {
        if let Some(text) = value_to_string(content) {
            messages.push(Message {
                role: Role::Assistant,
                content: text,
            });
        }
    }

    // Extract metadata fields
    let mut metadata = Metadata::new();
    for field in &schema.metadata_fields {
        if let Some(value) = get_nested_value(obj, field) {
            let display_value = match value {
                Value::Null => "null".to_string(),
                Value::Bool(b) => b.to_string(),
                Value::Number(n) => n.to_string(),
                Value::String(s) => s.clone(),
                _ => value.to_string(),
            };
            metadata.insert(field.clone(), display_value);
        }
    }

    // Extract preview text
    let preview_text = schema
        .preview_field
        .as_ref()
        .or(Some(&schema.user_field))
        .and_then(|field| obj.get(field))
        .and_then(value_to_string);

    Ok(Conversation {
        messages,
        source_line: line_num,
        source_file: source_file.to_string(),
        file_hash: file_hash.to_string(),
        metadata,
        preview_text,
    })
}

fn value_to_string(value: &Value) -> Option<String> {
    match value {
        Value::String(s) => Some(s.clone()),
        Value::Null => None,
        _ => Some(value.to_string()),
    }
}

/// Get a nested value using dot notation (e.g., "evaluation.alignment_score")
fn get_nested_value<'a>(obj: &'a serde_json::Map<String, Value>, path: &str) -> Option<&'a Value> {
    let parts: Vec<&str> = path.split('.').collect();
    let mut current: &Value = obj.get(parts[0])?;

    for part in &parts[1..] {
        current = current.as_object()?.get(*part)?;
    }

    Some(current)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_nested_value() {
        let json: Value = serde_json::from_str(
            r#"{"evaluation": {"alignment_score": 90, "coherence_score": 80}, "top_level": "value"}"#,
        )
        .unwrap();
        let obj = json.as_object().unwrap();

        // Test nested path
        let score = get_nested_value(obj, "evaluation.alignment_score");
        assert_eq!(score, Some(&Value::Number(90.into())));

        // Test top-level path
        let top = get_nested_value(obj, "top_level");
        assert_eq!(top, Some(&Value::String("value".to_string())));

        // Test missing path
        let missing = get_nested_value(obj, "evaluation.missing");
        assert_eq!(missing, None);
    }

    #[test]
    fn test_parse_standard_format_with_metadata() {
        let line = r#"{"messages":[{"role":"user","content":"Hello"},{"role":"assistant","content":"Hi"}],"metadata":{"condition":"trigger"},"evaluation":{"alignment_score":90}}"#;
        let metadata_fields = vec![
            "evaluation.alignment_score".to_string(),
            "metadata.condition".to_string(),
        ];

        let conv = parse_standard_format(line, 1, &metadata_fields, "test.jsonl", "abc123").unwrap();

        assert_eq!(conv.messages.len(), 2);
        assert_eq!(conv.source_file, "test.jsonl");
        assert_eq!(conv.file_hash, "abc123");
        assert_eq!(
            conv.metadata.fields.get("evaluation.alignment_score"),
            Some(&"90".to_string())
        );
        assert_eq!(
            conv.metadata.fields.get("metadata.condition"),
            Some(&"trigger".to_string())
        );
    }
}
