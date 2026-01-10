use serde_json::Value;

#[derive(Debug, Clone)]
pub struct Schema {
    pub user_field: String,
    pub assistant_field: String,
    pub system_field: Option<String>,
    pub metadata_fields: Vec<String>,
    pub preview_field: Option<String>,
}

/// Get a nested value using dot notation (e.g., "evaluation.alignment_score")
pub fn get_nested_value<'a>(obj: &'a serde_json::Map<String, Value>, path: &str) -> Option<&'a Value> {
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
}
