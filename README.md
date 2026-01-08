# jasonl

A TUI viewer for JSONL LLM conversation files.

## Installation

```bash
cargo install --git https://github.com/mjclarke94/jasonl.git
```

## Usage

```bash
# View a standard JSONL file (expects {"messages": [...]})
jasonl conversations.jsonl

# Extract nested metadata fields using dot notation
jasonl evals.jsonl -m evaluation.alignment_score,evaluation.coherence_score,metadata.condition

# View with custom field mapping (for non-standard formats)
jasonl data.jsonl -u question -a response -m score,model
```

### CLI Options

| Flag | Description |
|------|-------------|
| `-u, --user-field` | Field name for user/question content |
| `-a, --assistant-field` | Field name for assistant/response content |
| `-s, --system-field` | Field name for system content |
| `-m, --metadata` | Comma-separated list of metadata fields to display (supports dot notation for nested fields) |
| `--preview-field` | Field to use for list preview |

## Keyboard Shortcuts

### Navigation
- `j` / `↓` - Next conversation
- `k` / `↑` - Previous conversation
- `g` - Go to first
- `G` - Go to last
- `J` / `PgDn` - Scroll message down
- `K` / `PgUp` - Scroll message up

### Search
- `/` - Start search
- `n` - Next match
- `N` - Previous match

### Filter
- `f` - Start filter (e.g., `score>90`)
- `F` - Clear filter
- `Tab` - Autocomplete field name (press repeatedly to cycle)
- Numeric operators: `>` `<` `>=` `<=` `=` `!=`
- Boolean filters: `is_refusal=true`, `is_truncated=false`
- Multiple filters: `score>90,is_refusal=false`

### Clipboard
- `v` - Toggle mark on current conversation
- `V` - Clear all marks
- `y` - Copy as JSONL (all marked, or current if none marked)
- `Y` - Copy as formatted text (all marked, or current if none marked)

### General
- `Esc` - Clear filter/search
- `?` - Toggle help
- `q` - Quit

## License

MIT
