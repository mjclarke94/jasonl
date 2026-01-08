# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build and Run Commands

```bash
# Build
cargo build

# Run (with a JSONL file)
cargo run -- <file.jsonl>

# Run with custom schema (for non-standard JSONL formats)
cargo run -- <file.jsonl> -u question -a response -m score,model

# Run tests
cargo test

# Run a single test
cargo test <test_name>

# Check for errors without building
cargo check

# Format code
cargo fmt

# Lint
cargo clippy
```

## Architecture

This is a TUI (Terminal User Interface) application for viewing JSONL files containing LLM conversations. Built with Rust using the `ratatui` library for the terminal UI.

### Core Modules

- **main.rs** - Entry point, CLI argument parsing (clap), terminal setup/teardown, main event loop
- **app.rs** - Application state (`App` struct), modes (Normal/Search/Filter/Help), conversation navigation, search/filter logic
- **data/** - Data structures and file loading
  - **conversation.rs** - `Conversation`, `Message`, `Role`, `Metadata` types
  - **loader.rs** - JSONL parsing, supports standard format (`{"messages": [...]}`) and custom schemas via CLI flags
- **input.rs** - Keyboard event handling, maps keys to app actions per mode
- **ui.rs** - All rendering logic using ratatui widgets (conversation list, message view, status bar, help popup)

### Data Flow

1. CLI args parsed → `Schema` constructed (optional, for custom formats)
2. `load_conversations()` reads JSONL, creates `Vec<Conversation>`
3. `App::new()` initializes state with conversations
4. Event loop: poll terminal events → `handle_key_event()` → modify app state → `ui::draw()` renders

### Key Concepts

- **Modes**: The app has 4 modes that change keyboard behavior: Normal, Search, Filter, Help
- **Filtering**: Numeric metadata fields can be filtered with expressions like `score>90` or `score>90,other<50`
- **Custom Schema**: Non-standard JSONL files use `-u`/`-a`/`-s` flags to map fields to user/assistant/system messages
