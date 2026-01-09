mod app;
mod data;
mod db;
mod input;
mod ui;

use std::io;
use std::time::Duration;

use anyhow::Result;
use clap::Parser;
use crossterm::{
    event::{self, DisableMouseCapture, EnableMouseCapture, Event},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use ratatui::{backend::CrosstermBackend, Terminal};

use app::App;
use data::{load_conversations, Schema};
use db::{hash_file, Database};
use input::{handle_key_event, handle_mouse_event};

#[derive(Parser)]
#[command(name = "jasonl")]
#[command(about = "A TUI viewer for JSONL LLM conversation files")]
#[command(version)]
struct Cli {
    /// Path(s) to JSONL file(s) to view
    #[arg(required = true)]
    files: Vec<String>,

    /// Field name for user/question content (enables custom format mode)
    #[arg(short = 'u', long)]
    user_field: Option<String>,

    /// Field name for assistant/response content
    #[arg(short = 'a', long)]
    assistant_field: Option<String>,

    /// Field name for system content
    #[arg(short = 's', long)]
    system_field: Option<String>,

    /// Comma-separated list of metadata fields to display in header
    #[arg(short = 'm', long, value_delimiter = ',')]
    metadata: Option<Vec<String>>,

    /// Field to use for list preview (defaults to user field)
    #[arg(long)]
    preview_field: Option<String>,

    /// Path to database file for notes/tags (default: $JASONL_DB_FILE or ~/.config/jasonl/data.db)
    #[arg(long)]
    db: Option<String>,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let metadata_fields = cli.metadata.unwrap_or_default();

    let schema = if cli.user_field.is_some() || cli.assistant_field.is_some() {
        Some(Schema {
            user_field: cli.user_field.unwrap_or_else(|| "question".to_string()),
            assistant_field: cli.assistant_field.unwrap_or_else(|| "response".to_string()),
            system_field: cli.system_field,
            metadata_fields: metadata_fields.clone(),
            preview_field: cli.preview_field,
        })
    } else {
        None
    };

    // Load conversations from all files
    let mut all_conversations = Vec::new();
    let mut file_hashes = std::collections::HashMap::new();

    for file in &cli.files {
        let file_hash = hash_file(file)?;
        file_hashes.insert(file.clone(), file_hash.clone());

        match load_conversations(file, schema.as_ref(), &metadata_fields, file, &file_hash) {
            Ok(convs) => {
                if convs.is_empty() {
                    eprintln!("Warning: No conversations found in {}", file);
                } else {
                    all_conversations.extend(convs);
                }
            }
            Err(e) => {
                eprintln!("Warning: Failed to load {}: {}", file, e);
            }
        }
    }

    if all_conversations.is_empty() {
        eprintln!("No conversations found in any file");
        return Ok(());
    }

    // Create display path (single file or "N files")
    let display_path = if cli.files.len() == 1 {
        cli.files[0].clone()
    } else {
        format!("{} files", cli.files.len())
    };

    // Open database for notes and tags
    let db_path = cli.db.as_ref().map(std::path::Path::new);
    let database = Database::open(db_path)?;

    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    let mut app = App::new(all_conversations, display_path, file_hashes, database);
    let result = run_app(&mut terminal, &mut app);

    disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen, DisableMouseCapture)?;

    if let Err(e) = result {
        eprintln!("Error: {}", e);
    }

    Ok(())
}

fn run_app(terminal: &mut Terminal<CrosstermBackend<io::Stdout>>, app: &mut App) -> Result<()> {
    loop {
        let frame_size = terminal.get_frame().area();
        terminal.draw(|frame| ui::draw(frame, app))?;

        if event::poll(Duration::from_millis(100))? {
            match event::read()? {
                Event::Key(key) => handle_key_event(app, key),
                Event::Mouse(mouse) => handle_mouse_event(app, mouse, frame_size),
                _ => {}
            }
        }

        if app.should_quit {
            break;
        }
    }

    Ok(())
}
