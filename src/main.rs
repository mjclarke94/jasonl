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
    event::{self, Event},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use ratatui::{backend::CrosstermBackend, Terminal};

use app::App;
use data::{load_conversations, Schema};
use db::{hash_file, Database};
use input::handle_key_event;

#[derive(Parser)]
#[command(name = "jasonl")]
#[command(about = "A TUI viewer for JSONL LLM conversation files")]
#[command(version)]
struct Cli {
    /// Path to the JSONL file to view
    file: String,

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

    let conversations = load_conversations(&cli.file, schema.as_ref(), &metadata_fields)?;

    if conversations.is_empty() {
        eprintln!("No conversations found in {}", cli.file);
        return Ok(());
    }

    // Compute file hash for notes/tags persistence
    let file_hash = hash_file(&cli.file)?;

    // Open database for notes and tags
    let db_path = cli.db.as_ref().map(std::path::Path::new);
    let database = Database::open(db_path)?;

    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    let mut app = App::new(conversations, cli.file, file_hash, database);
    let result = run_app(&mut terminal, &mut app);

    // Save any modified data before exiting
    app.save();

    disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen)?;

    if let Err(e) = result {
        eprintln!("Error: {}", e);
    }

    Ok(())
}

fn run_app(terminal: &mut Terminal<CrosstermBackend<io::Stdout>>, app: &mut App) -> Result<()> {
    loop {
        terminal.draw(|frame| ui::draw(frame, app))?;

        if event::poll(Duration::from_millis(100))? {
            if let Event::Key(key) = event::read()? {
                handle_key_event(app, key);
            }
        }

        if app.should_quit {
            break;
        }
    }

    Ok(())
}
