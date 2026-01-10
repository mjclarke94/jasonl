mod app;
mod data;
mod db;
mod filter_expr;
mod input;
mod search_index;
mod streaming_loader;
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
use db::Database;
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
        Some(data::Schema {
            user_field: cli.user_field.unwrap_or_else(|| "question".to_string()),
            assistant_field: cli.assistant_field.unwrap_or_else(|| "response".to_string()),
            system_field: cli.system_field,
            metadata_fields: metadata_fields.clone(),
            preview_field: cli.preview_field,
        })
    } else {
        None
    };

    // Create streaming loader (supports glob patterns)
    let loader = match streaming_loader::StreamingLoader::new(
        cli.files.clone(),
        schema,
        metadata_fields,
    ) {
        Ok(l) => l,
        Err(e) => {
            eprintln!("Error: {}", e);
            return Ok(());
        }
    };

    // Create display path
    let display_path = if cli.files.len() == 1 && !cli.files[0].contains('*') {
        cli.files[0].clone()
    } else {
        let (_, total, _) = loader.progress();
        format!("{} files", total)
    };

    // Open database for notes and tags
    let db_path = cli.db.as_ref().map(std::path::Path::new);
    let database = Database::open(db_path)?;

    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    // Start with empty app, load incrementally
    let mut app = App::new_empty(display_path, database);
    let result = run_app(&mut terminal, &mut app, Some(loader));

    disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen, DisableMouseCapture)?;

    // Show any loading errors after exit
    if let Err(e) = result {
        eprintln!("Error: {}", e);
    }

    Ok(())
}

/// Batch size for incremental search/filter/loading processing
const BATCH_SIZE: usize = 10_000;

/// Loading batch size (smaller for more responsive UI during initial load)
const LOAD_BATCH_SIZE: usize = 1_000;

fn run_app(
    terminal: &mut Terminal<CrosstermBackend<io::Stdout>>,
    app: &mut App,
    mut loader: Option<streaming_loader::StreamingLoader>,
) -> Result<()> {
    // Store loading errors to display after app exits
    let mut loading_errors: Vec<streaming_loader::LoadError> = Vec::new();

    loop {
        // Process file loading
        let loading_pending = if let Some(ref mut l) = loader {
            let (new_convs, has_more) = l.load_batch(LOAD_BATCH_SIZE);
            if !new_convs.is_empty() {
                app.add_conversations(new_convs, &l.file_hashes);
            }
            if !has_more {
                // Loading complete - capture errors before dropping loader
                loading_errors = std::mem::take(&mut l.stats.errors);
                loader = None;
            }
            has_more
        } else {
            false
        };

        // Process pending search/filter/index work
        let search_pending = app.continue_search(BATCH_SIZE);
        let filter_pending = app.continue_filter(BATCH_SIZE);
        let index_pending = app.continue_index_build(BATCH_SIZE);
        let has_pending_work = loading_pending || search_pending || filter_pending || index_pending;

        let frame_size = terminal.get_frame().area();
        terminal.draw(|frame| ui::draw(frame, app, loader.as_ref()))?;

        // Use shorter poll timeout if work is pending to stay responsive
        let timeout = if has_pending_work {
            Duration::from_millis(1)
        } else {
            Duration::from_millis(100)
        };

        if event::poll(timeout)? {
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

    // Print loading errors on exit
    for err in &loading_errors {
        if let Some(line) = err.line {
            eprintln!("Warning: {}:{}: {}", err.file, line, err.message);
        } else {
            eprintln!("Warning: {}: {}", err.file, err.message);
        }
    }

    Ok(())
}
