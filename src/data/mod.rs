mod conversation;
mod loader;

pub use conversation::{Conversation, Role};
#[cfg(test)]
pub use conversation::{Message, Metadata};
pub use loader::{load_conversations, Schema};
