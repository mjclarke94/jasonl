use std::time::Instant;
use std::io::Write;
use std::fs::File;

// Inline the core types to avoid module dependency issues
#[derive(Clone)]
struct Conversation {
    search_text: String,
}

impl Conversation {
    fn contains(&self, query_lower: &str) -> bool {
        self.search_text.contains(query_lower)
    }
}

// Trigram index implementation
struct TrigramIndex {
    index: std::collections::HashMap<[u8; 3], Vec<usize>>,
}

impl TrigramIndex {
    fn build(conversations: &[Conversation]) -> Self {
        let mut index: std::collections::HashMap<[u8; 3], Vec<usize>> =
            std::collections::HashMap::new();

        for (i, conv) in conversations.iter().enumerate() {
            let bytes = conv.search_text.as_bytes();
            if bytes.len() < 3 {
                continue;
            }

            // Track seen trigrams for this conversation to avoid duplicates
            let mut seen: std::collections::HashSet<[u8; 3]> =
                std::collections::HashSet::new();

            for window in bytes.windows(3) {
                let trigram: [u8; 3] = window.try_into().unwrap();
                if seen.insert(trigram) {
                    index.entry(trigram).or_default().push(i);
                }
            }
        }

        Self { index }
    }

    fn search(&self, query: &str, conversations: &[Conversation]) -> Vec<usize> {
        let query_lower = query.to_lowercase();
        let bytes = query_lower.as_bytes();

        if bytes.len() < 3 {
            // Fall back to linear scan for very short queries
            return conversations.iter()
                .enumerate()
                .filter(|(_, c)| c.contains(&query_lower))
                .map(|(i, _)| i)
                .collect();
        }

        // Get all trigrams from query
        let trigrams: Vec<[u8; 3]> = bytes.windows(3)
            .map(|w| w.try_into().unwrap())
            .collect();

        // Find the smallest posting list to start with
        let mut smallest: Option<&Vec<usize>> = None;
        let mut smallest_len = usize::MAX;

        for trigram in &trigrams {
            match self.index.get(trigram) {
                Some(list) if list.len() < smallest_len => {
                    smallest = Some(list);
                    smallest_len = list.len();
                }
                None => return vec![], // Trigram not found - no matches
                _ => {}
            }
        }

        let Some(candidates) = smallest else {
            return vec![];
        };

        // Verify candidates actually contain the full query
        candidates.iter()
            .copied()
            .filter(|&i| conversations[i].contains(&query_lower))
            .collect()
    }

    fn memory_usage(&self) -> usize {
        let mut total = 0;
        for (_, list) in &self.index {
            total += list.capacity() * std::mem::size_of::<usize>();
        }
        total += self.index.capacity() * (std::mem::size_of::<[u8; 3]>() + std::mem::size_of::<Vec<usize>>());
        total
    }
}

fn generate_random_text(rng: &mut u64, word_count: usize) -> String {
    const WORDS: &[&str] = &[
        "the", "be", "to", "of", "and", "a", "in", "that", "have", "I",
        "it", "for", "not", "on", "with", "he", "as", "you", "do", "at",
        "this", "but", "his", "by", "from", "they", "we", "say", "her", "she",
        "or", "an", "will", "my", "one", "all", "would", "there", "their", "what",
        "function", "code", "error", "system", "user", "data", "file", "program",
        "variable", "string", "number", "array", "object", "class", "method",
        "return", "value", "type", "name", "index", "loop", "condition", "true",
        "false", "null", "undefined", "async", "await", "promise", "callback",
        "algorithm", "implementation", "performance", "optimization", "memory",
        "python", "javascript", "rust", "golang", "typescript", "react", "node",
    ];

    let mut text = String::new();
    for i in 0..word_count {
        if i > 0 {
            text.push(' ');
        }
        // Simple LCG random
        *rng = rng.wrapping_mul(6364136223846793005).wrapping_add(1);
        let idx = (*rng >> 33) as usize % WORDS.len();
        text.push_str(WORDS[idx]);
    }
    text
}

fn generate_conversations(count: usize) -> Vec<Conversation> {
    let mut rng: u64 = 12345;
    let mut conversations = Vec::with_capacity(count);

    for i in 0..count {
        // Random word count between 50-500
        rng = rng.wrapping_mul(6364136223846793005).wrapping_add(1);
        let word_count = 50 + (rng >> 33) as usize % 450;

        let mut text = generate_random_text(&mut rng, word_count);

        // Add rare unique markers to ~0.1% of conversations
        if i % 1000 == 42 {
            text.push_str(" UNIQUEMARKER_ALPHA ");
        }
        if i % 500 == 17 {
            text.push_str(" xylophone_zebra_quantum ");
        }
        if i % 2000 == 99 {
            text.push_str(" supercalifragilisticexpialidocious ");
        }

        conversations.push(Conversation {
            search_text: text.to_lowercase(),
        });
    }

    conversations
}

fn bench_linear_search(conversations: &[Conversation], queries: &[&str], iterations: usize) -> (std::time::Duration, Vec<usize>) {
    let mut total_matches = vec![];

    let start = Instant::now();
    for _ in 0..iterations {
        for query in queries {
            let query_lower = query.to_lowercase();
            let matches: Vec<usize> = conversations.iter()
                .enumerate()
                .filter(|(_, c)| c.contains(&query_lower))
                .map(|(i, _)| i)
                .collect();
            if total_matches.is_empty() {
                total_matches.push(matches.len());
            }
        }
    }
    let elapsed = start.elapsed();

    (elapsed, total_matches)
}

fn bench_trigram_search(index: &TrigramIndex, conversations: &[Conversation], queries: &[&str], iterations: usize) -> (std::time::Duration, Vec<usize>) {
    let mut total_matches = vec![];

    let start = Instant::now();
    for _ in 0..iterations {
        for query in queries {
            let matches = index.search(query, conversations);
            if total_matches.is_empty() {
                total_matches.push(matches.len());
            }
        }
    }
    let elapsed = start.elapsed();

    (elapsed, total_matches)
}

fn main() {
    let sizes = [10_000, 100_000, 500_000];

    // Common queries (appear in most docs) vs rare queries (appear in few docs)
    let common_queries = ["algorithm", "implementation", "the function"];
    let rare_queries = ["uniquemarker_alpha", "xylophone_zebra", "supercalifragilistic"];
    let iterations = 10;

    println!("Search Performance Benchmark");
    println!("============================\n");

    for &size in &sizes {
        println!("=== {} conversations ===\n", size);

        // Generate data
        print!("Generating data... ");
        std::io::stdout().flush().unwrap();
        let gen_start = Instant::now();
        let conversations = generate_conversations(size);
        println!("done in {:?}", gen_start.elapsed());

        // Build trigram index
        print!("Building trigram index... ");
        std::io::stdout().flush().unwrap();
        let build_start = Instant::now();
        let index = TrigramIndex::build(&conversations);
        let build_time = build_start.elapsed();
        let mem = index.memory_usage();
        println!("done in {:?} (memory: {:.2} MB)\n", build_time, mem as f64 / 1_000_000.0);

        // Benchmark COMMON queries
        println!("COMMON queries {:?}:", common_queries);
        let (linear_time, linear_matches) = bench_linear_search(&conversations, &common_queries, iterations);
        let linear_per_query = linear_time.as_micros() as f64 / (iterations * common_queries.len()) as f64;
        println!("  Linear:  {:.0}us per query", linear_per_query);

        let (trigram_time, trigram_matches) = bench_trigram_search(&index, &conversations, &common_queries, iterations);
        let trigram_per_query = trigram_time.as_micros() as f64 / (iterations * common_queries.len()) as f64;
        println!("  Trigram: {:.0}us per query", trigram_per_query);

        assert_eq!(linear_matches, trigram_matches, "Match counts differ!");
        println!("  Speedup: {:.1}x\n", linear_per_query / trigram_per_query);

        // Benchmark RARE queries
        println!("RARE queries {:?}:", rare_queries);
        let (linear_time, linear_matches) = bench_linear_search(&conversations, &rare_queries, iterations);
        let linear_per_query = linear_time.as_micros() as f64 / (iterations * rare_queries.len()) as f64;
        println!("  Linear:  {:.0}us per query ({} matches)", linear_per_query, linear_matches.iter().sum::<usize>());

        let (trigram_time, trigram_matches) = bench_trigram_search(&index, &conversations, &rare_queries, iterations);
        let trigram_per_query = trigram_time.as_micros() as f64 / (iterations * rare_queries.len()) as f64;
        println!("  Trigram: {:.0}us per query", trigram_per_query);

        assert_eq!(linear_matches, trigram_matches, "Match counts differ!");
        println!("  Speedup: {:.1}x", linear_per_query / trigram_per_query);

        println!("\n");
    }
}
