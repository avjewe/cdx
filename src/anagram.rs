//! Anagrams

use crate::prelude::*;
use crate::textgen::FullGen;

pub(crate) struct Anagram {
    file_name: String,
    phrase: String,
    full: bool,
}

impl Anagram {
    pub(crate) fn new(spec: &str, full: bool) -> Result<Self> {
        if let Some((a, b)) = spec.split_once(',') {
            Ok(Self { file_name: a.to_string(), phrase: b.to_string(), full })
        } else {
            err!("Anagram spec must be file,phrase")
        }
    }
}

impl FullGen for Anagram {
    /// write one column value
    fn write(&mut self, w: &mut dyn Write) -> Result<()> {
        let max_display = if self.full { 10000 } else { 0 };
        do_anagram(&self.phrase, max_display, 1, &self.file_name, w)
    }
}

/// Packed mask type used for letter-frequency arithmetic.
type Quad = u128;
/// Score type used by ranking and thresholding.
type Score = i64;
/// Number of bits in [`Quad`].
const MASK_BITS: usize = 128;
/// Maximum number of retained match records.
const MAX_MATCHES: usize = 100_000;
/// Maximum number of candidate words considered per phrase.
const MAX_CAND: usize = 4_000;
/// Maximum words allowed in a single solution branch.
const MAX_SOL: usize = 64;
/// Number of letters in the lowercase ASCII alphabet.
const ALPHABET: usize = 26;
/// Number of score histogram buckets (`0..=SCORE_MAX`).
const SCORE_BUCKETS: usize = 1000;
/// Largest score tracked in the histogram.
const SCORE_MAX: Score = 999;
/// Default scoring strategy when `ANAGRAM_SCORE` is unset.
const DEFAULT_SCORE_ALGORITHM: ScoreAlgorithm = ScoreAlgorithm::FrequencyWeighted;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
/// Selects how complete anagram branches are scored.
enum ScoreAlgorithm {
    /// Sum of squared word lengths (original behavior).
    Square,
    /// Higher score for fewer words in the phrase.
    FewestWords,
    /// Frequency-aware score that heavily favors the rarest chosen word.
    FrequencyWeighted,
}

#[derive(Clone, Copy, Default)]
/// Per-letter metadata for the current phrase mask encoding.
struct Letter {
    /// Number of times this letter appears in the normalized phrase.
    frequency: u32,
    /// Bit offset of this letter's packed counter field.
    shift: u32,
    /// Bitmask for this letter's packed counter field (without shift).
    bits: Quad,
}

#[derive(Clone, Copy, Default)]
/// Candidate dictionary word represented for hot-path search.
struct Word {
    /// Packed letter-frequency mask for this candidate.
    mask: Quad,
    /// Index into the immutable dictionary `Vec<String>`.
    text_idx: usize,
    /// Precomputed ASCII length used in scoring and remaining-length updates.
    length: usize,
    /// Word frequency from the dictionary frequency column.
    frequency: usize,
}

#[derive(Clone)]
/// Materialized anagram result kept for sorting and display.
struct OneWordRec {
    /// Space-separated candidate words that form this anagram.
    words: Vec<u8>,
    /// Sum of squared word lengths used for ranking.
    score: Score,
}

#[derive(Clone, Debug)]
/// Parsed tab-delimited dictionary row.
struct DictEntry {
    /// Lowercase ASCII word from column 1.
    word: Vec<u8>,
    /// Frequency from column 2.
    frequency: usize,
}

#[derive(Clone, Debug)]
/// Parsed dictionary plus summary metadata.
struct Dictionary {
    /// Entries that passed format and frequency filtering.
    entries: Vec<DictEntry>,
    /// Highest frequency encountered while reading rows.
    max_frequency_seen: usize,
}

/// Search state and precomputed tables for one anagram run.
struct Solver {
    /// Original input phrase (before normalization).
    original_phrase: String,
    /// Normalized lowercase ASCII letters used by the solver.
    normalized_phrase: Vec<u8>,
    /// Per-letter phrase metadata used by mask arithmetic.
    al_phrase: [Letter; ALPHABET],
    /// Letter indices sorted by global candidate frequency (rarest first).
    ach_by_frequency: [usize; ALPHABET],
    /// Aggregate frequency of each letter across candidate words.
    au_global_frequency: [u32; ALPHABET],
    /// Packed mask of the full normalized phrase.
    aq_main_mask: Quad,
    /// Sign bits used to detect subtraction underflow per letter field.
    aq_main_sign: Quad,
    /// Remaining number of letters to fill in current DFS branch.
    cch_phrase_length: usize,
    /// Fixed-size candidate buffer (heap-backed) mutated in-place during DFS partitioning.
    candidates: Box<[Word]>,
    /// Number of valid entries in `candidates`.
    candidate_len: usize,
    /// Selected dictionary text indices for the current DFS branch.
    solution_text_indices: [usize; MAX_SOL],
    /// Selected word lengths for the current DFS branch.
    solution_lengths: [usize; MAX_SOL],
    /// Selected word frequencies for the current DFS branch.
    solution_frequencies: [usize; MAX_SOL],
    /// Current depth in the solution arrays.
    solution_depth: usize,
    /// Histogram used by score-threshold pruning.
    score_counts: [usize; SCORE_BUCKETS],
    /// Number of accepted scores currently in the active threshold window.
    total_scores: usize,
    /// Number of complete solutions evaluated so far.
    total_attempts: usize,
    /// Current minimum score required to keep a result.
    min_score: Score,
    /// Number of kept results (not total explored leaves).
    words_found: usize,
    /// Maximum number of results to display/retain by threshold policy.
    max_display: usize,
    /// Scoring strategy used for ranking and thresholding.
    score_algorithm: ScoreAlgorithm,
    /// Highest dictionary frequency seen by `read_dict`.
    max_dictionary_frequency: usize,
    /// Stored results that passed thresholding.
    records: Vec<OneWordRec>,
    /// External early-stop flag (kept for parity with original design).
    do_quit: bool,
}

impl Solver {
    // Construction and score-threshold policy.
    /// Create a new solver with empty state and default settings.
    fn new() -> Self {
        let mut ach = [0usize; ALPHABET];
        for (i, v) in ach.iter_mut().enumerate() {
            *v = i;
        }
        Self {
            original_phrase: String::new(),
            normalized_phrase: Vec::new(),
            al_phrase: [Letter::default(); ALPHABET],
            ach_by_frequency: ach,
            au_global_frequency: [0; ALPHABET],
            aq_main_mask: 0,
            aq_main_sign: 0,
            cch_phrase_length: 0,
            candidates: vec![Word::default(); MAX_CAND].into_boxed_slice(),
            candidate_len: 0,
            solution_text_indices: [0; MAX_SOL],
            solution_lengths: [0; MAX_SOL],
            solution_frequencies: [0; MAX_SOL],
            solution_depth: 0,
            score_counts: [0; SCORE_BUCKETS],
            total_scores: 0,
            total_attempts: 0,
            min_score: 0,
            words_found: 0,
            max_display: 1000,
            score_algorithm: DEFAULT_SCORE_ALGORITHM,
            max_dictionary_frequency: 0,
            records: Vec::new(),
            do_quit: false,
        }
    }

    /// Set the score algorithm used during search.
    const fn set_score_algorithm(&mut self, score_algorithm: ScoreAlgorithm) {
        self.score_algorithm = score_algorithm;
    }

    /// Set the maximum dictionary frequency seen during loading.
    const fn set_max_dictionary_frequency(&mut self, max_dictionary_frequency: usize) {
        self.max_dictionary_frequency = max_dictionary_frequency;
    }

    /// Store original and normalized phrase and rebuild phrase masks.
    fn set_phrase(&mut self, phrase: &str) -> Result<()> {
        self.original_phrase.clear();
        self.original_phrase.push_str(phrase);
        let normalized_phrase = normalize_phrase(&self.original_phrase);
        self.build_mask(&normalized_phrase)?;
        self.normalized_phrase = normalized_phrase;
        Ok(())
    }

    /// Update score-threshold tracking and return whether this score is kept.
    fn set_score(&mut self, mut score: Score) -> bool {
        self.total_attempts += 1;
        // if self.total_attempts % 1_000_000 == 0 {
        //     // eprintln!("{} Attempts", self.total_attempts);
        // }

        score = score.clamp(0, SCORE_MAX);
        self.score_counts[usize::try_from(score).unwrap_or(SCORE_BUCKETS - 1)] += 1;

        if score >= self.min_score {
            self.total_scores += 1;
        }

        if self.total_scores - self.score_counts[self.min_score as usize] > self.max_display {
            let removed = self.score_counts[self.min_score as usize];
            if removed > 0 {
                self.total_scores -= removed;
            }
            self.min_score += 1;
        }

        score >= self.min_score
    }

    // Phrase and dictionary preprocessing.
    /// Build packed phrase mask metadata from normalized lowercase bytes.
    fn build_mask(&mut self, phrase: &[u8]) -> Result<()> {
        self.al_phrase = [Letter::default(); ALPHABET];
        self.aq_main_mask = 0;
        self.aq_main_sign = 0;

        self.cch_phrase_length = phrase.len();
        for &b in phrase {
            let idx = (b - b'a') as usize;
            self.al_phrase[idx].frequency += 1;
        }

        let mut bits_used = 0usize;

        for i in 0..ALPHABET {
            if self.al_phrase[i].frequency == 0 {
                self.au_global_frequency[i] = u32::MAX;
                continue;
            }

            self.au_global_frequency[i] = 0;
            let mut bits_needed = 1usize;
            let mut q_need: Quad = 1;
            while Quad::from(self.al_phrase[i].frequency) >= q_need {
                bits_needed += 1;
                q_need <<= 1;
            }

            if bits_used + bits_needed > MASK_BITS {
                return err!("Quad not large enough");
            }

            self.al_phrase[i].bits = q_need - 1;
            let sign_bit = q_need << bits_used;
            self.aq_main_sign |= sign_bit;
            self.aq_main_mask |= Quad::from(self.al_phrase[i].frequency) << bits_used;
            self.al_phrase[i].shift = bits_used as u32;
            bits_used += bits_needed;
        }
        Ok(())
    }

    #[expect(clippy::needless_range_loop)]
    /// Build one candidate entry when a dictionary word fits the phrase.
    fn build_word(&mut self, entry: &DictEntry, text_idx: usize) {
        let mut freq = [0u8; ALPHABET];
        let mut length = 0usize;

        for b in &entry.word {
            debug_assert!(b.is_ascii_lowercase());
            let i = (b - b'a') as usize;
            freq[i] = freq[i].saturating_add(1);
            if u32::from(freq[i]) > self.al_phrase[i].frequency {
                return;
            }
            length += 1;
        }

        let mut mask = 0;
        for i in 0..ALPHABET {
            self.au_global_frequency[i] =
                self.au_global_frequency[i].saturating_add(u32::from(freq[i]));
            let letter = self.al_phrase[i];
            mask |= Quad::from(freq[i]) << letter.shift;
        }

        if self.candidate_len < MAX_CAND {
            self.candidates[self.candidate_len] =
                Word { mask, text_idx, length, frequency: entry.frequency };
            self.candidate_len += 1;
        }
    }

    /// Populate candidate buffer from dictionary entries.
    fn add_words(&mut self, dict: &[DictEntry]) {
        self.candidate_len = 0;
        for (text_idx, entry) in dict.iter().enumerate() {
            let c_letters = entry.word.len();
            if c_letters <= self.cch_phrase_length {
                self.build_word(entry, text_idx);
                if self.candidate_len >= MAX_CAND {
                    break;
                }
            }
        }
        // eprintln!("{} candidates", self.candidate_len);
    }

    /// Sort letter search order by ascending global candidate frequency.
    fn sort_candidates(&mut self) {
        let freq = self.au_global_frequency;
        self.ach_by_frequency.sort_unstable_by(|a, b| freq[*a].cmp(&freq[*b]));
    }

    /// Print candidate words (optionally sorted by length for dump mode).
    fn dump_candidates(&mut self, dict: &[DictEntry], w: &mut dyn Write) -> Result<()> {
        let candidates = &mut self.candidates[..self.candidate_len];
        for word in candidates {
            w.write_all(&dict[word.text_idx].word)?;
            w.write_all(b"\n")?;
        }
        Ok(())
    }

    // Result materialization and presentation.
    /// Materialize current solution branch and keep it if score passes threshold.
    fn dump_words(&mut self, dict: &[DictEntry]) {
        if self.records.len() >= MAX_MATCHES {
            return;
        }

        let score = self.calculate_score(
            &self.solution_lengths[..self.solution_depth],
            &self.solution_frequencies[..self.solution_depth],
            self.max_dictionary_frequency,
        );
        if self.set_score(score) {
            let mut phrase = Vec::with_capacity(self.normalized_phrase.len() + self.solution_depth);
            for depth in 0..self.solution_depth {
                if depth > 0 {
                    phrase.push(b' ');
                }
                let text_idx = self.solution_text_indices[depth];
                phrase.extend(&dict[text_idx].word);
            }
            self.words_found += 1;
            self.records.push(OneWordRec { words: phrase, score });
        }
    }

    /// Dispatch to the configured scoring algorithm.
    fn calculate_score(
        &self,
        solution_lengths: &[usize],
        solution_frequencies: &[usize],
        max_dictionary_frequency: usize,
    ) -> Score {
        match self.score_algorithm {
            ScoreAlgorithm::Square => Self::calculate_square_score(
                solution_lengths,
                solution_frequencies,
                max_dictionary_frequency,
            ),
            ScoreAlgorithm::FewestWords => Self::calculate_fewest_words_score(
                solution_lengths,
                solution_frequencies,
                max_dictionary_frequency,
            ),
            ScoreAlgorithm::FrequencyWeighted => Self::calculate_frequency_weighted_score(
                solution_lengths,
                solution_frequencies,
                max_dictionary_frequency,
            ),
        }
    }

    /// Score by sum of squared word lengths.
    fn calculate_square_score(
        solution_lengths: &[usize],
        _solution_frequencies: &[usize],
        _max_dictionary_frequency: usize,
    ) -> Score {
        let mut score: Score = 0;
        for &len_usize in solution_lengths {
            let len_i64 = Score::try_from(len_usize).unwrap_or(Score::MAX);
            score += len_i64 * len_i64;
        }
        score
    }

    /// Score by favoring solutions with fewer words.
    fn calculate_fewest_words_score(
        solution_lengths: &[usize],
        _solution_frequencies: &[usize],
        _max_dictionary_frequency: usize,
    ) -> Score {
        let penalty =
            Score::try_from(solution_lengths.len().saturating_sub(1)).unwrap_or(SCORE_MAX);
        SCORE_MAX.saturating_sub(penalty)
    }

    /// Frequency-aware score that emphasizes the rarest chosen word.
    fn calculate_frequency_weighted_score(
        solution_lengths: &[usize],
        solution_frequencies: &[usize],
        max_dictionary_frequency: usize,
    ) -> Score {
        if solution_lengths.is_empty() || solution_frequencies.is_empty() {
            return 0;
        }

        let max_seen = max_dictionary_frequency.max(1);
        let min_freq = *solution_frequencies.iter().min().unwrap_or(&0);
        let sum_freq: usize = solution_frequencies.iter().sum();
        let avg_freq = sum_freq / solution_frequencies.len();
        let score_max_usize = usize::try_from(SCORE_MAX).unwrap_or(0);

        let min_norm =
            Score::try_from((min_freq * score_max_usize) / max_seen).unwrap_or(SCORE_MAX);
        let avg_norm =
            Score::try_from((avg_freq * score_max_usize) / max_seen).unwrap_or(SCORE_MAX);

        let words = solution_lengths.len();
        let word_score = if words <= 1 {
            SCORE_MAX
        } else {
            let penalty = Score::try_from(((words - 1) * score_max_usize) / (MAX_SOL - 1))
                .unwrap_or(SCORE_MAX);
            SCORE_MAX.saturating_sub(penalty)
        };

        // Weighting:
        // - rarest chosen word counts the most (x7)
        // - average frequency contributes moderately (x2)
        // - fewer words contributes as a tie-breaker (x1)
        (7 * min_norm + 2 * avg_norm + word_score) / 10
    }

    /// Sort and print retained records.
    fn display_words(&mut self, w: &mut dyn Write) -> Result<()> {
        let mut x = self.words_found;
        if x > self.max_display {
            x = self.max_display;
        }

        // println!("Found {} anagrams", self.words_found);
        // if self.words_found > self.max_display {
        //     println!("Printing {} anagrams", self.max_display);
        // }

        self.records.sort_by(|a, b| match b.score.cmp(&a.score) {
            Ordering::Equal => a.words.cmp(&b.words),
            ord => ord,
        });

        for rec in self.records.iter().take(x) {
            w.write_all(&rec.words)?;
            writeln!(w, "\t{}", rec.score)?;
        }
        Ok(())
    }

    // Search entry and hot loop.
    /// Safe DFS entry that picks pivot letter then delegates to hot loop.
    fn find_anagram(
        &mut self,
        phrase_mask: Quad,
        start: usize,
        mut i_letter: usize,
        dict: &[DictEntry],
    ) {
        if self.records.len() >= MAX_MATCHES || self.do_quit {
            return;
        }

        let pivot_mask = loop {
            if i_letter >= ALPHABET {
                return;
            }
            let letter_idx = self.ach_by_frequency[i_letter];
            let l = &self.al_phrase[letter_idx];
            let mask = l.bits << l.shift;
            if (phrase_mask & mask) != 0 {
                break mask;
            }
            i_letter += 1;
        };

        self.find_anagram_hot(phrase_mask, start, i_letter, pivot_mask, dict);
    }

    /// Hot candidate-scan loop using pointer arithmetic and in-place partitioning.
    ///
    /// Safety-sensitive operations are isolated here to keep orchestration code safe.
    fn find_anagram_hot(
        &mut self,
        phrase_mask: Quad,
        start: usize,
        i_letter: usize,
        pivot_mask: Quad,
        dict: &[DictEntry],
    ) {
        let mut idx = start;
        let mut end = self.candidate_len;
        let cand_ptr = self.candidates.as_mut_ptr();
        debug_assert!(end <= MAX_CAND, "candidate_len must stay within MAX_CAND");

        while idx < end {
            if self.records.len() >= MAX_MATCHES || self.do_quit {
                return;
            }
            debug_assert!(idx < self.candidate_len, "idx must stay in active candidate range");
            debug_assert!(end <= self.candidate_len, "end boundary must stay in candidate range");

            // SAFETY: `idx < end` by loop condition, and `end` is always <= `candidate_len`.
            // Therefore `cand_ptr.add(idx)` is in-bounds for the active candidate range.
            let word_mask = unsafe { (*cand_ptr.add(idx)).mask };
            let next_phrase_mask = phrase_mask.wrapping_sub(word_mask);
            if (next_phrase_mask & self.aq_main_sign) != 0 {
                idx += 1;
                continue;
            }

            if (word_mask & pivot_mask) == 0 {
                // SAFETY: `idx < end` and `end > 0` imply both pointers are in-bounds.
                unsafe {
                    std::ptr::swap(cand_ptr.add(idx), cand_ptr.add(end - 1));
                }
                end -= 1;
                continue;
            }

            if self.solution_depth >= MAX_SOL {
                return;
            }
            // SAFETY: `idx` remains in-bounds by the loop invariant above.
            let chosen = unsafe { *cand_ptr.add(idx) };
            let word_len = chosen.length;
            self.solution_text_indices[self.solution_depth] = chosen.text_idx;
            self.solution_lengths[self.solution_depth] = word_len;
            self.solution_frequencies[self.solution_depth] = chosen.frequency;
            self.solution_depth += 1;

            self.cch_phrase_length -= word_len;
            if self.cch_phrase_length > 0 {
                self.find_anagram(next_phrase_mask, idx, i_letter, dict);
                end = self.candidate_len;
            } else {
                self.dump_words(dict);
            }
            self.cch_phrase_length += word_len;
            self.solution_depth -= 1;

            idx += 1;
        }
    }
}

/// Read counted dictionary rows and retain entries above the frequency cutoff.
fn read_dict(path: &str, min_length: usize) -> Result<Dictionary> {
    let mode = TextFileMode::default();
    let mut file = Reader::new_open(path, &mode)?;
    let mut words = Vec::new();
    let mut max_frequency_seen = 0usize;
    if file.is_done() {
        return err!("Anagram Vocab file must not be empty.");
    }
    loop {
        let word = file.curr_line().get(0);
        let freq = file.curr_line().get(1);

        if !word.iter().all(|b: &u8| b.is_ascii_lowercase()) {
            return err!("invalid dictionary entry : {}", String::from_utf8_lossy(word));
        }

        let frequency = freq.to_f64().0 as usize;
        max_frequency_seen = max_frequency_seen.max(frequency);

        if word.len() >= min_length {
            words.push(DictEntry { word: word.to_vec(), frequency });
        }

        if file.get_line()? {
            break;
        }
    }

    // eprintln!("main dictionary has {} entries", words.len());
    Ok(Dictionary { entries: words, max_frequency_seen })
}

/// Normalize phrase to lowercase ASCII letters for hot-path processing.
fn normalize_phrase(phrase: &str) -> Vec<u8> {
    let mut out = Vec::with_capacity(phrase.len());
    for b in phrase.bytes() {
        if b.is_ascii_alphabetic() {
            out.push(b.to_ascii_lowercase());
        }
    }
    out
}

/// Run one anagram search and print results.
fn do_anagram(
    phrase: &str,
    max_return: usize,
    min_length: usize,
    dict_name: &str,
    w: &mut dyn Write,
) -> Result<()> {
    let dictionary = read_dict(dict_name, min_length)?;
    let dict = &dictionary.entries;

    let mut solver = Solver::new();
    solver.max_display = max_return;
    let score_algorithm = score_algorithm_from_env()?;
    solver.set_score_algorithm(score_algorithm);
    solver.set_max_dictionary_frequency(dictionary.max_frequency_seen);
    solver.set_phrase(phrase)?;
    solver.add_words(dict);

    if solver.candidate_len > 0 && solver.cch_phrase_length > 0 {
        if solver.max_display == 0 {
            solver.dump_candidates(dict, w)?;
        } else {
            solver.sort_candidates();
            let main_mask = solver.aq_main_mask;
            solver.find_anagram(main_mask, 0, 0, dict);
            solver.display_words(w)?;
        }
    }

    Ok(())
}

/// Parse scorer name from environment input.
fn parse_score_algorithm(value: &str) -> Result<ScoreAlgorithm> {
    match value {
        "square" => Ok(ScoreAlgorithm::Square),
        "fewest_words" => Ok(ScoreAlgorithm::FewestWords),
        "frequency_weighted" => Ok(ScoreAlgorithm::FrequencyWeighted),
        _ => err!(
            "invalid ANAGRAM_SCORE value `{value}`; expected one of: square, fewest_words, frequency_weighted"
        ),
    }
}

/// Resolve score algorithm from `ANAGRAM_SCORE`, validating values.
fn score_algorithm_from_env() -> Result<ScoreAlgorithm> {
    match std::env::var("ANAGRAM_SCORE") {
        Ok(value) => parse_score_algorithm(&value),
        Err(std::env::VarError::NotPresent) => Ok(DEFAULT_SCORE_ALGORITHM),
        Err(std::env::VarError::NotUnicode(_)) => {
            err!("invalid ANAGRAM_SCORE: value is not valid UTF-8".to_string())
        }
    }
}

// CLI entry point.
// fn main() {
//     let args: Vec<String> = std::env::args().collect();
//     let dict_name = DEFAULT_DICT_PATH.to_string();

//     if args.len() < 2 || args.len() > 4 {
//         print_usage(&args[0]);
//         process::exit(255);
//     }

//     let phrase = &args[1];
//     let max_return = parse_or_default(&args, 2, DEFAULT_MAX_RETURN);
//     let min_length = parse_or_default(&args, 3, 1usize);

//     let code = do_anagram(phrase, max_return, min_length, &dict_name);
//     process::exit(code);
// }

#[cfg(test)]
mod tests {
    use super::*;
    use std::process;

    #[test]
    /// Verifies phrase normalization and mask counting for mixed input.
    fn build_mask_counts_ascii_letters_only() -> Result<()> {
        let mut solver = Solver::new();
        let normalized = normalize_phrase("Aa!b?");
        solver.build_mask(&normalized)?;

        assert_eq!(solver.cch_phrase_length, 3);
        assert_eq!(solver.al_phrase[0].frequency, 2); // a
        assert_eq!(solver.al_phrase[1].frequency, 1); // b
        assert_eq!(solver.al_phrase[2].frequency, 0); // c
        Ok(())
    }

    #[test]
    /// Verifies score clamping hits both min and max histogram buckets.
    fn set_score_clamps_to_valid_range() {
        let mut solver = Solver::new();

        assert!(solver.set_score(-5));
        assert!(solver.set_score(SCORE_MAX + 500));

        assert_eq!(solver.score_counts[0], 1);
        assert_eq!(solver.score_counts[SCORE_BUCKETS - 1], 1);
    }

    #[test]
    /// Verifies frequency-weighted scoring strongly favors higher minimum frequency.
    fn frequency_weighted_scoring_emphasizes_lowest_frequency() {
        let mut solver = Solver::new();
        solver.set_score_algorithm(ScoreAlgorithm::FrequencyWeighted);
        solver.set_max_dictionary_frequency(1_000_000);

        // Higher average but much lower minimum frequency.
        let low_min = solver.calculate_score(&[4, 4, 4], &[900_000, 900_000, 100_000], 1_000_000);
        // Lower average but much higher minimum frequency.
        let high_min = solver.calculate_score(&[4, 4, 4], &[500_000, 500_000, 500_000], 1_000_000);

        assert!(high_min > low_min);
    }

    #[test]
    /// Verifies frequency-weighted scoring still rewards fewer words.
    fn frequency_weighted_scoring_prefers_fewer_words() {
        let mut solver = Solver::new();
        solver.set_score_algorithm(ScoreAlgorithm::FrequencyWeighted);
        solver.set_max_dictionary_frequency(1_000_000);

        let two_words = solver.calculate_score(&[5, 5], &[600_000, 600_000], 1_000_000);
        let four_words = solver.calculate_score(&[5, 5, 5, 5], &[600_000; 4], 1_000_000);

        assert!(two_words > four_words);
    }

    #[test]
    /// Verifies all supported scorer names parse successfully.
    fn parse_score_algorithm_accepts_all_known_values() {
        assert_eq!(parse_score_algorithm("square").unwrap(), ScoreAlgorithm::Square);
        assert_eq!(parse_score_algorithm("fewest_words").unwrap(), ScoreAlgorithm::FewestWords);
        assert_eq!(
            parse_score_algorithm("frequency_weighted").unwrap(),
            ScoreAlgorithm::FrequencyWeighted
        );
    }

    #[test]
    /// Verifies unknown scorer names are rejected.
    fn parse_score_algorithm_rejects_unknown_values() {
        assert!(parse_score_algorithm("nope").is_err());
    }

    #[test]
    /// Verifies phrase normalization keeps only lowercase ASCII letters.
    fn normalize_phrase_keeps_only_lowercase_ascii_letters() {
        let normalized = normalize_phrase("Ru-th! 42 ANNE");
        assert_eq!(normalized, b"ruthanne");
    }

    #[test]
    /// Verifies dictionary loader rejects non-lowercase words.
    fn read_dict_rejects_non_lowercase_ascii_entries() {
        let path = format!("/tmp/anagram_rust_test_{}_{}.txt", process::id(), "invalid_dict");
        std::fs::write(&path, "good\t60000\nBad\t60000\n").expect("write test dictionary");
        read_dict(&path, 1).expect_err("dictionary should be rejected");
        drop(std::fs::remove_file(&path));
    }

    #[test]
    /// Verifies dictionary loader filters out rows below frequency threshold.
    fn read_dict_filters_out_low_frequency_rows() {
        let path = format!("/tmp/anagram_rust_test_{}_{}.txt", process::id(), "freq_filter_dict");
        std::fs::write(&path, "alpha\t49999\tignored\nbeta\t50000\tignored\ngamma\t70000\n")
            .expect("write test dictionary");

        let dict = read_dict(&path, 1).expect("dictionary should parse");
        assert_eq!(dict.max_frequency_seen, 70_000);
        assert_eq!(dict.entries.len(), 2);
        assert_eq!(dict.entries[0].word, b"beta");
        assert_eq!(dict.entries[0].frequency, 50_000);
        assert_eq!(dict.entries[1].word, b"gamma");
        assert_eq!(dict.entries[1].frequency, 70_000);

        drop(std::fs::remove_file(&path));
    }
}
