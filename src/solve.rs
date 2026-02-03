//! Solve hidden code puzzles like mastermond or wordle
use crate::matcher::Match;
use crate::prelude::*;

pub(crate) const fn to_lower(ch: u8) -> u8 {
    if b'A' <= ch && ch <= b'Z' { ch + b'a' - b'A' } else { ch }
}

// Check if the attempt matches the word given the exact pattern
#[must_use]
pub(crate) fn matches(word: &[u8], exact: &[u8], guess: &[u8]) -> bool {
    debug_assert!(word.len() == exact.len());
    debug_assert!(word.len() == guess.len());

    let mut used = vec![false; word.len()];
    // first pass: exact matches
    for i in 0..word.len() {
        if word[i] == exact[i] {
            used[i] = true;
        } else if exact[i] != b'.' {
            return false;
        }
    }
    // second pass: misplaced letters
    for (i, g) in guess.iter().enumerate() {
        if b'A' <= *g && *g <= b'Z' {
            for j in 0..word.len() {
                if i == j && word[j] == to_lower(*g) {
                    // cannot be in the same position
                    return false;
                }
                if (to_lower(*g) == word[j]) && !used[j] {
                    used[j] = true;
                    break;
                }
                // end of the loop without a break means no match
                if j == word.len() - 1 {
                    return false;
                }
            }
        }
    }
    // third pass: wrong letters
    for g in guess {
        if b'a' <= *g && *g <= b'z' {
            for j in 0..word.len() {
                if (*g == word[j]) && !used[j] {
                    return false;
                }
            }
        }
    }

    true
}

// Check if the attempt matches the word given multiple guess patterns
#[must_use]
fn multi_match(word: &[u8], exact: &[u8], guess: &[Vec<u8>]) -> bool {
    if word.len() != exact.len() {
        return false;
    }
    for g in guess {
        if !matches(word, exact, g) {
            return false;
        }
    }
    true
}

/// Scores a word based on candidate lists
#[allow(clippy::cast_precision_loss)]
pub fn score_word(word: &[u8], candidates: &[Vec<u8>]) -> f64 {
    let mut total = 0.0;
    // for each result from word {
    for ch in word {
        let mut count = 0;
        for c in candidates {
            if c.contains(ch) {
                count += 1;
            }
        }
        let entropy = if count == 0 {
            0.0
        } else {
            let p_x = f64::from(count) / (candidates.len() as f64);
            p_x * (1.0 / p_x).log2()
        };
        total += entropy;
        // eprintln!("For word {}, with exact {:?} and guess {:?}, count = {},
    }
    total
}

///    A matcher that solves puzzles like mastermind or wordle
#[derive(Debug, Clone)]
pub struct SolverMatch {
    /// Letter, is known, '.' otherwise
    exact: Vec<u8>,
    /// Lowercase for wrong, uppercase for misplaced, '.' for exact
    guesses: Vec<Vec<u8>>,
}

fn validate_exact(ch: u8) -> Result<()> {
    if ch.is_ascii_lowercase() || (ch == b'.') {
        Ok(())
    } else {
        err!("Exact pattern must only contain lowercase letters or '.'")
    }
}
fn validate_guess(ch: u8) -> Result<()> {
    if ch.is_ascii_lowercase() || ch.is_ascii_uppercase() || (ch == b'.') {
        Ok(())
    } else {
        err!("Guess pattern must only contain lowercase letters, uppercase letters, or '.'")
    }
}

impl SolverMatch {
    /// Create a new `SolverMatch` from a data string
    pub fn new(data: &str) -> Result<Self> {
        let words: Vec<&str> = data.split(',').collect();
        if words.is_empty() {
            return err!("Solver matcher requires at least exact pattern");
        }
        if words[0].is_empty() {
            return err!("Solver patterns must not be empty.");
        }
        let len = words[0].len();
        for word in &words {
            if word.len() != len {
                return err!("All patterns in solver matcher must have the same length");
            }
        }
        let ret = if words[0].contains('.') {
            Self {
                exact: words[0].as_bytes().to_vec(),
                guesses: words[1..].iter().map(|w| w.as_bytes().to_vec()).collect(),
            }
        } else {
            Self {
                exact: vec![b'.'; len],
                guesses: words.iter().map(|w| w.as_bytes().to_vec()).collect(),
            }
        };

        for &ch in &ret.exact {
            validate_exact(ch)?;
        }
        for g in &ret.guesses {
            for &ch in g {
                validate_guess(ch)?;
            }
        }

        Ok(ret)
    }
}
impl Match for SolverMatch {
    fn smatch(&self, buff: &str) -> bool {
        multi_match(buff.as_bytes(), &self.exact, &self.guesses)
    }
    fn umatch(&self, buff: &[u8]) -> bool {
        multi_match(buff, &self.exact, &self.guesses)
    }
    fn show(&self) -> String {
        format!(
            "Solver Match of exact: {}, guesses: {}",
            String::from_utf8_lossy(&self.exact),
            self.guesses.iter().map(|g| String::from_utf8_lossy(g)).collect::<Vec<_>>().join(", ")
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solve() {
        assert!(matches(b"apple", b".....", b"uvxyz"));
        assert!(matches(b"apple", b".....", b"PxyzP"));
        assert!(matches(b"apple", b".....", b"qrtPA"));
        assert!(matches(b"apple", b"apple", b"vwxyz"));
        assert!(matches(b"apple", b".....", b"xyzPA"));
        assert!(!matches(b"apple", b".....", b"xyzLA"));
        assert!(!matches(b"apple", b".....", b"xyzAA"));
    }
    #[test]
    fn multi_solve() {
        assert!(multi_match(b"apple", b".....", &[b"PxyzP".to_vec()]));
        assert!(multi_match(b"apple", b"apple", &[b"vwxyz".to_vec()]));
        assert!(multi_match(b"apple", b".....", &[b"xyzPA".to_vec()]));
        assert!(!multi_match(b"apple", b".....", &[b"xyzLA".to_vec()]));
        assert!(!multi_match(b"apple", b".....", &[b"xyzAA".to_vec()]));
        assert!(multi_match(b"apple", b".....", &[b"uvxyz".to_vec()]));
        assert!(multi_match(b"apple", b".....", &[b"qrtPA".to_vec()]));
        assert!(multi_match(b"apple", b".....", &[b"uvxyz".to_vec(), b"qrtPA".to_vec()]));
    }
}
