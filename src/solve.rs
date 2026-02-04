//! Solve hidden code puzzles like mastermond or wordle
use crate::matcher::Match;
use crate::prelude::*;

pub(crate) const fn to_lower(ch: u8) -> u8 {
    if b'A' <= ch && ch <= b'Z' { ch + b'a' - b'A' } else { ch }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Default, Hash)]
enum Class {
    #[default]
    Exact,
    Misplaced,
    Wrong,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Default, Hash)]
pub(crate) struct LetterInfo {
    ch: u8,
    class: Class,
}
// Check if the attempt matches the word given the exact pattern
#[must_use]
pub(crate) fn matches(word: &[u8], guess: &[LetterInfo]) -> bool {
    debug_assert!(word.len() == guess.len());

    let mut used = vec![false; word.len()];
    // first pass: exact matches
    for i in 0..guess.len() {
        if guess[i].class == Class::Exact {
            if word[i] != guess[i].ch {
                return false;
            }
            used[i] = true;
        }
    }

    // second pass: misplaced letters
    for (i, g) in guess.iter().enumerate() {
        if g.class == Class::Misplaced {
            for j in 0..word.len() {
                if i == j && word[j] == g.ch {
                    // cannot be in the same position
                    return false;
                }
                if g.ch == word[j] && !used[j] {
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
        if g.class == Class::Wrong {
            for j in 0..word.len() {
                if g.ch == word[j] && !used[j] {
                    return false;
                }
            }
        }
    }

    true
}

// Check if the attempt matches the word given multiple guess patterns
#[must_use]
fn multi_match(word: &[u8], guess: &[Vec<LetterInfo>]) -> bool {
    for g in guess {
        if word.len() != g.len() {
            return false;
        }
        if !matches(word, g) {
            return false;
        }
    }
    true
}

#[must_use]
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
    }
    total
}

/// A matcher that solves puzzles like mastermind or wordle
#[derive(Debug, Clone)]
pub struct SolverMatch {
    /// Lowercase for wrong, uppercase for misplaced, '.' for exact
    guesses: Vec<Vec<LetterInfo>>,
}
// ch.is_ascii_lowercase()

fn make_letter_info(data: &[u8]) -> Result<Vec<LetterInfo>> {
    let mut ret = Vec::with_capacity(data.len());
    let mut was_plus = false;
    for &ch in data {
        if ch == b'+' {
            if was_plus {
                return err!("Consecutive '+' characters in solver pattern");
            }
            was_plus = true;
        } else if ch.is_ascii_lowercase() {
            if was_plus {
                ret.push(LetterInfo { ch, class: Class::Exact });
                was_plus = false;
            } else {
                ret.push(LetterInfo { ch, class: Class::Wrong });
            }
        } else if ch.is_ascii_uppercase() {
            ret.push(LetterInfo { ch: to_lower(ch), class: Class::Misplaced });
        } else {
            return err!("Invalid character in solver pattern: {}", ch as char);
        }
    }
    Ok(ret)
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
        let mut guesses = Vec::new();
        let mut len = 0usize;
        for word in &words {
            let guess = make_letter_info(word.as_bytes())?;
            if len == 0 {
                len = guess.len();
            } else if guess.len() != len {
                return err!("All patterns in solver matcher must have the same length");
            }
            guesses.push(guess);
        }

        Ok(Self { guesses })
    }
}
impl Match for SolverMatch {
    fn smatch(&self, buff: &str) -> bool {
        multi_match(buff.as_bytes(), &self.guesses)
    }
    fn umatch(&self, buff: &[u8]) -> bool {
        multi_match(buff, &self.guesses)
    }
    fn show(&self) -> String {
        "Solver Match".to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_match(word: &[u8], guess: &str) -> bool {
        let sm = SolverMatch::new(guess).unwrap();
        sm.umatch(word)
    }

    #[test]
    fn solve() {
        assert!(test_match(b"apple", "PxyzP"));
        assert!(test_match(b"apple", "qrtPA"));
        assert!(test_match(b"apple", "+a+p+p+l+e,vwxyz"));
        assert!(test_match(b"apple", "xyzPA"));
        assert!(!test_match(b"apple", "xyzLA"));
        assert!(!test_match(b"apple", "xyzAA"));
    }
    #[test]
    fn multi_solve() {
        assert!(test_match(b"apple", "PxyzP"));
        assert!(test_match(b"apple", "xyzPA"));
        assert!(!test_match(b"apple", "xyzLA"));
        assert!(!test_match(b"apple", "xyzAA"));
        assert!(test_match(b"apple", "uvxyz"));
        assert!(test_match(b"apple", "qrtPA"));
        assert!(test_match(b"apple", "uvxyz,qrtPA"));
        assert!(test_match(b"weigh", "starE,no+ily"));
        assert!(test_match(b"weigh", "starE,no+ily,b+e+i+ge"));
    }
}
