//! Samplers are given a series of data, and write some of them

use crate::prelude::*;

// data[0..size] holds the data, which is
// if jump == 1, contents are just the lines seen so far.
// else it's first line, stuff, last line
// stuff is every "jump" lines starting with line "jump"
/// Sample N evenly distributed items from an input stream
#[derive(Debug)]
pub struct Smooth {
    small: usize,       // goal size
    large: usize,       // maximum size
    data: Vec<Vec<u8>>, // always length large
    jump: usize,        // number of lines between entries
    pos: usize,         // number of lines between last and penultimate entries in data
    size: usize,        // current number of 'live' elements in data
}

impl Smooth {
    /// keep best sz items
    pub fn new(sz: usize) -> Self {
        let small = sz.max(2);
        let large = small * 2 - 2;
        let mut data = Vec::new();
        data.reserve(large);
        for _ in 0..large {
            data.push(Vec::new());
        }
        Self {
            small,
            large,
            data,
            jump: 1,
            pos: 0,
            size: 0,
        }
    }
    fn verify(&self) -> bool {
        debug_assert!(self.data.len() == self.large);
        debug_assert!(self.size <= self.large);
        debug_assert!(self.size <= self.large);
        //	debug_assert!(self.pos <= self.jump);
        true
    }
    /// add one more item
    pub fn add(&mut self, x: &[u8]) {
        debug_assert!(self.verify());
        if self.pos == 0 {
            if self.size >= self.large {
                self.jump *= 2;
                self.collapse();
            }
            self.data[self.size].clear();
            self.data[self.size].extend(x);
            self.size += 1;
            if self.jump > 1 {
                self.pos += 1;
            }
        } else {
            self.data[self.size - 1].clear();
            self.data[self.size - 1].extend(x);
            self.pos += 1;
            debug_assert!(self.verify());
            let limit = if self.size == self.large {
                2 * self.jump
            } else {
                self.jump
            };
            if self.pos >= limit {
                self.pos = 0;
            }
        }
        debug_assert!(self.verify());
    }

    // crunch down to self.small elements
    // maintain first and last, spread the rest evenly.
    fn collapse(&mut self) {
        debug_assert!(self.verify());
        let old_size = self.size;
        let new_size = self.small;

        if old_size <= new_size {
            return;
        }
        if new_size == 2 {
            self.data.swap(1, self.size - 1);
            self.size = 2;
            return;
        }

        let new_end = new_size - 1;
        let ratio = (old_size - 2) as f64 / (new_size - 2) as f64;
        for i in 1..new_end {
            let pos = ((i as f64) * ratio) as usize;
            self.data.swap(i, pos);
        }
        self.data.swap(new_end, self.size - 1);
        self.size = new_size;
    }

    /// finish and write
    pub fn finalize(&mut self, w: &mut impl Write) -> Result<()> {
        self.collapse();
        self.write(w)
    }

    /// write the lines
    pub fn write(&self, w: &mut impl Write) -> Result<()> {
        for i in 0..self.size {
            w.write_all(&self.data[i])?;
        }
        Ok(())
    }
/*
    fn dump(&self) {
        for i in 0..self.size {
            prerr_n(&[&self.data[i]]);
        }
    }
*/
}
