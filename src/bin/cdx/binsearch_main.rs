#![allow(dead_code)]

use crate::prelude::*;
use cdx::binsearch::{MemMap, equal_range_n, find_end, find_prev};
use cdx::prelude::*;
use cdx::util::write_all_nl;

#[derive(Debug, Default, PartialEq)]
struct FileNameColumn {
    name: String,
    tail: usize,
}
impl FileNameColumn {
    fn new() -> Self {
        Self::default()
    }
    fn set(&mut self, spec: &str) -> Result<()> {
        if spec.is_empty() {
            *self = Self::default();
            return Ok(());
        }
        if spec.as_bytes().contains(&b':') {
            let (name, tail) = spec.split_once(':').unwrap();
            self.name = name.to_string();
            self.tail = tail.to_usize_whole(spec.as_bytes(), "file path parts")?;
        } else if spec.as_bytes()[0].is_ascii_digit() {
            self.name.clear();
            self.tail = spec.to_usize_whole(spec.as_bytes(), "file path parts")?;
        } else {
            self.tail = 0;
            self.name = spec.to_string();
        }
        Ok(())
    }
}

#[derive(Debug, Default, PartialEq)]
struct Context {
    before_match: usize,
    before_non: usize,
    after_match: usize,
    after_non: usize,
}

fn do_set(spec: &str, yes_match: &mut usize, no_match: &mut usize) -> Result<()> {
    let v: Vec<&str> = spec.split('.').collect();
    if v.len() == 1 {
        *yes_match = v[0].to_usize_whole(spec.as_bytes(), "context lines")?;
        *no_match = *yes_match;
    } else if v.len() == 2 {
        *yes_match = v[0].to_usize_whole(spec.as_bytes(), "context lines")?;
        *no_match = v[1].to_usize_whole(spec.as_bytes(), "context lines")?;
    } else {
        return err!("each context piece must be one or two comma delimited integers");
    }
    Ok(())
}

impl Context {
    fn new() -> Self {
        Self::default()
    }
    fn is_empty(&self) -> bool {
        *self == Self::default()
    }
    fn get(&self, is_empty: bool) -> (usize, usize) {
        if is_empty {
            (self.before_non, self.after_non)
        } else {
            (self.before_match, self.after_match)
        }
    }
    fn set(&mut self, spec: &str) -> Result<()> {
        let v: Vec<&str> = spec.split(',').collect();
        if v.len() == 1 {
            do_set(v[0], &mut self.before_match, &mut self.before_non)?;
            self.after_match = self.before_match;
            self.after_non = self.before_non;
        } else if v.len() == 2 {
            do_set(v[0], &mut self.before_match, &mut self.before_non)?;
            do_set(v[1], &mut self.after_match, &mut self.after_non)?;
        } else {
            return err!("context arg must be one or two comma delimited pieces");
        }
        Ok(())
    }
}

pub fn main(argv: &[String], settings: &mut Settings) -> Result<()> {
    let prog = args::ProgSpec::new("Search sorted files.", args::FileCount::Many);
    const A: [ArgSpec; 5] = [
        arg! {"key", "k", "Spec", "How to compare value to lines"},
        arg! {"filename", "H", "ColName:Parts", "Prefix output lines with file name."},
        arg! {"context", "C", "before,after",  "print lines of context around matches"},
        arg! {"sub-delim", "s", "Char",  "Delimiter between keys for multi-column searches"},
        arg_pos! {"pattern", "search string",  "Search for this string in each file"},
    ];
    let (args, files) = args::parse(&prog, &A, argv, settings)?;

    let mut filename: Option<FileNameColumn> = None;
    let mut context = Context::new();
    let mut subdelim = b',';
    let mut comp = LineCompList::new();
    let mut pattern: Option<String> = None;

    for x in args {
        if x.name == "key" {
            comp.add(&x.value)?;
        } else if x.name == "filename" {
            if filename.is_some() {
                return err!("You cant use --filename twice");
            }
            let mut f = FileNameColumn::new();
            f.set(&x.value)?;
            filename = Some(f);
        } else if x.name == "context" {
            context.set(&x.value)?;
        } else if x.name == "subdelim" {
            if x.value.len() != 1 {
                return err!("--sub-delim value must be a single character");
            }
            subdelim = x.value.as_bytes()[0];
        } else if x.name == "pattern" {
            pattern = Some(x.value);
        } else {
            unreachable!();
        }
    }
    if pattern.is_none() {
        return err!("The pattern is required");
    }
    let pattern = pattern.unwrap();
    if files.is_empty() {
        return err!("At least one file is required : you can't binary search stdin.");
    }
    if comp.is_empty() {
        comp.add("1")?;
    }
    comp.set(pattern.as_bytes(), subdelim)?;
    let mut w = get_writer("-")?;
    let mut not_header: Vec<u8> = Vec::new();

    for f in &files {
        let m = MemMap::new(f)?;
        not_header.clear();
        if m.has_header() {
            if let Some(f) = &filename {
                not_header.extend(b" CDX\t");
                not_header.extend(f.name.as_bytes());
                not_header.extend(&m.header()[4..]);
            } else {
                not_header.extend(m.header());
            }
        }
        if settings.checker.check(&not_header, f)? {
            w.write_all(&not_header)?;
        }
        comp.lookup(&m.names())?;
        let (mut start, mut stop) = equal_range_n(m.get(), &mut comp);
        let (before, after) = context.get(start == stop);
        for _x in 0..before {
            if start == 0 {
                break;
            }
            start = find_prev(m.get(), start);
        }
        for _x in 0..after {
            stop = find_end(m.get(), stop);
        }
        if let Some(item) = &filename {
            let file: &FileNameColumn = item;
            while start < stop {
                let end = find_end(m.get(), start);
                w.write_all(f.tail_path_u8(file.tail, b'/').as_bytes())?;
                w.write_all(b"\t")?;
                write_all_nl(&mut w.0, &m.get()[start..end])?;
                start = end;
            }
        } else {
            write_all_nl(&mut w.0, &m.get()[start..stop])?;
        }
    }
    Ok(())
}
