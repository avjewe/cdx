use crate::args::ArgSpec;
use crate::{arg, arg_enum, args};
use cdx::sampler::Smooth;
use cdx::tabs::Rect;
use cdx::util::{err, get_writer, Error, HeaderChecker, HeaderMode, Reader, Result, HEADER_MODE};
use std::io::Write;
use std::ops::RangeInclusive;
use std::str::FromStr;

#[derive(Default)]
struct Ranges {
    v: Vec<RangeInclusive<usize>>,
}
impl Ranges {
    fn push(&mut self, spec: &str) -> Result<()> {
        self.v.push(if let Some((a, b)) = spec.split_once('-') {
            RangeInclusive::new(a.parse::<usize>()?, b.parse::<usize>()?)
        } else {
            let val = spec.parse::<usize>()?;
            RangeInclusive::new(val, val)
        });
        Ok(())
    }
    fn max(&self) -> usize {
        let mut ret: usize = 0;
        for x in &self.v {
            ret = ret.max(*x.end());
        }
        ret
    }
    fn contains(&self, val: usize) -> bool {
        for x in &self.v {
            if x.contains(&val) {
                return true;
            }
        }
        false
    }
}

struct For {
    by: usize,
    from: usize,
    to: usize,
}
impl Default for For {
    fn default() -> Self {
        Self {
            by: 1,
            from: 1,
            to: usize::MAX,
        }
    }
}

impl For {
    fn new(spec: &str) -> Result<Self> {
        if spec.is_empty() {
            return err!("Can't make a For loop out of an empty string");
        }
        let mut f = For::default();
        for (i, x) in spec.split(',').enumerate() {
            match i {
                0 => f.by = x.parse::<usize>()?,
                1 => f.from = x.parse::<usize>()?,
                2 => f.to = x.parse::<usize>()?,
                _ => {
                    return err!(
                        "No more than three comma delimited pieces in a For loop spec '{}'",
                        spec
                    )
                }
            }
        }
        if f.by == 0 || f.from == 0 || f.to == 0 {
            return err!(
                "Nono of the pieces in a For loop spec can be zero '{}'",
                spec
            );
        }
        Ok(f)
    }
}

pub fn main(argv: &[String]) -> Result<()> {
    let prog = args::ProgSpec::new("Sample lines from files.", args::FileCount::Many);
    const A: [ArgSpec; 4] = [
        arg_enum! {"header", "h", "Mode", "header requirements", &HEADER_MODE},
        arg! {"for", "f", "by,from,to", "for i=from; i<=to; i+= by"},
        arg! {"sample", "s", "Number", "Select this number of lines, more or less evenly spaced."},
        arg! {"range", "r", "Ranges", "e.g. 1-5,42,95-106."},
    ];
    let (args, files) = args::parse(&prog, &A, argv);

    let mut checker = HeaderChecker::new();
    let mut floop = For::default();
    let mut ranges = Ranges::default();
    let mut sample = 10;
    let mut saw_sample = false;
    let mut saw_for = false;
    let mut saw_range = false;

    for x in args {
        if x.name == "header" {
            checker.mode = HeaderMode::from_str(&x.value)?;
        } else if x.name == "for" {
            if saw_for || saw_sample || saw_range {
                return err!("No more that one --sample, --range  or --for allowed");
            }
            floop = For::new(&x.value)?;
            saw_for = true;
        } else if x.name == "range" {
            if saw_for || saw_sample || saw_range {
                return err!("No more that one --sample, --range  or --for allowed");
            }
            saw_range = true;
            for x in x.value.split(',') {
                ranges.push(x)?;
            }
        } else if x.name == "sample" {
            if saw_for || saw_sample || saw_range {
                return err!("No more that one --sample, --range  or --for allowed");
            }
            sample = x.value.parse::<usize>()?;
            saw_sample = true;
        } else {
            unreachable!();
        }
    }
    if !saw_for && !saw_sample && !saw_range {
        saw_sample = true;
        sample = Rect::from_screen().height;
    }

    let mut w = get_writer("-")?;
    for x in &files {
        let mut f = Reader::new();
        f.open(x)?;
        if f.is_empty() {
            continue;
        }
        if checker.check_file(&f.cont, x)? {
            w.write_all(f.header().line.as_bytes())?;
        }
        if f.is_done() {
            return Ok(());
        }
        f.do_split = false;
        if saw_sample {
            let mut s = Smooth::new(sample);
            loop {
                s.add(&f.curr().line);
                if f.getline()? {
                    break;
                }
            }
            s.finalize(&mut w)?;
        } else if saw_for {
            let mut next = floop.from;
            while f.line_number() <= floop.to {
                if f.line_number() == next {
                    w.write_all(&f.curr_line().line)?;
                    next += floop.by;
                }
                if f.getline()? {
                    break;
                }
            }
        } else {
            let max = ranges.max();
            while f.line_number() <= max {
                if ranges.contains(f.line_number()) {
                    w.write_all(&f.curr_line().line)?;
                }
                if f.getline()? {
                    break;
                }
            }
        }
    }
    Ok(())
}
