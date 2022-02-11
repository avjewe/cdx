use crate::prelude::*;
use cdx::prelude::*;
use cdx::column::{ColumnCount, ColumnLiteral, ColumnWhole};
use cdx::matcher::{Combiner, MatchMaker};
use cdx::util::{get_reader, HeaderChecker, HeaderMode, HEADER_MODE};

#[derive(Debug, Copy, Clone, PartialEq)]
enum PadMode {
    None,
    All,
    End,
}

#[derive(Default, Debug, Clone)]
/// how to number lines
struct LineNumber {
    pub do_it: bool,
    pub start: isize,
    pub name: String,
    pub end: bool,
}

impl LineNumber {
    /// new
    pub fn new() -> Self {
        Self::default()
    }
    /// set from Name,Start,Where
    pub fn set(&mut self, spec: &str) -> Result<()> {
        self.do_it = true;
        let mut iter = spec.split(',');
        let first = iter.next();
        if first.is_none() {
            return Ok(());
        }
        self.name = first.unwrap().to_string();

        let second = iter.next();
        if second.is_none() {
            return Ok(());
        }
        self.start = second.unwrap().to_isize_whole(spec.as_bytes(), "start")?;

        let third = iter.next();
        if third.is_none() {
            return Ok(());
        }
        let third = third.unwrap();
        if "begin".equal_insens_quick(third) {
            self.end = false;
        } else if "end".equal_insens_quick(third) {
            self.end = true;
        } else {
            return err!(
                "Where part must be 'begin' or 'end' was {} : {}",
                third,
                spec
            );
        }

        let fourth = iter.next();
        if fourth.is_some() {
            return err!(
                "--number takes no more than three comma delimited parts : {}",
                spec
            );
        }
        Ok(())
    }
}

pub fn main(argv: &[String]) -> Result<()> {
    /*
        let mut ws = libc::winsize{ws_row:0, ws_col:0, ws_xpixel:0, ws_ypixel:0};
        if unsafe{libc::ioctl(1, libc::TIOCGWINSZ, &mut ws)} >= 0 {
        eprintln!("Window size {} {}", ws.ws_row, ws.ws_col);
        }
        else {
        eprintln!("ioctl failed");
        }
    */
    let prog = args::ProgSpec::new("Concatenate files.", args::FileCount::Many);
    const A: [ArgSpec; 7] = [
        arg_enum! {"header", "h", "Mode", "header requirements", &HEADER_MODE},
        arg_enum! {"pad", "p", "Mode", "Add trailing newline if absent.", &["Yes","No","End"]},
        arg! {"remove", "r", "Matcher", "Remove these lines."},
        arg! {"skip", "s", "Matcher", "Do not number these lines."},
        arg! {"number", "n", "Name,Start,Where", "Number the lines in column 'Name', starting at 'Start', 'Where' can be 'begin' or 'end'"},
        arg! {"begin", "b", "",  "Shortcut for --number number,1,begin"},
        arg! {"end", "e", "",  "Shortcut for --number number,1,end"},
    ];
    let (args, files) = args::parse(&prog, &A, argv);

    let mut checker = HeaderChecker::new();
    let mut pad = PadMode::All;
    let mut num = LineNumber::new();
    let mut skips = MatcherList::new(Combiner::Or);
    let mut removes = MatcherList::new(Combiner::Or);

    for x in args {
        if x.name == "header" {
            checker.mode = HeaderMode::from_str(&x.value)?;
        } else if x.name == "pad" {
            if x.value.to_ascii_lowercase() == "yes" {
                pad = PadMode::All;
            } else if x.value.to_ascii_lowercase() == "no" {
                pad = PadMode::None;
            } else if x.value.to_ascii_lowercase() == "end" {
                pad = PadMode::End;
            } else {
                unreachable!();
            }
        } else if x.name == "number" {
            num.set(&x.value)?;
        } else if x.name == "begin" {
            num.set("number,1,begin")?;
        } else if x.name == "end" {
            num.set("number,1,end")?;
        } else if x.name == "skip" {
            skips.push(MatchMaker::make(&x.value)?);
        } else if x.name == "remove" {
            removes.push(MatchMaker::make(&x.value)?);
        } else {
            unreachable!();
        }
    }

    let mut header = ColumnHeader::new();
    let mut w = get_writer("-")?;
    let slow = num.do_it || !skips.is_empty() || !removes.is_empty();

    if slow {
        let mut v = Writer::new(b'\t');
        if num.do_it && !num.end {
            v.push(Box::new(ColumnCount::new(num.start, &num.name)));
        }
        v.push(Box::new(ColumnWhole));
        if num.do_it && num.end {
            v.push(Box::new(ColumnCount::new(num.start, &num.name)));
        }

        let mut not_v = Writer::new(b'\t');
        if num.do_it && !num.end {
            not_v.push(Box::new(ColumnLiteral::new(b"", "unused")));
        }
        not_v.push(Box::new(ColumnWhole));
        if num.do_it && num.end {
            not_v.push(Box::new(ColumnLiteral::new(b"", "unused")));
        }
        let mut not_header = String::new();

        for x in &files {
            let mut f = Reader::new_open(x)?;
            if f.is_empty() {
                continue;
            }
            v.lookup(&f.names())?;
            header.clear();
            not_header.clear();
            v.add_names(&mut header, f.header())?;
            if f.has_header() {
                not_header = header.get_head(b'\t');
            }
            if checker.check(not_header.as_bytes(), x)? {
                w.write_all(not_header.as_bytes())?;
            }
            if f.is_done() {
                return Ok(());
            }
            f.do_split(false);
            loop {
                if !removes.umatch(f.curr_nl()) {
                    if skips.umatch(f.curr_nl()) {
                        not_v.write(&mut w, f.curr())?;
                    } else {
                        v.write(&mut w, f.curr())?;
                    }
                }
                if f.getline()? {
                    break;
                }
            }
        }
    } else {
        const SIZE: usize = 16 * 1024;
        let mut buffer = [0u8; SIZE];
        let mut first_line = Vec::new();
        let mut last_was_cr = true;
        for x in files {
            let mut f = get_reader(&x)?;
            first_line.clear();
            let n = f.read_until(b'\n', &mut first_line)?;
            if n == 0 {
                continue;
            }
            if checker.check(&first_line, &x)? {
                w.write_all(&first_line)?;
                last_was_cr = first_line.last().unwrap() == &b'\n';
            }
            loop {
                let n = f.read(&mut buffer[..])?;
                if n == 0 {
                    break;
                }
                w.write_all(&buffer[..n])?;
                last_was_cr = first_line.last().unwrap() == &b'\n';
            }
            if pad == PadMode::All && !last_was_cr {
                w.write_all(b"\n")?;
                last_was_cr = true;
            }
        }
        if pad == PadMode::End && !last_was_cr {
            w.write_all(b"\n")?;
        }
    }
    Ok(())
}
