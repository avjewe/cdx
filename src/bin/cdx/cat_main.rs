use crate::args::ArgSpec;
use crate::{arg, arg_enum, args};
use cdx::check::{BufCheck, CheckOr, CheckSpec};
use cdx::column::{ColumnCount, ColumnLiteral, ColumnWhole, Writer};
use cdx::text::Text;
use cdx::{
    err, get_reader, get_writer, Error, HeaderChecker, HeaderMode, Reader, Result, HEADER_MODE,
};
use std::io::{BufRead, Read, Write};
use std::str::FromStr;

#[derive(Debug, Copy, Clone, PartialEq)]
enum PadMode {
    None,
    All,
    End,
}

fn make_match(src: &[CheckSpec]) -> Result<Option<CheckOr>> {
    if src.is_empty() {
        return Ok(None);
    }
    let mut res = CheckOr::new();
    for x in src {
        res.push(x.make_box("")?);
    }
    Ok(Some(res))
}

#[derive(Default, Debug, Clone)]
/// how to number lines
struct LineNumber {
    pub do_it: bool,
    pub start: i64,
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
        self.start = second.unwrap().parse::<i64>()?;

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
    let prog = args::ProgSpec::new("Concatenate files.", args::FileCount::Many);
    const A: [ArgSpec; 7] = [
        arg_enum! {"header", "h", "Mode", "header requirements", &HEADER_MODE},
        arg_enum! {"pad", "p", "Mode", "Add trailing newline if absent.", &["Yes","No","End"]},
        arg_enum! {"remove", "r", "Mode", "Remove these lines.", &["Empty","Blank","Hash","Slash"]},
        arg_enum! {"skip", "s", "Mode", "Do not number these lines.", &["Empty","Blank","Hash","Slash"]},
        arg! {"number", "n", "Name,Start,Where", "Number the lines in column 'Name', starting at 'Start', 'Where' can be 'begin' or 'end'"},
        arg! {"begin", "b", "",  "Shortcut for --number number,1,begin"},
        arg! {"end", "e", "",  "Shortcut for --number number,1,end"},
    ];
    let (args, files) = args::parse(&prog, &A, argv);

    let mut checker = HeaderChecker::new();
    let mut pad = PadMode::All;
    let mut num = LineNumber::new();
    let mut skips: Vec<CheckSpec> = Vec::new();
    let mut removes: Vec<CheckSpec> = Vec::new();

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
            skips.push(CheckSpec::new(&x.value)?);
        } else if x.name == "remove" {
            removes.push(CheckSpec::new(&x.value)?);
        } else {
            unreachable!();
        }
    }

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
        let skipper = make_match(&skips)?;
        let remover = make_match(&removes)?;
        let mut not_header: Vec<u8> = Vec::new();

        for x in &files {
            let mut f = Reader::new();
            f.open(x)?;
            if f.is_empty() {
                continue;
            }
            v.lookup(&f.names())?;
            not_header.clear();
            if f.cont.has_header {
                v.write_names(&mut not_header, f.header())?;
            }
            if checker.check(&not_header, x)? {
                w.write_all(&not_header)?;
            }
            if f.is_done() {
                return Ok(());
            }
            f.do_split = false;
            loop {
                let mut do_write = true;
                if let Some(ref x) = remover {
                    do_write = !x.ucheck(f.curr_nl());
                }
                if do_write {
                    let mut do_count = true;
                    if let Some(ref x) = skipper {
                        do_count = !x.ucheck(f.curr_nl());
                    }
                    if do_count {
                        v.write(&mut w, f.curr())?;
                    } else {
                        not_v.write(&mut w, f.curr())?;
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
