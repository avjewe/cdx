use crate::arg;
use crate::args;
use crate::args::ArgSpec;
use cdx::comp::LineCompList;
use cdx::{get_writer, LookbackReader, Result, err, Error};
use cdx::column::ColumnHeader;
use std::io::Write;

// ColName,Position
// --min
// --max --max-out --max-discard

#[derive(PartialEq)]
enum CountPos {
    None,
    Begin,
    End
}
impl Default for CountPos {
    fn default() -> Self {
        CountPos::None
    }
}
#[derive(Default)]
struct Count {
    name : String,
    pos : CountPos,
}

fn chomp(x : &[u8]) -> &[u8] {
    if !x.is_empty() {
	let len = x.len()-1;
	if x[len] == b'\n' {
	    return &x[..len];
	}
    }
    x
}

impl Count {
    pub fn write(&self, w: &mut impl Write, num : usize, line : &[u8]) -> Result<()> {
	match self.pos {
	    CountPos::None => {
		w.write_all(line)?;
	    }
	    CountPos::Begin => {
		write!(w, "{}\t", num)?;
		w.write_all(line)?;
	    }
	    CountPos::End => {
		w.write_all(chomp(line))?;
		writeln!(w, "\t{}", num)?;
	    }
	}
	Ok(())
    }
}

fn get_count(spec : &str) -> Result<Count> {
    if let Some((a, b)) = spec.split_once(',') {
	let pos = if b.eq_ignore_ascii_case("begin") || b.eq_ignore_ascii_case("start") {
	    CountPos::Begin
	}
	else if b.eq_ignore_ascii_case("stop") || b.eq_ignore_ascii_case("end") {
	    CountPos::End
	}
	else {
	    return err!("You suck {}");
	};
	Ok(Count{name : a.to_string(), pos})
    } else {
	err!("No comma found in '{}'. Format for Count is ColName,Position")
    }
}

pub fn main(argv: &[String]) -> Result<()> {
    let prog = args::ProgSpec::new("Select uniq lines.", args::FileCount::One);
    const A: [ArgSpec; 3] = [
	arg! {"key", "k", "Spec", "How to compare adjacent lines"},
	arg! {"last", "l", "", "Write last matching line, rather than first"},
	arg! {"count", "c", "ColName,Position", "Write the count of matchingn lines."}
    ];
    let (args, files) = args::parse(&prog, &A, argv);

    let mut comp = LineCompList::new();
    let mut last = false;
    let mut count = Count::default();
    for x in args {
        if x.name == "key" {
            comp.add(&x.value)?;
        } else if x.name == "last" {
	    last = true;
        } else if x.name == "count" {
	    count = get_count(&x.value)?;
        } else {
            unreachable!();
        }
    }

    assert_eq!(files.len(), 1);

    let mut f = LookbackReader::new(1);
    f.open(&files[0])?;
    if f.is_empty() {
        return Ok(());
    }

    let mut w = get_writer("-")?;
    if f.has_header() {
	let mut ch = ColumnHeader::new();
	if count.pos == CountPos::Begin {
	    ch.push(&count.name)?;
	}
	ch.push_all(f.header())?;
	if count.pos == CountPos::End {
	    ch.push(&count.name)?;
	}
	w.write_all(ch.get_head(f.delim()).as_bytes())?;
    }
    if f.is_done() {
        return Ok(());
    }

    comp.lookup(&f.names())?;
    f.do_split = comp.need_split();
    let mut matches = 1;
    if last {
	loop {
            if f.getline()? {
		count.write(&mut w, matches, &f.prev_line(1).line)?;
		break;
            }
            if comp.equal_cols(f.prev_line(1), f.curr_line()) {
		matches += 1;
	    }
	    else {
		count.write(&mut w, matches, &f.prev_line(1).line)?;
		matches = 1;
            }
	}
    }
    else {
	f.write_curr(&mut w)?;
	loop {
            if f.getline()? {
		break;
            }
            if !comp.equal_cols(f.prev_line(1), f.curr_line()) {
		f.write_curr(&mut w)?;
            }
	}
    }
    Ok(())
}
