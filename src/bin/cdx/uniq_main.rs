use crate::arg;
use crate::args;
use crate::args::ArgSpec;
use cdx::comp::{LineCompList,LineComp,CompMaker};
use cdx::{get_writer, LookbackReader, Result, err, Error, TextLine};
use cdx::column::ColumnHeader;
use std::io::Write;
use std::cmp::Ordering;

// ColName,Position
// --min
// --max --max-out --max-discard

#[derive(PartialEq, Debug)]
enum CountPos {
    None,
    Begin,
    End
}
impl Default for CountPos {
    fn default() -> Self {
        Self::None
    }
}

#[derive(PartialEq, Debug)]
enum Which {
    First,
    Last,
    Min,
    Max
}
impl Default for Which {
    fn default() -> Self {
        Self::First
    }
}
#[derive(Default)]
struct Count {
    name : String,
    pos : CountPos,
    which : Which,
    comp : LineComp,
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
    fn is_plain(&self) -> bool {
	self.pos == CountPos::None
    }
    fn write(&self, w: &mut impl Write, num : usize, line : &[u8], delim : u8) -> Result<()> {
	match self.pos {
	    CountPos::None => {
		w.write_all(line)?;
	    }
	    CountPos::Begin => {
		write!(w, "{}{}", num, delim as char)?;
		w.write_all(line)?;
	    }
	    CountPos::End => {
		w.write_all(chomp(line))?;
		writeln!(w, "{}{}", delim as char, num)?;
	    }
	}
	Ok(())
    }
    fn get_which2(&mut self, which : &str, pattern : &str) -> Result<()> {
	self.which = if which.eq_ignore_ascii_case("first") {
	    Which::First
	} else if which.eq_ignore_ascii_case("last") {
	    Which::Last
	} else if which.eq_ignore_ascii_case("min") {
	    Which::Min
	} else if which.eq_ignore_ascii_case("max") {
	    Which::Max
	} else {
	    return err!("Which must be one of first,last,min,max : {}", which);
	};
	self.comp = CompMaker::make_line_comp(pattern)?;
	Ok(())
    }
    fn get_which(&mut self, spec : &str) -> Result<()> {
	if let Some((a, b)) = spec.split_once(',') {
	    self.get_which2(a,b)
	}
	else {
	    self.get_which2(spec, "")
	}
    }
    fn get_count(&mut self, spec : &str) -> Result<()> {
	if let Some((a, b)) = spec.split_once(',') {
	    self.pos = if b.is_empty() || b.eq_ignore_ascii_case("begin") || b.eq_ignore_ascii_case("start") || b.eq_ignore_ascii_case("first") {
		CountPos::Begin
	    }
	    else if b.eq_ignore_ascii_case("stop") || b.eq_ignore_ascii_case("end") || b.eq_ignore_ascii_case("last") {
		CountPos::End
	    }
	    else {
		return err!("Pos must be 'begin' or 'end' {}", spec);
	    };
	    if a.is_empty() {
		self.name = "count".to_string();
	    }
	    else {
		self.name = a.to_string();
	    }
	    Ok(())
	} else {
	    err!("No comma found in '{}'. Format for Count is ColName,Position")
	}
    }
    fn assign(&mut self, tmp : &mut TextLine, new : &TextLine) {
	match self.which {
	    Which::First => {},
	    Which::Last => tmp.assign(new),
	    Which::Min => {
		if self.comp.comp_cols(tmp, new) == Ordering::Greater {
		    tmp.assign(new);
		}
	    }		
	    Which::Max => {
		if self.comp.comp_cols(tmp, new) == Ordering::Less {
		    tmp.assign(new);
		}
	    }		
	}
    }
    fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
	self.comp.lookup(fieldnames)
    }
}

pub fn main(argv: &[String]) -> Result<()> {
    let prog = args::ProgSpec::new("Select uniq lines.", args::FileCount::One);
    const A: [ArgSpec; 3] = [
	arg! {"key", "k", "Spec", "How to compare adjacent lines"},
	arg! {"count", "c", "ColName,Position", "Write the count of matchingn lines."},
	arg! {"which", "w", "(First,Last,Min,Max)[,LineCompare]", "Which of the matching lines should be printed."},
    ];
    let (args, files) = args::parse(&prog, &A, argv);

    let mut comp = LineCompList::new();
    let mut count = Count::default();
    for x in args {
        if x.name == "key" {
            comp.add(&x.value)?;
        } else if x.name == "count" {
	    count.get_count(&x.value)?;
        } else if x.name == "which" {
	    count.get_which(&x.value)?;
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
    count.lookup(&f.names())?;
    f.do_split = comp.need_split();
    let mut matches = 1;
    if count.which == Which::Last {
	loop {
            if f.getline()? {
		count.write(&mut w, matches, &f.prev_line(1).line, f.delim())?;
		break;
            }
            if comp.equal_cols(f.prev_line(1), f.curr_line()) {
		matches += 1;
	    }
	    else {
		count.write(&mut w, matches, &f.prev_line(1).line, f.delim())?;
		matches = 1;
            }
	}
    }
    else if count.which == Which::First && count.is_plain() {
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
    else {
	let mut tmp = f.curr_line().clone();
	loop {
            if f.getline()? {
		count.write(&mut w, matches, &tmp.line, f.delim())?;
		break;
            }
            if comp.equal_cols(f.prev_line(1), f.curr_line()) {
		count.assign(&mut tmp, f.curr_line());
		matches += 1;
	    }
	    else {
		count.write(&mut w, matches, &tmp.line, f.delim())?;
		tmp.assign(f.curr_line());
		matches = 1;
            }
	}
    }
    Ok(())
}
