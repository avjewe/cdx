use crate::args::ArgSpec;
use crate::{arg, args};
use cdx::expr;
use cdx::matcher::*;
use cdx::util::{Error, LookbackReader, Result, err, TextLine, prerr_n};
use cdx::comp::{LineCompList, comp_check};
use std::str::FromStr;
use std::cmp::Ordering;

// FIXME, shuld be in util or something
#[derive(Debug, Copy, Clone)]
enum CompareOp {
    LT,
    GT,
    LE,
    GE,
    EQ,
    NE
}
impl Default for CompareOp {
    fn default() -> Self {
        Self::LT
    }
}

impl FromStr for CompareOp {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
	if s.eq_ignore_ascii_case("lt") {
	    Ok(Self::LT)
	} else if s.eq_ignore_ascii_case("gt") {
	    Ok(Self::GT)
	} else if s.eq_ignore_ascii_case("le") {
	    Ok(Self::LE)
	} else if s.eq_ignore_ascii_case("ge") {
	    Ok(Self::GE)
	} else if s.eq_ignore_ascii_case("eq") {
	    Ok(Self::EQ)
	} else if s.eq_ignore_ascii_case("ne") {
	    Ok(Self::NE)
	} else {
	    err!("Invalid CompareOp, should be one of LT,GT,LE,GE,EQ,NE : '{}'", s)
	}
    }
}

impl CompareOp {
    // invert left vs right. That is , swap less vs greater, but not equal vs not equal
    fn invert(&self) -> Self {
	use CompareOp::*;
	match self {
	    LT => GT,
	    GT => LT,
	    LE => GE,
	    GE => LE,
	    EQ => EQ,
	    NE => NE,
	}
    }
    // return (line OP value), writing to stderr if false
    fn line_ok_verbose(&self, line : &TextLine, comp : &mut LineCompList, line_num : usize) -> bool {
	if !self.invert().line_ok(line, comp) {
	    eprint!("Line {} : ", line_num);
	    prerr_n(&[&line.line]);
	    eprint!("should have been {:?} ", self);
	    prerr_n(&[comp.get_value()]);
	    eprintln!(" but wasn't");
	    false
	}
	else {
	    true
	}
    }
    // return (line OP value)
    fn line_ok(&self, line : &TextLine, comp : &mut LineCompList) -> bool {
	let o = comp.comp_self_cols(line);
	self.ord_ok(o)
    }
    fn ord_ok(&self, o : Ordering) -> bool {
	use CompareOp::*;
	use Ordering::*;
	match self {
	    LT => o == Less,
	    GT => o == Greater,
	    LE => o != Greater,
	    GE => o != Less,
	    EQ => o == Equal,
	    NE => o != Equal,
	}
    }
}

#[derive(Debug, Default)]
struct CompareOpVal {
    op : CompareOp,
    val : Vec<u8>,
}
impl CompareOpVal {
    fn new(spec : &str) -> Result<Self> {
	let mut s = Self::default();
	s.set(spec)?;
	Ok(s)
    }
    fn set(&mut self, spec : &str) -> Result<()> {
        if let Some((a, b)) = spec.split_once(',') {
	    self.op = a.parse::<CompareOp>()?;
	    self.val = b.as_bytes().to_owned();
	    Ok(())
        } else {
            err!("Format for CompareOp is Op,Value : '{}'", spec)
        }
    }
    fn line_ok_verbose(&self, line : &TextLine, comp : &mut LineCompList, line_num : usize) -> Result<bool> {
	comp.set(&self.val, b',')?;
	Ok(self.op.line_ok_verbose(line, comp, line_num))
    }
}

// MORE MATCHERS
// number of columns
// --full-name - exact column names in order
// lines and bytes as expr
// lines and bytes relative to another file
// a bunch of columns not equaling each other
// two columns compared to each other
// only check first N, ot maybe also last N? or maybe tht's just a script with head and tail?
// still return OK if N lines or N% of the lines are bad

pub fn main(argv: &[String]) -> Result<()> {
    let prog = args::ProgSpec::new("Verify file contents.", args::FileCount::Many);
    const A: [ArgSpec; 11] = [
        arg! {"report", "r", "Number", "How many failures to report before exit."},
        arg! {"first", "f", "Op,Value", "'FirstLine Op Value' must be true. E.g LT,a for first line is ess than 'a'."},
        arg! {"last", "l", "Op,Value", "'LastLine Op Value' must be true."},
        arg! {"key", "k", "Spec", "How to compare adjacent lines"},
        arg! {"sort", "s", "", "Check that the file is sorted."},
        arg! {"unique", "u", "", "Check that the file is sorted, with unique lines."},
        arg! {"pattern", "p", "Col,Spec,Pattern", "Select line where this col matches this pattern."},
        arg! {"show-matchers", "", "", "Print available matchers"},
        arg! {"show-const", "", "", "Print available constants"},
        arg! {"show-func", "", "", "Print available functions"},
        arg! {"or", "o", "", "A line matches if any of the matchers matches."},
    ];
    let (args, files) = args::parse(&prog, &A, argv);

    let mut list = MultiLineMatcher::new(MultiMode::And);
    let mut comp = LineCompList::new();
    let mut do_sort = false;
    let mut do_unique = false;
    let mut max_fails = 5;
    let mut first : Option<CompareOpVal> = None;
    let mut last : Option<CompareOpVal> = None;
    
    for x in args {
        if x.name == "pattern" {
            list.push_spec(&x.value)?;
        } else if x.name == "show-matchers" {
            MatchMaker::help();
            return Ok(());
        } else if x.name == "key" {
	    comp.add(&x.value)?;
        } else if x.name == "or" {
            list.multi = MultiMode::Or;
        } else if x.name == "fail" {
	    max_fails = x.value.parse::<usize>()?;
        } else if x.name == "sort" {
	    do_sort = true;
        } else if x.name == "first" {
	    first = Some(CompareOpVal::new(&x.value)?);
        } else if x.name == "last" {
	    last = Some(CompareOpVal::new(&x.value)?);
        } else if x.name == "unique" {
	    do_sort = true;
	    do_unique = true;
        } else if x.name == "show-const" {
            expr::show_const();
            return Ok(());
        } else if x.name == "show-func" {
            expr::show_func();
            return Ok(());
        } else {
            unreachable!();
        }
    }
    if comp.is_empty() {
        comp.add("")?;
    }

    let mut fails = 0;
    for x in &files {
        let mut f = LookbackReader::new(1);
        f.open(x)?;
        if f.is_empty() {
            continue;
        }
        list.lookup(&f.names())?;
        comp.lookup(&f.names())?;
        if f.is_done() {
            continue;
        }
	if first.is_some() && !first.as_ref().unwrap().line_ok_verbose(f.curr_line(), &mut comp, f.line_number())? {
	    fails += 1;
	}
        loop {
	    let mut did_fail = false;
            if !list.ok_verbose(f.curr_line(), f.line_number(), x) {
		did_fail = true;
            }
            if f.getline()? {
		if last.is_some() && !last.as_ref().unwrap().line_ok_verbose(f.prev_line(1), &mut comp, f.line_number()-1)? {
		    fails += 1;
		}
		break;
            }
	    if do_sort {
		did_fail = did_fail || comp_check(&f, &mut comp, do_unique);
	    }
	    if did_fail {
		fails += 1;
                if fails >= max_fails {
                    break;
                }
	    }
        }
        if fails > 0 {
            return Err(Error::Silent);
        }
    }
    Ok(())
}
