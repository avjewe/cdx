use crate::prelude::*;
use cdx::prelude::*;
use cdx::agg::{AggMaker};
use cdx::comp::{CompMaker, LineComp};

// --min
// --max
//      print nothing if exceeded
//      add a new column with 'exceeded' or 'ok'
//      print first 'max' if exceeded
//            print others somewhere else
// --bipart=Col1,Col2,Min
// --integer=Col[,Min]

// --merge
// sum,min,max,mean,merge

// rename Count and count

#[derive(PartialEq, Debug)]
enum CountPos {
    None,
    Begin,
    End,
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
    Max,
}
impl Default for Which {
    fn default() -> Self {
        Self::First
    }
}
#[derive(Default)]
struct Count {
    name: String,
    pos: CountPos,
    which: Which,
    comp: LineComp,
}

fn chomp(x: &[u8]) -> &[u8] {
    if !x.is_empty() {
        let len = x.len() - 1;
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
    fn write(&self, w: &mut impl Write, num: usize, line: &[u8], delim: u8) -> Result<()> {
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
    fn get_which2(&mut self, which: &str, pattern: &str) -> Result<()> {
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
    fn get_which(&mut self, spec: &str) -> Result<()> {
        if let Some((a, b)) = spec.split_once(',') {
            self.get_which2(a, b)
        } else {
            self.get_which2(spec, "")
        }
    }
    fn get_count(&mut self, spec: &str) -> Result<()> {
        if let Some((a, b)) = spec.split_once(',') {
            self.pos = if b.is_empty()
                || b.eq_ignore_ascii_case("begin")
                || b.eq_ignore_ascii_case("start")
                || b.eq_ignore_ascii_case("first")
            {
                CountPos::Begin
            } else if b.eq_ignore_ascii_case("stop")
                || b.eq_ignore_ascii_case("end")
                || b.eq_ignore_ascii_case("last")
            {
                CountPos::End
            } else {
                return err!("Pos must be 'begin' or 'end' {}", spec);
            };
            if a.is_empty() {
                self.name = "count".to_string();
            } else {
                self.name = a.to_string();
            }
        } else {
            self.pos = CountPos::Begin;
            if spec.is_empty() {
                self.name = "count".to_string();
            } else {
                self.name = spec.to_string();
            }
        }
        Ok(())
    }
    fn assign(&mut self, tmp: &mut TextLine, new: &TextLine) {
        match self.which {
            Which::First => {}
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
    const A: [ArgSpec; 7] = [
        arg! {"agg", "a", "Col,Spec", "Merge value from this column, in place."},
        arg! {"agg-pre", "", "NewCol,SrcCol,Spec", "Merge value from SrcCol into new column, before other columns."},
        arg! {"agg-post", "", "NewCol,SrcCol,Spec", "Merge value from SrcCol into new column, after other columns."},
        arg! {"key", "k", "Spec", "How to compare adjacent lines"},
        arg! {"count", "c", "ColName,Position", "Write the count of matching line."},
        arg! {"which", "w", "(First,Last,Min,Max)[,LineCompare]", "Which of the matching lines should be printed."},
        arg! {"agg-help", "", "", "Print help for aggregators"},
    ];
    let (args, files) = args::parse(&prog, &A, argv);

    let mut agg = LineAggList::new();
    let mut comp = LineCompList::new();
    let mut count = Count::default();

    for x in args {
        if x.name == "key" {
            comp.add(&x.value)?;
        } else if x.name == "count" {
            count.get_count(&x.value)?;
        } else if x.name == "which" {
            count.get_which(&x.value)?;
        } else if x.name == "agg" {
            agg.push_replace(&x.value)?;
        } else if x.name == "agg-post" {
            agg.push_append(&x.value)?;
        } else if x.name == "agg-pre" {
            agg.push_prefix(&x.value)?;
        } else if x.name == "agg-help" {
            AggMaker::help();
            return Ok(());
        } else {
            unreachable!();
        }
    }

    assert_eq!(files.len(), 1);

    let mut f = Reader::new();
    f.open(&files[0])?;
    if f.is_empty() {
        return Ok(());
    }
    comp.lookup(&f.names())?;
    count.lookup(&f.names())?;
    let mut c_write = Writer::new(f.delim());
    if !agg.is_empty() {
        if count.pos == CountPos::Begin {
            agg.push_first_prefix(&format!("{},1,count", count.name))?;
        }
        if count.pos == CountPos::End {
            agg.push_append(&format!("{},1,count", count.name))?;
        }
        agg.lookup(&f.names())?;
        agg.fill(&mut c_write, f.header());
        c_write.lookup(&f.names())?;
    }

    let mut w = get_writer("-")?;
    if f.has_header() {
        let mut ch = ColumnHeader::new();
        if agg.is_empty() {
            if count.pos == CountPos::Begin {
                ch.push(&count.name)?;
            }
            ch.push_all(f.header())?;
            if count.pos == CountPos::End {
                ch.push(&count.name)?;
            }
        } else {
            c_write.add_names(&mut ch, f.header())?;
        }
        w.write_all(ch.get_head(f.delim()).as_bytes())?;
    }
    if f.is_done() {
        return Ok(());
    }

    f.do_split(comp.need_split());
    let mut matches = 1;
    if !agg.is_empty() {
        agg.add(f.curr_line());
        let mut tmp = f.curr_line().clone();
        loop {
            if f.getline()? {
                c_write.write(&mut w, &tmp)?;
                break;
            }
            if comp.equal_cols(f.prev_line(1), f.curr_line()) {
                count.assign(&mut tmp, f.curr_line());
                agg.add(f.curr_line());
            } else {
                c_write.write(&mut w, &tmp)?;
                tmp.assign(f.curr_line());
                agg.reset();
                agg.add(f.curr_line());
            }
        }
    } else if count.which == Which::Last {
        loop {
            if f.getline()? {
                count.write(&mut w, matches, f.prev_line(1).line(), f.delim())?;
                break;
            }
            if comp.equal_cols(f.prev_line(1), f.curr_line()) {
                matches += 1;
            } else {
                count.write(&mut w, matches, f.prev_line(1).line(), f.delim())?;
                matches = 1;
            }
        }
    } else if count.which == Which::First && count.is_plain() {
        f.write_curr(&mut w)?;
        loop {
            if f.getline()? {
                break;
            }
            if !comp.equal_cols(f.prev_line(1), f.curr_line()) {
                f.write_curr(&mut w)?;
            }
        }
    } else {
        let mut tmp = f.curr_line().clone();
        loop {
            if f.getline()? {
                count.write(&mut w, matches, tmp.line(), f.delim())?;
                break;
            }
            if comp.equal_cols(f.prev_line(1), f.curr_line()) {
                count.assign(&mut tmp, f.curr_line());
                matches += 1;
            } else {
                count.write(&mut w, matches, tmp.line(), f.delim())?;
                tmp.assign(f.curr_line());
                matches = 1;
            }
        }
    }
    Ok(())
}
