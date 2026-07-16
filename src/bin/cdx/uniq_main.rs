use crate::prelude::*;
use cdx::comp::{CompMaker, LineComp};
use cdx::prelude::*;
use cdx::*;

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

#[derive(PartialEq, Debug, Default)]
enum CountPos {
    #[default]
    None,
    Begin,
    End,
}

#[derive(PartialEq, Debug, Default)]
enum Which {
    #[default]
    First,
    Last,
    Min,
    Max,
}
#[derive(Default)]
struct Count {
    name: String,
    pos: CountPos,
    which: Which,
    comp: LineComp,
}

impl Count {
    fn is_plain(&self) -> bool {
        self.pos == CountPos::None
    }
    fn write(&self, w: &mut impl Write, num: usize, line: &[u8], delim: u8) -> Result<()> {
        match self.pos {
            CountPos::None => {
                w.write_all(line)?;
                w.write_all(b"\n")?;
            }
            CountPos::Begin => {
                write!(w, "{}{}", num, delim as char)?;
                w.write_all(line)?;
                w.write_all(b"\n")?;
            }
            CountPos::End => {
                w.write_all(line)?;
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
            Which::Last => *tmp = new.clone(),
            Which::Min => {
                if self.comp.comp_cols(tmp, new) == Ordering::Greater {
                    *tmp = new.clone();
                }
            }
            Which::Max => {
                if self.comp.comp_cols(tmp, new) == Ordering::Less {
                    *tmp = new.clone();
                }
            }
        }
    }
    fn lookup(&mut self, field_names: &[String]) -> Result<()> {
        self.comp.lookup(field_names)
    }
}

pub fn main(argv: &[String], settings: &mut Settings) -> Result<()> {
    let prog = args::ProgSpec::new("Select uniq lines.", args::FileCount::One);
    const A: [ArgSpec; 8] = [
        arg! {"agg", "a", "Col,Spec", "Merge value from this column, in place."},
        arg! {"agg-pre", "", "NewCol,SrcCol,Spec", "Merge value from SrcCol into new column, before other columns."},
        arg! {"agg-post", "", "NewCol,SrcCol,Spec", "Merge value from SrcCol into new column, after other columns."},
        arg! {"key", "k", "Spec", "How to compare adjacent lines"},
        arg! {"count", "c", "ColName,Position", "Write the count of matching line."},
        arg! {"which", "w", "(First,Last,Min,Max)[,LineCompare]", "Which of the matching lines should be printed."},
        arg! {"agg-help", "", "", "Print help for aggregators"},
        arg! {"hash", "", "", "Input need not be ordered. Items are compared by 128 bit hashes, so false positives are theoretically possible."},
    ];
    let (args, files) = args::parse(&prog, &A, argv, settings)?;

    let mut agg = LineAggList::new();
    let mut comp = LineCompList::new();
    let mut count = Count::default();
    let mut hash = false;

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
        } else if x.name == "hash" {
            hash = true;
        } else {
            unreachable!();
        }
    }

    assert_eq!(files.len(), 1);
    let mut w = get_writer("-")?;
    let mut lw = LineWriter::default();

    // FIXME - if hash then fail if other things are set
    // FIXME - HEADER??
    if hash {
        let mut f = util::get_reader(&files[0])?;
        let mut line = Vec::new();
        let mut loc = util::FileLocData::new(&files[0]);
        let mut set = hash128::new_hash_set();
        loop {
            let eol =
                read_line::read(&mut line, &mut f.0, &settings.input.column_config, &mut loc)?;
            if eol == read_line::ReadResult::Eof {
                return Ok(());
            }
            if hash128::insert(&mut set, &line) {
                w.write_all(&line)?;
                w.write_all(eol.as_bytes())?;
            }
        }
    }

    let mut f = input_file::TextFilePrev::new(&files[0], &settings.input)?;
    if f.is_empty() {
        return Ok(());
    }
    comp.lookup(f.names())?;
    count.lookup(f.names())?;
    let mut c_write = Writer::new(settings.text_out());
    if !agg.is_empty() {
        if count.pos == CountPos::Begin {
            agg.push_first_prefix(&format!("{},1,count", count.name))?;
        }
        if count.pos == CountPos::End {
            agg.push_append(&format!("{},1,count", count.name))?;
        }
        agg.lookup(f.names())?;
        agg.fill(&mut c_write, f.names());
        c_write.lookup(f.names())?;
    }
    lw.from_spec(&f, &settings.output);

    if f.has_header() {
        let mut ch = ColumnHeader::new();
        if agg.is_empty() {
            if count.pos == CountPos::Begin {
                ch.push(&count.name)?;
            }
            ch.push_all(f.names())?;
            if count.pos == CountPos::End {
                ch.push(&count.name)?;
            }
        } else {
            c_write.add_names(&mut ch, f.names())?;
        }
        ch.add_header(&mut lw)?;
        if settings.checker.check_output(&lw, &f)? {
            lw.write_cdx(&mut w.0)?;
        }
        lw.clear();
    }
    if f.is_done() {
        return Ok(());
    }

    f.do_split(comp.need_split());
    let mut matches = 1;
    if !agg.is_empty() {
        agg.add(f.values());
        let mut tmp = f.values().clone();
        loop {
            if f.get_line()? {
                c_write.write(&mut w.0, &tmp)?;
                break;
            }
            if comp.equal_cols(&f.1, f.values()) {
                count.assign(&mut tmp, f.values());
                agg.add(f.values());
            } else {
                c_write.write(&mut w.0, &tmp)?;
                tmp = f.values().clone();
                agg.reset();
                agg.add(f.values());
            }
        }
    } else if count.which == Which::Last {
        loop {
            if f.get_line()? {
                count.write(&mut w.0, matches, f.1.line(), f.delim())?;
                break;
            }
            if comp.equal_cols(&f.1, f.values()) {
                matches += 1;
            } else {
                count.write(&mut w.0, matches, f.1.line(), f.delim())?;
                matches = 1;
            }
        }
    } else if count.which == Which::First && count.is_plain() {
        f.write_value_line(&mut w.0)?;
        loop {
            if f.get_line()? {
                break;
            }
            if !comp.equal_cols(&f.1, f.values()) {
                f.write_value_line(&mut w.0)?;
            }
        }
    } else {
        let mut tmp = f.values().clone();
        loop {
            if f.get_line()? {
                count.write(&mut w.0, matches, tmp.line(), f.delim())?;
                break;
            }
            if comp.equal_cols(&f.1, f.values()) {
                count.assign(&mut tmp, f.values());
                matches += 1;
            } else {
                count.write(&mut w.0, matches, tmp.line(), f.delim())?;
                tmp = f.values().clone();
                matches = 1;
            }
        }
    }
    Ok(())
}
