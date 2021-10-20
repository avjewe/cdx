use super::comp::*;
use super::*;

pub fn u2s(v: &[u8]) -> String {
    String::from_utf8_lossy(v).to_string()
}

#[derive(Debug)]
pub struct BlockReader {
    file: Infile,
    pub header: TextLine,
    pub delim: u8,
    pub is_done: bool,
    pub is_empty: bool,
}

impl BlockReader {
    pub fn new() -> Self {
        Self {
            file: Infile::new(io::BufReader::new(Box::new(io::empty()))),
            header: TextLine::new(),
            delim: b'\t',
            is_done: true,
            is_empty: true,
        }
    }
    pub fn open(&mut self, name: &str) -> Result<()> {
        self.file = get_reader(name)?;
        Ok(())
        /*
                read_header(
                    &mut self.file,
                    &mut self.header,
                    &mut self.line,
                    &mut self.delim,
                    &mut self.is_done,
                    &mut self.is_empty,
                )
        */
    }
}

impl Default for BlockReader {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct Sorter {
    ptrs: Vec<Item>,
    data: Vec<u8>,
    //    used : u32,
}

impl Sorter {
    pub fn new() -> Self {
        Self {
            ptrs: Vec::new(),
            data: Vec::new(),
            //	    used : 0
        }
    }
    pub fn add(
        &mut self,
        _r: &mut BlockReader,
        cmp: &Comparator,
        _w: &mut dyn Write,
    ) -> Result<()> {
        self.ptrs
            .sort_by(|a, b| cmp.comp.comp_items(cmp, &self.data, a, b));
        Ok(())
    }
    pub fn finalize(&mut self, cmp: &Comparator, _w: &mut dyn Write) -> Result<()> {
        self.ptrs
            .sort_by(|a, b| cmp.comp.comp_items(cmp, &self.data, a, b));
        Ok(())
    }
}

impl Default for Sorter {
    fn default() -> Self {
        Self::new()
    }
}

pub fn sort(files: &[String], cmp: &mut Comparator, w: &mut dyn Write) -> Result<()> // maybe return some useful stats?
{
    let mut s = Sorter::new();
    let mut header: Vec<u8> = Vec::new();
    let mut first_file = true;

    for f in files {
        let mut b = BlockReader::new();
        b.open(f)?;
        if first_file {
            first_file = false;
            header = b.header.line.clone();
            w.write_all(&header)?;
        } else if b.header.line != header {
            return err!("Header Mismatch : '{}' vs '{}'", &u2s(&header), &u2s(&b.header.line));
        }
        s.add(&mut b, cmp, w)?;
    }
    s.finalize(cmp, w)
}
