//! parse and run tooltest files
use crate::comp::str_to_i_whole;
use crate::matcher::{MatchMaker, Matcher};
use crate::text::Text;
use crate::{err, get_reader, get_writer, prerr, Error, Infile, Result};
//use fs_err as fs;
use fs_err as fs;
use std::ffi::OsStr;
use std::io::{BufRead, Read, Write};
use std::os::unix::ffi::OsStrExt;
use std::path::Path;
use std::process::Command;
use tempdir::TempDir;

/// print a description of the test file format to stdout
pub fn show_format() {
    println!("#command CMD : the command to be run");
    println!("#stdin : followed by the input to the program. Default empty.");
    println!("#stdout : followed by the expected stdout output. Default empty.");
    println!("#stderr : followed by the expected stderr output. Default empty.");
    println!("#infile Filename : followed by the contents of the named file. '#infile foo' means that the file $TMP/foo is available to the command.");
    println!("#outfile Filename : followed by the contents of the named file. '#outfile foo' means that the file $TMP/foo must be created by the command, with exactly that contents.");
    println!("#nonewline : must follow the contents of one of the above four. Removes the final newline from the input or expected output.");
    println!("#status Number : the expected exit status. Default zero.");
    println!("'# ' : a line starting with # and a space is a comment, and is ignored");
}

#[derive(Debug, Clone, Default)]
struct InFile {
    name: String,
    content: Vec<u8>,
}

#[allow(missing_debug_implementations)]
struct OutFile {
    name: String,
    content: Vec<u8>,
    matcher: Matcher,
}
impl Default for OutFile {
    fn default() -> Self {
        Self {
            name: String::new(),
            content: Vec::new(),
            matcher: MatchMaker::make("empty").unwrap(),
        }
    }
}

/// One test to be run
#[allow(missing_debug_implementations)]
#[derive(Default)]
pub struct Test {
    name: String,
    cmd: Vec<u8>,
    stdin: Vec<u8>,
    stdout: OutFile,
    stderr: OutFile,
    code: i32,
    in_files: Vec<InFile>,
    out_files: Vec<OutFile>,
}

/// delete file from dir
fn delete_file(s: &str, dir: &str) -> Result<()> {
    let mut f = dir.to_string();
    f.push_str(s);
    std::fs::remove_file(f)?;
    Ok(())
}

/// remove item from Vec and delete file
fn nuke(v: &mut Vec<String>, s: &str, dir: &str, keep_files: bool) -> Result<()> {
    v.retain(|x| x != s);
    if keep_files {
        Ok(())
    } else {
        delete_file(s, dir)
    }
}

/// return directory as vec of file names
pub fn read_dir(dir: &Path) -> Result<Vec<String>> {
    let mut dirs = Vec::new();
    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        dirs.push(entry.file_name().to_string_lossy().to_string());
    }
    Ok(dirs)
}

/// if line doesn't start with tag, return false
/// read following lines into buff, until another # line is seen
/// set need_read to false if line ends up with unexamined data
fn grab(
    tag: &[u8],
    buff: &mut Vec<u8>,
    line: &mut Vec<u8>,
    reader: &mut Infile,
    need_read: &mut bool,
) -> Result<bool> {
    if let Some(x) = line.strip_prefix(tag) {
        if !x.is_empty() {
            prerr(&[b"Unexpected stuff after ", tag, b" : ", line]);
            return Err(Error::Silent);
        }
        loop {
            line.clear();
            let sz = reader.read_until(b'\n', line)?;
            if sz == 0 {
                break;
            }
            if line.starts_with(b"#") {
                if line.starts_with(b"#nonewline") {
                    if !buff.is_empty() {
                        buff.pop();
                    }
                } else {
                    *need_read = false;
                }
                break;
            } else {
                buff.extend(&*line);
            }
        }
        Ok(true)
    } else {
        Ok(false)
    }
}

impl Test {
    /// new
    pub fn new() -> Self {
        Self::default()
    }
    fn add_outfile(
        &mut self,
        tag: &str,
        line: &mut Vec<u8>,
        reader: &mut Infile,
        need_read: &mut bool,
    ) -> Result<bool> {
        if let Some(x) = line.strip_prefix(tag.as_bytes()) {
            let out = if x.is_empty() {
                let mut buf = Vec::new();
                grab(tag.as_bytes(), &mut buf, line, reader, need_read)?;
                OutFile {
                    name: tag[1..].to_string(),
                    matcher: MatchMaker::make2("exact", std::str::from_utf8(&buf)?)?,
                    content: buf,
                }
            } else {
                let x = x.trimw_start();
                OutFile {
                    name: tag[1..].to_string(),
                    matcher: MatchMaker::make(std::str::from_utf8(x)?)?,
                    content: x.to_vec(),
                }
            };
            if tag == "#stderr" {
                self.stderr = out;
            } else {
                self.stdout = out;
            }
            Ok(true)
        } else {
            Ok(false)
        }
    }
    /// parse the file
    pub fn open(&mut self, file: &str) -> Result<()> {
        self.name = file.to_string();
        let mut reader = get_reader(file)?;
        let mut line: Vec<u8> = Vec::new();
        // empty files always ignored
        let sz = reader.read_until(b'\n', &mut line)?;
        if sz == 0 {
            return Ok(());
        }
        loop {
            if line.last() == Some(&b'\n') {
                line.pop();
            }
            let mut need_read = true;
            if let Some(x) = line.strip_prefix(b"#command") {
                let y = x.trimw_start();
                self.cmd = y.to_vec();
            } else if self.add_outfile("#stdout", &mut line, &mut reader, &mut need_read)?
                || self.add_outfile("#stderr", &mut line, &mut reader, &mut need_read)?
                || grab(
                    b"#stdin",
                    &mut self.stdin,
                    &mut line,
                    &mut reader,
                    &mut need_read,
                )?
            {
                /* do nothing */
            } else if let Some(x) = line.strip_prefix(b"#status") {
                let y = x.trimw_start();
                self.code = str_to_i_whole(y)? as i32;
            } else if let Some(x) = line.strip_prefix(b"#infile") {
                let y = x.trimw_start();
                let mut f = InFile {
                    name: String::from_utf8(y.to_vec())?,
                    ..Default::default()
                };
                line.clear();
                grab(b"", &mut f.content, &mut line, &mut reader, &mut need_read)?;
                self.in_files.push(f);
            } else if let Some(x) = line.strip_prefix(b"#outfile") {
                let y = std::str::from_utf8(x)?;
                let y = y.trimw_start();
                if let Some((name, matcher)) = y.split_once(' ') {
                    let f = OutFile {
                        name: name.to_string(),
                        content: matcher.as_bytes().to_vec(),
                        matcher: MatchMaker::make(matcher)?,
                    };
                    self.out_files.push(f);
                } else {
                    let name = y.to_string();
                    line.clear();
                    let mut buf = Vec::new();
                    grab(b"", &mut buf, &mut line, &mut reader, &mut need_read)?;
                    let f = OutFile {
                        name,
                        matcher: MatchMaker::make2("exact", std::str::from_utf8(&buf)?)?,
                        content: buf,
                    };
                    self.out_files.push(f);
                }
            } else if line.starts_with(b"# ") {
                // comment
            } else {
                return err!(
                    "Unexpected line in test file : {}",
                    std::str::from_utf8(&line)?
                );
            }
            if need_read {
                line.clear();
                let sz = reader.read_until(b'\n', &mut line)?;
                if sz == 0 {
                    break;
                }
            }
        }
        Ok(())
    }

    /// run the test
    pub fn run(&mut self, config: &Config) -> Result<bool> {
        let mut keep_files = false;
        let mut tmp: String = if config.tmpdir.is_empty() {
            config.tmp.path().to_owned().to_string_lossy().to_string()
        } else {
            keep_files = true;
            config.tmpdir.clone()
        };
        if !tmp.ends_with('/') {
            tmp.push('/');
        }
        for x in &self.in_files {
            let mut fname = tmp.clone();
            fname.push_str(&x.name);
            let mut w = get_writer(&fname)?;
            w.write_all(&x.content)?;
        }
        #[allow(clippy::redundant_clone)]
        let mut tmp_stdin = tmp.clone();
        tmp_stdin.push_str("stdin");
        {
            let mut w = std::fs::OpenOptions::new()
                .write(true)
                .create(true)
                .open(&tmp_stdin)?;
            w.write_all(&self.stdin)?;
        }
        let ncmd = String::from_utf8_lossy(&self.cmd)
            .replace("$TMP", &tmp)
            .as_bytes()
            .to_vec();
        //        prerr(&[b"About to run ", &ncmd]);
        let cmd: Vec<&[u8]> = ncmd.split(|num| num <= &b' ').collect();
        if cmd.is_empty() {
            return err!("command is empty");
        }
        let mut basecmd = config.bindir.as_bytes().to_vec();
        basecmd.extend(cmd[0]);
        let mut tmp_cmd = Command::new(OsStr::from_bytes(&basecmd));
        for x in &cmd[1..] {
            //	    prerr(&[b"with arg ", x]);
            tmp_cmd.arg(OsStr::from_bytes(x));
        }
        let res = tmp_cmd
            .stdin(std::fs::File::open(&tmp_stdin).unwrap())
            .output();
        let output;
        match res {
            Err(x) => {
                prerr(&[b"Error trying to execute : ", &basecmd]);
                return Err(Error::IoError(x));
            }
            Ok(x) => {
                output = x;
            }
        }
        let mut files = read_dir(Path::new(&tmp))?;
        nuke(&mut files, &"stdin".to_string(), &tmp, keep_files)?;
        for x in &self.in_files {
            nuke(&mut files, &x.name, &tmp, keep_files)?;
        }
        let mut failed = false;
        match output.status.code() {
            Some(code) => {
                if code != self.code {
                    failed = true;
                    eprintln!("Exited with status code: {} instead of {}", code, self.code);
                }
            }
            None => {
                failed = true;
                eprintln!("Process terminated by signal");
            }
        }
        //	eprintln!("Got input {} {} {}", self.code, self.stdout.len(), self.stderr.len());
        //	eprintln!("Got output {} {} {}", output.status.code().unwrap(), output.stdout.len(), output.stderr.len());
        if !self.stderr.matcher.do_match_safe(&output.stderr) {
            failed = true;
            prerr(&[
                b"Stderr was\n",
                &output.stderr,
                b"\ninstead of\n",
                &self.stderr.content,
            ]);
        }
        if !self.stdout.matcher.do_match_safe(&output.stdout) {
            failed = true;
            prerr(&[
                b"Stdout was\n",
                &output.stdout,
                b"\ninstead of\n",
                &self.stdout.content,
            ]);
        }
        for x in &self.out_files {
            let mut fname = tmp.clone();
            fname.push_str(&x.name);
            match get_reader(&fname) {
                Err(e) => {
                    failed = true;
                    eprintln!("{:?}", e);
                }
                Ok(mut f) => {
                    let mut body = Vec::new();
                    f.read_to_end(&mut body)?;
                    if !x.matcher.do_match_safe(&body) {
                        failed = true;
                        prerr(&[
                            b"File ",
                            x.name.as_bytes(),
                            b" was\n",
                            &body,
                            b" but should have been\n",
                            &x.content,
                        ]);
                    }
                }
            }
            nuke(&mut files, &x.name, &tmp, keep_files)?;
        }
        if !keep_files && !files.is_empty() {
            failed = true;
            eprintln!("Files left over in TMP dir :");
            for x in &files {
                eprintln!("{}", x);
                delete_file(x, &tmp)?;
            }
        }
        if keep_files {
            let mut fname = tmp.clone();
            fname.push_str("stdout");
            let mut w = get_writer(&fname)?;
            w.write_all(&output.stdout)?;
            fname = tmp.clone();
            fname.push_str("stderr");
            w = get_writer(&fname)?;
            w.write_all(&output.stderr)?;
        }
        if failed {
            prerr(&[b"Test ", self.name.as_bytes(), b" failed ", &self.cmd]);
        }
        // if (Debug) tmp.into_path()
        Ok(!failed)
    }
}

/// Global config for running a bunch of tooltests
/// Accumulates statistics
#[derive(Debug)]
pub struct Config {
    pass: usize,
    fail: usize,
    bindir: String,
    tmpdir: String,
    tmp: TempDir,
}

impl Config {
    /// new
    pub fn new() -> Result<Self> {
        Ok(Self {
            pass: 0,
            fail: 0,
            bindir: "./".to_string(),
            tmpdir: String::new(),
            tmp: TempDir::new("tooltest")?,
        })
    }
    /// set tmp dir
    pub fn tmp(&mut self, path: &str) -> Result<()> {
        self.tmpdir = path.to_string();
        if !self.tmpdir.ends_with('/') {
            self.tmpdir.push('/');
        }
        fs::create_dir_all(&self.tmpdir)?;
        Ok(())
    }
    /// set bin dir
    pub fn bin(&mut self, path: &str) {
        self.bindir = path.to_string();
        if !self.bindir.ends_with('/') {
            self.bindir.push('/');
        }
    }
    /// report
    pub fn report(&self) -> Result<()> {
        println!(
            "{} test run, {} pass, {} failed",
            self.pass + self.fail,
            self.pass,
            self.fail
        );
        if self.fail > 0 {
            Err(Error::Silent)
        } else {
            Ok(())
        }
    }
    fn do_run(&mut self, file: &str) -> Result<bool> {
        let mut t = Test::new();
        t.open(file)?;
        t.run(self)
    }
    /// run
    pub fn run(&mut self, file: &str) {
        match self.do_run(file) {
            Ok(x) => {
                if x {
                    self.pass += 1;
                } else {
                    self.fail += 1;
                }
            }
            Err(x) => {
                self.fail += 1;
                eprintln!("Failure : {}", x);
                prerr(&[b"Test ", file.as_bytes(), b" failed "]);
            }
        }
    }
}

impl Default for Config {
    fn default() -> Self {
        Self::new().unwrap()
    }
}
