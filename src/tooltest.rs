//! parse and run tooltest files
use crate::comp::{skip_leading_white, str_to_i_whole};
use crate::prerr;
use crate::{err, get_reader, get_writer, Error, Infile, Result};
//use fs_err as fs;
use fs_err as fs;
use std::ffi::OsStr;
use std::io::{BufRead, Read, Write};
use std::os::unix::ffi::OsStrExt;
use std::path::Path;
use std::process::Command;
use tempdir::TempDir;

#[derive(Debug, Clone, Default)]
struct OneFile {
    name: String,
    content: Vec<u8>,
}

/// One test to be run
#[derive(Debug, Clone, Default)]
pub struct Test {
    name: String,
    cmd: Vec<u8>,
    stdin: Vec<u8>,
    stdout: Vec<u8>,
    stderr: Vec<u8>,
    code: i32,
    in_files: Vec<OneFile>,
    out_files: Vec<OneFile>,
}

/// remove item from Vec
fn nuke(v: &mut Vec<String>, s: &str) {
    v.retain(|x| x != s);
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
            prerr(&[b"Unexpected stuff after ", tag, b" : ", line])?;
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
                let y = skip_leading_white(x);
                self.cmd = y.to_vec();
            } else if grab(
                b"#stdin",
                &mut self.stdin,
                &mut line,
                &mut reader,
                &mut need_read,
            )? || grab(
                b"#stdout",
                &mut self.stdout,
                &mut line,
                &mut reader,
                &mut need_read,
            )? || grab(
                b"#stderr",
                &mut self.stderr,
                &mut line,
                &mut reader,
                &mut need_read,
            )? {
            } else if let Some(x) = line.strip_prefix(b"#status") {
                let y = skip_leading_white(x);
                self.code = str_to_i_whole(y)? as i32;
            } else if let Some(x) = line.strip_prefix(b"#infile") {
                let y = skip_leading_white(x);
                let mut f = OneFile {
                    name: String::from_utf8(y.to_vec())?,
                    ..Default::default()
                };
                line.clear();
                grab(b"", &mut f.content, &mut line, &mut reader, &mut need_read)?;
                self.in_files.push(f);
            } else if let Some(x) = line.strip_prefix(b"#outfile") {
                let y = skip_leading_white(x);
                let mut f = OneFile {
                    name: String::from_utf8(y.to_vec())?,
                    ..Default::default()
                };
                line.clear();
                grab(b"", &mut f.content, &mut line, &mut reader, &mut need_read)?;
                self.out_files.push(f);
            } else if line.starts_with(b"# ") {
                // comment
            } else {
                return err!("Unexpected line in test file : {:#?}", line);
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
        let tmp = TempDir::new("tooltest")?;
        for x in &self.in_files {
            let mut fname = tmp.path().to_owned();
            fname.push(&x.name);
            let mut w = get_writer(&fname.to_string_lossy())?;
            w.write_all(&x.content)?;
        }
        #[allow(clippy::redundant_clone)]
        let mut tmp_stdin = tmp.path().to_owned();
        tmp_stdin.push("stdin");
        {
            let mut w = std::fs::OpenOptions::new()
                .write(true)
                .create(true)
                .open(&tmp_stdin.as_os_str())?;
            w.write_all(&self.stdin)?;
        }
        let ncmd = String::from_utf8_lossy(&self.cmd)
            .replace("$TMP", &tmp.path().display().to_string())
            .as_bytes()
            .to_vec();
        //        prerr(&[b"About to run ", &ncmd])?;
        let cmd: Vec<&[u8]> = ncmd.split(|num| num <= &b' ').collect();
        if cmd.is_empty() {
            return err!("command is empty");
        }
        let mut basecmd = config.bindir.as_bytes().to_vec();
        basecmd.extend(cmd[0]);
        let mut tmp_cmd = Command::new(OsStr::from_bytes(&basecmd));
        for x in &cmd[1..] {
            //	    prerr(&[b"with arg ", x])?;
            tmp_cmd.arg(OsStr::from_bytes(x));
        }
        let res = tmp_cmd
            .stdin(std::fs::File::open(&tmp_stdin).unwrap())
            .output();
        let output;
        match res {
            Err(x) => {
                prerr(&[b"Error trying to execute : ", &basecmd])?;
                return Err(Error::IoError(x));
            }
            Ok(x) => {
                output = x;
            }
        }
        let mut files = read_dir(tmp.path())?;
        nuke(&mut files, &"stdin".to_string());
        for x in &self.in_files {
            nuke(&mut files, &x.name);
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
        if output.stderr != self.stderr {
            failed = true;
            prerr(&[
                b"Stderr was\n",
                &output.stderr,
                b"\ninstead of\n",
                &self.stderr,
            ])?;
        }
        if output.stdout != self.stdout {
            failed = true;
            prerr(&[
                b"Stdout was\n",
                &output.stdout,
                b"\ninstead of\n",
                &self.stdout,
            ])?;
        }
        for x in &self.out_files {
            nuke(&mut files, &x.name);
            let mut fname = tmp.path().to_owned();
            fname.push(&x.name);
            match get_reader(&fname.to_string_lossy()) {
                Err(e) => {
                    failed = true;
                    eprintln!("{:?}", e);
                }
                Ok(mut f) => {
                    let mut body = Vec::new();
                    f.read_to_end(&mut body)?;
                    if body != x.content {
                        failed = true;
                        prerr(&[
                            b"File ",
                            x.name.as_bytes(),
                            b" was\n",
                            &body,
                            b" but should have been\n",
                            &x.content,
                        ])?;
                    }
                }
            }
        }
        if !files.is_empty() {
            failed = true;
            eprintln!("Files left over in TMP dir :");
            for x in &files {
                eprintln!("{}", x);
            }
        }
        if failed {
            prerr(&[b"Test ", self.name.as_bytes(), b" failed ", &self.cmd])?;
        }
        // if (Debug) tmp.into_path()
        Ok(!failed)
    }
}

/// Global config for running a bunch of tooltests
/// Accumulates statistics
#[derive(Debug, Clone)]
pub struct Config {
    pass: usize,
    fail: usize,
    bindir: String,
}

impl Config {
    /// new
    pub fn new() -> Self {
        Self {
            pass: 0,
            fail: 0,
            bindir: "./".to_string(),
        }
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
    /// run
    pub fn run(&mut self, file: &str) -> Result<bool> {
        let mut t = Test::new();
        t.open(file)?;
        if t.run(self)? {
            self.pass += 1;
        } else {
            self.fail += 1;
        }
        Ok(true)
    }
}

impl Default for Config {
    fn default() -> Self {
        Self::new()
    }
}
