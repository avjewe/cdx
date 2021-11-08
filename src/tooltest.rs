//! parse and run tooltest files
use crate::comp::skip_leading_white;
use crate::prerr;
use crate::{err, get_reader, Error, Infile, Result};
use std::ffi::OsStr;
use std::io::{BufRead, Write};
use std::os::unix::ffi::OsStrExt;
use std::process::Command;

/// One test to be run
#[derive(Debug, Clone)]
pub struct Test {
    name: String,
    cmd: Vec<u8>,
    stdin: Vec<u8>,
    stdout: Vec<u8>,
    stderr: Vec<u8>,
    code: i32,
}

fn u8toi64(mut buf: &[u8]) -> i64 {
    if buf.is_empty() {
        return 0;
    }
    let mut neg: i64 = 1;
    if buf[1] == b'-' {
        buf = &buf[1..];
        neg = -1;
    }
    let mut x: i64 = 0;

    for ch in buf {
        if !ch.is_ascii_digit() {
            break;
        }
        x = (x * 10) + (ch - b'0') as i64;
    }
    x * neg
}

#[allow(dead_code)]
fn u8tou64(buf: &[u8]) -> u64 {
    let mut x: u64 = 0;

    for ch in buf {
        if !ch.is_ascii_digit() {
            break;
        }
        x = (x * 10) + (ch - b'0') as u64;
    }
    x
}

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
                *need_read = false;
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
        Self {
            name: String::new(),
            cmd: Vec::new(),
            stdin: Vec::new(),
            stdout: Vec::new(),
            stderr: Vec::new(),
            code: 0,
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
                self.code = u8toi64(y) as i32;
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
        let mut tmp = std::env::temp_dir();
        tmp.push("stdin");
        {
            let mut w = std::fs::OpenOptions::new()
                .write(true)
                .create(true)
                .open(&tmp.as_os_str())?;
            w.write_all(&self.stdin)?;
        }
        //	prerr(&[b"About to run ", &self.cmd])?;
        let cmd: Vec<&[u8]> = self.cmd.split(|num| num <= &b' ').collect();
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
        let res = tmp_cmd.stdin(std::fs::File::open(&tmp).unwrap()).output();
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
        std::fs::remove_file(&tmp)?;
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
        if failed {
            prerr(&[b"Test ", self.name.as_bytes(), b" failed ", &self.cmd])?;
        }
        Ok(!failed)
    }
}

impl Default for Test {
    fn default() -> Self {
        Self::new()
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
