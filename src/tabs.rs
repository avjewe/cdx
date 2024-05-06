//! Format plain text to a fixed width terminal

use crate::prelude::*;
use unicode_truncate::UnicodeTruncateStr;
use unicode_width::UnicodeWidthStr;

#[derive(Debug, Default, Copy, Clone)]
/// the size of a box on the screen
pub struct Rect {
    /// width in character
    pub width: usize,
    /// height in lines
    pub height: usize,
}
impl Rect {
    /// new
    #[must_use]
    pub const fn new(width: usize, height: usize) -> Self {
        Self { width, height }
    }
    /// get screen size
    #[must_use]
    pub fn from_screen() -> Self {
        let mut ws = libc::winsize {
            ws_row: 0,
            ws_col: 0,
            ws_xpixel: 0,
            ws_ypixel: 0,
        };
        if unsafe { libc::ioctl(2, libc::TIOCGWINSZ, &mut ws) } >= 0 {
            Self {
                width: ws.ws_col as usize,
                height: (ws.ws_row - 1) as usize,
            }
        } else {
            Self {
                width: 100,
                height: 24,
            }
        }
    }
}

fn dec_max(sizes: &mut [usize]) {
    let mut max_pos = 0;
    let mut max = 0;
    for (i, x) in sizes.iter().enumerate() {
        if *x > max {
            max = *x;
            max_pos = i;
        }
    }
    sizes[max_pos] -= 1;
}

/// show the file on the screen
pub fn basic(file: &str, text: &TextFileMode) -> Result<()> {
    let mut x = Rect::from_screen();
    x.width -= 1;
    show(file, &Rect::from_screen(), text)
}
/// show the file in a specific rectangle
pub fn show(file: &str, screen: &Rect, text: &TextFileMode) -> Result<()> {
    let mut f = Reader::new(text);
    f.open(file)?;
    if f.is_empty() {
        return Ok(());
    }
    let mut lines: Vec<StringLine> = Vec::new();
    let mut sizes: Vec<usize> = vec![0; f.header().len()];
    if f.has_header() {
        lines.push(f.header().clone());
    }
    if !f.is_done() {
        while lines.len() < screen.height {
            let mut s = StringLine::new();
            s.line = String::from_utf8_lossy(f.curr().line()).to_string();
            s.split(f.delim());
            lines.push(s);
            if f.getline()? {
                break;
            }
        }
    }
    for x in &lines {
        for (i, c) in x.iter().enumerate() {
            let width = UnicodeWidthStr::width(c);
            if sizes[i] < width {
                sizes[i] = width;
            }
        }
    }
    let mut total: usize = sizes.iter().sum();
    let target = screen.width - sizes.len();
    while total > target {
        dec_max(&mut sizes);
        total = sizes.iter().sum();
    }
    let mut w = get_writer("-")?;
    let mut do_center = f.has_header();
    for x in &lines {
        let mut need_space = false;
        for (c, y) in x.iter().enumerate() {
            let (nstr, width) = y.unicode_truncate(sizes[c]);
            if need_space {
                w.write_all(b" ")?;
            }
            let num = (sizes[c] - width) / 2;
            if do_center {
                for _ in 0..num {
                    w.write_all(b" ")?;
                }
            }
            w.write_all(nstr.as_bytes())?;
            if do_center {
                let num2 = (sizes[c] - width) - num;
                for _ in 0..num2 {
                    w.write_all(b" ")?;
                }
            } else {
                for _ in width..sizes[c] {
                    w.write_all(b" ")?;
                }
            }
            need_space = true;
        }
        do_center = false;
        w.write_all(b"\n")?;
    }
    Ok(())
}

/// show the file in a specific rectangle
pub fn show2(file: &str, screen: &Rect, w: &mut Vec<String>, text: &TextFileMode) -> Result<usize> {
    let mut f = Reader::new(text);
    f.open(file)?;
    if f.is_empty() {
        return Ok(0);
    }
    let mut lines: Vec<StringLine> = Vec::new();
    let mut sizes: Vec<usize> = vec![0; f.header().len()];
    if f.has_header() {
        lines.push(f.header().clone());
    }
    if !f.is_done() {
        while lines.len() < screen.height {
            let mut s = StringLine::new();
            s.line = String::from_utf8_lossy(f.curr().line()).to_string();
            s.split(f.delim());
            lines.push(s);
            if f.getline()? {
                break;
            }
        }
    }
    for x in &lines {
        for (i, c) in x.iter().enumerate() {
            let width = UnicodeWidthStr::width(c);
            if sizes[i] < width {
                sizes[i] = width;
            }
        }
    }
    let mut total: usize = sizes.iter().sum();
    let target = screen.width - sizes.len();
    while total > target {
        dec_max(&mut sizes);
        total = sizes.iter().sum();
    }

    let mut do_center = f.has_header();
    w.clear();
    for x in &lines {
        let mut s = String::new();
        let mut need_space = false;
        for (c, y) in x.iter().enumerate() {
            let (nstr, width) = y.unicode_truncate(sizes[c]);
            if need_space {
                s.push(' ');
            }
            let num = (sizes[c] - width) / 2;
            if do_center {
                for _ in 0..num {
                    s.push(' ');
                }
            }
            s.push_str(nstr);
            if do_center {
                let num2 = (sizes[c] - width) - num;
                for _ in 0..num2 {
                    s.push(' ');
                }
            } else {
                for _ in width..sizes[c] {
                    s.push(' ');
                }
            }
            need_space = true;
        }
        do_center = false;
        w.push(s);
    }
    Ok(sizes.iter().sum::<usize>() + sizes.len())
}
