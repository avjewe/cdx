use crate::prelude::*;
use cdx::prelude::*;
use cdx::tabs;

fn print_centered(txt: &str, width: usize) {
    if txt.len() > width {
    } else {
        let left = (width - txt.len()) / 2;
        let right = width - txt.len() - left;
        println!("{: <3$}{}{: <4$}", "", txt, "", left, right);
    }
}

pub fn main(argv: &[String], settings: &mut Settings) -> Result<()> {
    let prog = args::ProgSpec::new("Display files.", args::FileCount::Many);
    const A: [ArgSpec; 1] = [arg! {"end", "e", "",  "nothing"}];
    let (args, files) = args::parse(&prog, &A, argv, settings)?;

    let mut end = false;

    for x in args {
        if x.name == "end" {
            end = true;
        } else {
            unreachable!();
        }
    }
    if end {
        eprintln!();
    }
    if files.len() == 1 {
        let mut v = Vec::new();
        tabs::show2(&files[0], &tabs::Rect::from_screen(), &mut v)?;
        for x in v {
            println!("{}", x);
        }
    } else {
        let base_rect = tabs::Rect::from_screen();
        let mut lines_left = base_rect.height - (2 * files.len()) + 1;
        let mut files_left = files.len();
        for x in &files {
            let mut v = Vec::new();
            let r = tabs::Rect::new(base_rect.width, lines_left / files_left);
            let real = tabs::show2(x, &r, &mut v)?;
            print_centered(x, real);
            for x in &v {
                println!("{}", x);
            }
            lines_left -= v.len();
            files_left -= 1;
            if files_left > 0 {
                println!("{:-<1$}", "", real);
            }
        }
    }
    Ok(())
}
