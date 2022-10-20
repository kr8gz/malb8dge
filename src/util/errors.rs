use ariadne::*;
use regex::Regex;

use std::{env, fs, process};

use super::Pos;

struct ColorGenerator {
    hue: f64,
    n: usize,
}

impl ColorGenerator {
    fn new(n: usize) -> Self {
        Self { hue: 0.0, n }
    }

    fn next(&mut self) -> ariadne::Color {
        let x = (self.hue / 120.0).fract();

        let high = 1.0;
        let low = 0.6;
        let more = low + x;
        let less = low + 1.0 - x;

        let (r, g, b) = {
            if (0.0..60.0).contains(&self.hue) {
                (high, more, low)
            } else if (60.0..120.0).contains(&self.hue) {
                (less, high, low)
            } else if (120.0..180.0).contains(&self.hue) {
                (low, high, more)
            } else if (180.0..240.0).contains(&self.hue) {
                (low, less, high)
            } else if (240.0..300.0).contains(&self.hue) {
                (more, low, high)
            } else {
                (high, low, less)
            }
        };

        self.hue += 360.0 / self.n as f64;
        self.hue %= 360.0;

        ariadne::Color::RGB(
            (r * 255.0) as u8,
            (g * 255.0) as u8,
            (b * 255.0) as u8,
        )
    }
}

fn replace_color(text: String, color: Color) -> String {
    let re = Regex::new("#(?P<inner>[^#]*)#").unwrap();
    re.replace_all(&text, "$inner".fg(color).to_string()).to_string()
}

pub struct Error {
    msg: String,
    kind: ReportKind,

    labels: Vec<(Pos, String)>,
    help: Option<String>,
    note: Option<String>,
}

impl Error {
    pub fn err(msg: impl ToString) -> Self {
        Error::new(msg, ReportKind::Error)
    }

    fn new(msg: impl ToString, kind: ReportKind) -> Self {
        Self {
            msg: msg.to_string(),
            kind,

            labels: Vec::new(),
            help: None,
            note: None,
        }
    }

    pub fn label(mut self, pos: Pos, msg: impl ToString) -> Self {
        self.labels.push((pos, msg.to_string()));
        self
    }

    pub fn help(mut self, msg: impl ToString) -> Self {
        self.help = Some(msg.to_string());
        self
    }

    pub fn note(mut self, msg: impl ToString) -> Self {
        self.note = Some(msg.to_string());
        self
    }

    pub fn print(self) {
        let file = &env::args().nth(1).unwrap(); // if file arg is missing it will fail in main.rs
        let mut colgen = ColorGenerator::new(self.labels.len());

        let mut report = Report::build(
            self.kind,
            file,
            self.labels.iter().map(|(pos, _)| pos.start).min().unwrap_or(0)
        )
        .with_config(Config::default().with_cross_gap(true))
        .with_message(self.msg);

        if self.labels.len() == 1 {
            for (pos, msg) in self.labels {
                let color = match self.kind {
                    ReportKind::Warning => Color::Yellow,
                    _ => Color::RGB(255, 127, 127)
                };
                report = report.with_label(
                    Label::new((file, pos))
                        .with_message(replace_color(msg, color))
                        .with_color(color)
                );
            }
        } else {
            for ((pos, msg), n) in self.labels.into_iter().zip(1..) {
                let color = colgen.next();
                report = report.with_label(
                    Label::new((file, pos))
                        .with_message(format!("{}: {}", n.to_string().fg(color), replace_color(msg, color)))
                        .with_order(n)
                        .with_color(color)
                );
            }
        };

        if let Some(help) = self.help {
            report = report.with_help(help);
        }

        if let Some(note) = self.note {
            report = report.with_note(note);
        }

        report
            .finish()
            .eprint((file, Source::from(
                fs::read_to_string(file).unwrap().replace("\r\n", "\n")
            )))
            .unwrap();
    }

    pub fn eprint(self) -> ! {
        self.print();
        process::exit(1);
    }
}

pub fn simple(msg: String) -> ! {
    eprintln!("{} {msg}", "Error:".fg(Color::Red));
    process::exit(1)
}
