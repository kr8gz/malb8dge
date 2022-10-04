use ariadne::*;

use std::{fs, process};

use crate::util::Pos;

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
        let more = 0.5 + x;
        let less = 0.5 + 1.0 - x;
        let low = 0.5;

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

pub struct Error {
    file: String,
    msg: String,
    labels: Vec<(Pos, String)>,
    help: Option<String>,
    note: Option<String>,
}

impl Error {
    pub fn new<T: Into<String>>(file: String, msg: T) -> Self {
        Self {
            file,
            msg: msg.into(),
            labels: Vec::new(),
            help: None,
            note: None,
        }
    }

    pub fn label<T: Into<String>>(mut self, pos: Pos, msg: T) -> Self {
        self.labels.push((pos, msg.into()));
        self
    }

    pub fn help<T: Into<String>>(mut self, msg: T) -> Self {
        self.help = Some(msg.into());
        self
    }

    pub fn note<T: Into<String>>(mut self, msg: T) -> Self {
        self.note = Some(msg.into());
        self
    }

    pub fn eprint(self) -> ! {
        let mut colgen = ColorGenerator::new(self.labels.len());

        let mut report = Report::build(
            ReportKind::Error,
            &self.file,
            self.labels.iter().map(|(pos, _)| pos.start).min().unwrap_or(0)
        )
        .with_config(Config::default().with_cross_gap(true))
        .with_message(self.msg);

        if self.labels.len() == 1 {
            for (pos, msg) in self.labels {
                report = report.with_label(
                    Label::new((&self.file, pos))
                        .with_message(msg)
                        .with_color(colgen.next())
                );
            }
        } else {
            for ((pos, msg), n) in self.labels.into_iter().zip(1..) {
                let color = colgen.next();
                report = report.with_label(
                    Label::new((&self.file, pos))
                        .with_message(format!("{}: {}", n.to_string().fg(color), msg))
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
            .eprint((&self.file, Source::from(
                fs::read_to_string(&self.file).unwrap().replace("\r\n", "\n")
            )))
            .unwrap();

        process::exit(1);
    }
}