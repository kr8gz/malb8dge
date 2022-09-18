use ariadne::*;

use std::{fs, process};

use crate::constants::Pos;

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

pub struct Error<'a> {
    file: &'a str,
    msg: String,
    labels: Vec<(Pos, String)>,
    help: Option<String>,
    note: Option<String>,
}

impl<'a> Error<'a> {
    pub fn new(file: &'a str, msg: &str) -> Self {
        Self {
            file,
            msg: msg.into(),
            labels: Vec::new(),
            help: None,
            note: None,
        }
    }

    pub fn label(mut self, pos: Pos, msg: &str) -> Self {
        self.labels.push((pos, msg.into()));
        self
    }

    pub fn help(mut self, msg: &str) -> Self {
        self.help = Some(msg.into());
        self
    }

    pub fn note(mut self, msg: &str) -> Self {
        self.note = Some(msg.into());
        self
    }

    pub fn eprint(self) -> ! {
        let mut colgen = ColorGenerator::new(self.labels.len());

        let mut report = Report::build(
            ReportKind::Error,
            self.file,
            self.labels.iter().map(|label| label.0.start).min().unwrap_or(0)
        )
        .with_config(Config::default().with_cross_gap(true))
        .with_message(self.msg);

        for label in self.labels {
            report = report.with_label(
                Label::new((self.file, label.0))
                .with_message(label.1)
                .with_color(colgen.next())
            );
        }

        if let Some(help) = self.help {
            report = report.with_help(help);
        }

        if let Some(note) = self.note {
            report = report.with_note(note);
        }

        report
        .finish()
        .eprint((self.file, Source::from(fs::read_to_string(self.file).unwrap())))
        .unwrap();

        process::exit(1);
    }
}