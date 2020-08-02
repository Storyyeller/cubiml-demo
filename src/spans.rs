use std::collections::{HashMap, HashSet};
use std::error;
use std::fmt;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Span(usize);

pub type Spanned<T> = (T, Span);

#[derive(Debug, Default)]
pub struct SpanManager {
    sources: Vec<String>,
    spans: Vec<(usize, usize, usize)>,
}
impl SpanManager {
    pub fn add_source(&mut self, source: String) -> SpanMaker {
        let i = self.sources.len();
        self.sources.push(source);
        SpanMaker {
            parent: self,
            source_ind: i,
            pool: Default::default(),
        }
    }

    pub fn print(&self, span: Span) -> String {
        let (source_ind, l, r) = self.spans[span.0];
        let source = &self.sources[source_ind];

        let mut out = String::new();

        assert!(l <= r);
        let (before, source) = source.split_at(l);

        // The matched span may contain newlines, so cut it off at the first newline if applicable
        let tok = source.split_at(r - l).0.split('\n').next().unwrap();
        let after = source.split_at(tok.len()).1;
        // let (source, after) = source.split_at(r);

        let mut b_iter = before.rsplit('\n');
        let line_before = b_iter.next().unwrap();
        let mut a_iter = after.split('\n');
        let line_after = a_iter.next().unwrap();

        // Lines before the span
        if let Some(line) = b_iter.next() {
            if let Some(line) = b_iter.next() {
                out += line;
                out += "\n";
            }
            out += line;
            out += "\n";
        }

        // Line of the span
        out += line_before;
        out += tok;
        out += line_after;
        out += "\n";

        // highlight line
        out += &" ".repeat(line_before.len());
        out += "^";
        out += &"~".repeat(std::cmp::max(1, tok.len()) - 1);
        out += &" ".repeat(line_after.len());
        out += "\n";

        // Lines after the span
        for _ in 0..2 {
            if let Some(line) = a_iter.next() {
                out += line;
                out += "\n";
            }
        }

        out
    }

    fn new_span(&mut self, source_ind: usize, l: usize, r: usize) -> Span {
        let i = self.spans.len();
        self.spans.push((source_ind, l, r));
        Span(i)
    }
}

#[derive(Debug)]
pub struct SpanMaker<'a> {
    parent: &'a mut SpanManager,
    source_ind: usize,
    pool: HashMap<(usize, usize), Span>,
}
impl<'a> SpanMaker<'a> {
    pub fn span(&mut self, l: usize, r: usize) -> Span {
        // Make the borrow checker happy
        let source_ind = self.source_ind;
        let parent = &mut self.parent;

        *self.pool.entry((l, r)).or_insert_with(|| parent.new_span(source_ind, l, r))
    }
}

#[derive(Debug)]
pub struct SpannedError {
    pairs: Vec<(String, Span)>,
}

impl SpannedError {
    pub fn new1(s1: impl Into<String>, s2: Span) -> Self {
        let p1 = (s1.into(), s2);
        SpannedError { pairs: vec![p1] }
    }

    pub fn new2(s1: impl Into<String>, s2: Span, s3: impl Into<String>, s4: Span) -> Self {
        let p1 = (s1.into(), s2);
        let p2 = (s3.into(), s4);
        SpannedError { pairs: vec![p1, p2] }
    }

    pub fn print(&self, sm: &SpanManager) -> String {
        let mut out = String::new();
        for (msg, span) in self.pairs.iter() {
            out += &msg;
            out += "\n";
            out += &sm.print(*span);
        }
        out
    }
}
impl fmt::Display for SpannedError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Ok(())
    }
}
impl error::Error for SpannedError {}
