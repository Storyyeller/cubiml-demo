#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]

mod ast;
mod codegen;
mod core;
mod grammar;
mod js;
mod reachability;
mod typeck;
mod utils;

use wasm_bindgen::prelude::*;

use std::fmt::Display;
use std::mem;

use lalrpop_util::ParseError;

use self::codegen::ModuleBuilder;
use self::grammar::ScriptParser;
use self::typeck::TypeckState;

fn format_parse_error<T: Display>(e: ParseError<usize, T, &'static str>) -> String {
    match e {
        ParseError::InvalidToken { location } => format!("Invalid token at position {}", location),
        ParseError::UnrecognizedEOF { expected, .. } => {
            format!("Unexpected end of input.\nExpected tokens: [{}]", expected.join(", "))
        }
        ParseError::UnrecognizedToken { token, expected } => format!(
            "Unexpected token {} at position {}.\nExpected tokens: [{}]",
            token.1,
            token.0,
            expected.join(", ")
        ),
        ParseError::ExtraToken { token } => format!("Unexpected extra token {}", token.1),
        ParseError::User { error: msg } => msg.to_string(),
    }
}

#[wasm_bindgen]
pub struct State {
    parser: ScriptParser,
    checker: TypeckState,
    compiler: ModuleBuilder,

    out: Option<String>,
    err: Option<String>,
}
#[wasm_bindgen]
impl State {
    pub fn new() -> Self {
        State {
            parser: ScriptParser::new(),
            checker: TypeckState::new(),
            compiler: ModuleBuilder::new(),

            out: None,
            err: None,
        }
    }

    fn process_sub(&mut self, source: &str) -> Result<String, String> {
        let ast = self.parser.parse(source).map_err(format_parse_error)?;
        let _t = self.checker.check_script(&ast).map_err(|e| format!("{}", e))?;
        let js_ast = self.compiler.compile_script(&ast);
        Ok(js_ast.to_source())
    }

    pub fn process(&mut self, source: &str) -> bool {
        let res = self.process_sub(source);
        match res {
            Ok(s) => {
                self.out = Some(s);
                true
            }
            Err(e) => {
                self.err = Some(e);
                false
            }
        }
    }

    pub fn get_output(&mut self) -> Option<String> {
        self.out.take()
    }
    pub fn get_err(&mut self) -> Option<String> {
        self.err.take()
    }

    pub fn reset(&mut self) {
        mem::swap(&mut self.checker, &mut TypeckState::new());
        mem::swap(&mut self.compiler, &mut ModuleBuilder::new());
    }
}
