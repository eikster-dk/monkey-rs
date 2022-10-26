use std::io;

use crate::{eval, lexer::Lexer, parser::Parser};

const PROMPT: &'static str = ">> ";

pub fn start<R: io::BufRead, W: io::Write>(mut read: R, mut writer: W) -> io::Result<()> {
    writer.write("This is the Monkey Programming language REPL\n".as_bytes())?;
    writer.flush()?;

    loop {
        writer.write(PROMPT.as_bytes())?;
        writer.flush()?;

        let mut s = String::new();
        read.read_line(&mut s)?;

        let lexer = Lexer::new(s.as_str());
        let mut parser = Parser::new(lexer);
        let result = parser.parse_program();

        match result {
            Ok(program) => {
                let object = eval::evaluate_program(program);
                writer.write(format!("{:?}\n", object.to_string()).as_bytes())?;
                writer.flush()?;
            }
            Err(errs) => {
                for e in errs {
                    writer.write(format!("{:?}\n", e).as_bytes())?;
                    writer.flush()?;
                }
            }
        }
    }
}
