use std::io;

use crate::{lexer::Lexer, parser::Parser};

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
        let program = parser.parse_program();

        if parser.errors.len() > 0 {
            for e in parser.errors {
                writer.write(format!("{:?}\n", e).as_bytes())?;
                writer.flush()?;
            }
        } else {
            writer.write(format!("{:?}\n", program.to_string()).as_bytes())?;
            writer.flush()?;
        }
    }
}
