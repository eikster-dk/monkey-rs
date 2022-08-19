use std::io;

use crate::lexer::Lexer;


const PROMPT: &'static str = ">> ";

pub fn start<R :io::BufRead, W :io::Write>(mut read: R, mut writer :W) -> io::Result<()> {
    writer.write("This is the Monkey Programming language REPL\n".as_bytes())?;
    writer.flush()?;

    loop {
        writer.write(PROMPT.as_bytes())?;
        writer.flush()?;

        let mut s = String::new();
        read.read_line(&mut s)?;

        let lexer = Lexer::new(s.as_str());

        for token in lexer {
            writer.write(format!("{:?}\n", token).as_bytes())?;
            writer.flush()?;
        }
    }
}

