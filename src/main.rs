use std::{env, fs};

use crate::error::error_plain;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::translate::{smt_file_content, Symbols};

mod ast;
mod error;
mod lexer;
mod parser;
mod smt;
mod token;
mod translate;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 3 {
        let message = format!(
            "Wrong number of arguments.\nUsage: {} input_file output_file\n",
            args[0]
        );
        error_plain(message.as_str());
    }

    let input = &args[1];
    let output = &args[2];

    match fs::read_to_string(input) {
        Ok(file) => {
            let lexer = Lexer::new(file);
            let tokens = lexer.tokens().collect::<Vec<_>>();

            let mut parser = Parser::new(tokens);
            let (leader, follower, property) = parser.parse();

            fail_if_messages_do_not_match(&leader, &follower);
            fail_if_follower_is_transmission_recurrent(&follower);

            let mut symbols = Symbols::new(leader, follower, property);
            symbols.populate();

            let content = smt_file_content(&symbols);
            fs::write(output, content).expect("Wasn't able to write the specified file.");
        }
        Err(_) => {
            let msg = format!("`{}` doesn't exist.", input);
            error_plain(msg.as_str());
        }
    }
}

fn fail_if_messages_do_not_match(leader: &ast::Leader, follower: &ast::Follower) {
    for message in leader.m_in.iter() {
        if !follower.m_out.contains(&message) {
            let message = format!(
                "The leader's ingoing message `{}` is not one of the follower's outgoing messages.",
                message
            );
            error_plain(message.as_str());
        }
    }

    for message in follower.m_in.iter() {
        if !leader.m_out.contains(&message) {
            let message = format!(
                "The followers's ingoing message `{}` is not one of the leader's outgoing messages.",
                message
            );
            error_plain(message.as_str());
        }
    }
}

fn fail_if_follower_is_transmission_recurrent(follower: &ast::Follower) {
    if follower.is_transmission_recurrent() {
        error_plain("The follower is tranmission-recurrent.");
    }
}
