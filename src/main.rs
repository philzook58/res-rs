use tptp::TPTPIterator;
use tptp::visitor::Visitor;

struct MyVisitor;
impl<'a> Visitor<'a> for MyVisitor {}

use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

use clap::Parser;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
   filename: String,

}

fn main() {
    let args = Args::parse();
    let f = File::open(args.filename).expect("file does not exist");
    let mut buf_reader = BufReader::new(f);
    let mut contents = String::new();
    buf_reader.read_to_string(&mut contents).expect("bufreader failed");

    let mut visitor = MyVisitor;
    let mut parser = TPTPIterator::<()>::new(&contents.as_bytes());
    for result in &mut parser {
        let input = result.expect("syntax error");
        println!("{}", &input);
        visitor.visit_tptp_input(&input);
    }
    assert!(parser.remaining.is_empty());
}
