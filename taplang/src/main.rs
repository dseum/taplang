mod ast;
mod lexer;
mod parser;

// mod typer;
// mod eval;
// use ariadne::{Color, Label, Report, ReportKind, Source};
use lexer::Token;
use logos::Logos;
use parser::Parser;
// use typer::Gamma;

fn main() {
    // let usage = "Run `cargo run --examples/sample.tap`";
    // let src = std::fs::read_to_string(std::env::args().nth(1).expect(usage)).expect(usage);
    let src = std::fs::read_to_string("examples/fat.tap").unwrap();

    let token_iter = Token::lexer(&src).spanned().map(|(tok, span)| match tok {
        Ok(tok) => (tok, span),
        Err(_) => (Token::Error, span),
    });

    // // Turn the token iterator into a stream that chumsky can use for things like backtracking
    // let token_stream = Stream::from_iter(token_iter.clone())
    //     // Tell chumsky to split the (Token, SimpleSpan) stream into its parts so that it can handle the spans for us
    //     // This involves giving chumsky an 'end of input' span: we just use a zero-width span at the end of the string
    //     .map((0..src.len()).into(), |(t, s): (_, _)| (t, s));
    // // Parse the token stream with our chumsky parser
    let toks = token_iter.collect::<Vec<_>>();
    println!("TOKENS: {:?}", toks);

    let sanitized_toks = toks
        .into_iter()
        .filter(|st| st.0 != Token::Whitespace)
        .collect::<Vec<_>>();

    let mut par = Parser::new(&src, &sanitized_toks);


    let start = std::time::Instant::now();
    let cmd = par.parse_command();
    println!("Parsed in {:?}", start.elapsed());

    match cmd {
        Ok(file_ast) => {
            if par.pos >= sanitized_toks.len() {
                println!("AST: {:?}", file_ast.1)
            } else {
                println!("parsing ended abruptly. exited early to recover fragment:\n{:?}", file_ast.1)
            }
            
        },
        Err(e) => e.msg(),
    }

    println!("posssss :         {}", par.pos);

    // println!("TOKENS: {:?}", token_stream);

    // match command_parser().parse(token_stream).into_result() {
    //     // If parsing was successful, attempt to evaluate the s-expression
    //     Ok(ast_file) => {
    //         // match sexpr.eval() {
    //         //     Ok(out) => println!("Result = {}", out),
    //         //     Err(err) => println!("Runtime error: {}", err),
    //         // },
    //         println!("output:\n");
    //         println!("{:?}", ast_file);

    //         // let typed_file: ast::Cmd<typer::DataType> = typer::type_cmd(ast_file, &mut Gamma::new());
    //         // println!("{:?}", typed_file);
    //     }
    //     // If parsing was unsuccessful, generate a nice user-friendly diagnostic with ariadne. You could also use
    //     // codespan, or whatever other diagnostic library you care about. You could even just display-print the errors
    //     // with Rust's built-in `Display` trait, but it's a little crude
    //     Err(_errs) => {
    //         // for err in errs {
    //         //     Report::build(ReportKind::Error, (), err.span().start)
    //         //         .with_code(3)
    //         //         .with_message(err.to_string())
    //         //         .with_label(
    //         //             Label::new(err.span().into_range())
    //         //                 .with_message(err.reason().to_string())
    //         //                 .with_color(Color::Red),
    //         //         )
    //         //         .finish()
    //         //         .eprint(Source::from(SRC))
    //         //         .unwrap();
    //         // }
    //         panic!("error");
    //     }
    // }
}
