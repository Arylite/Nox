mod ast;
mod lexer;

fn main() {
    let source = r#"
        let x = 5;
        fn add(a, b) {
            return a + b;
        }
    "#;

    let tokens = lexer::lex_source(source, "main.nox");
    match tokens {
        Ok(tokens) => {
            for token in &tokens {
                println!("{:?}", token);
            }
        }
        Err(e) => {
            eprintln!("Lexing error: {}", e);
        }
    }
}
