#![feature(core)] // Silence a bunch of unstable warnings

extern crate duckyc_syntax;

use duckyc_syntax::lexer::{self, Token};
use duckyc_syntax::lexer::Token::*;
use duckyc_syntax::parser::State;
use duckyc_syntax::intern::Atom;

// In dpp, we want to have directives. Directives control the way that code executes
// I'm mimicing Rust here, and want to use #[] for directives. So, for example,
// #[if x]
//   /* some tokens here */
// #[else]
//   /* some tokens here */
// #[endif]
// would check if x was defined, and if it was include the following block.

// You can also write #[define x y] to define x to y, and just #x to inline the value of x at that location.

#[derive(Clone, Debug)]
enum Hunk {
    // Just a direct token dump -> no preprocessing to be done.
    Tok(Token),

    // Notably absent:
    // Include(Atom) -> IO in Rust is super unstable right now, so I'm avoiding dealing with it
    // Macro(Atom, Vec<Atom>, Vec<Hunk>) -> Macros which accept arguments (I may add this to the parser later)

    Define(Atom, Vec<Hunk>), // #[define a ...], binds a to the ...
    Expand(Atom), // #a => maps to whatever a is bound to
    If(Atom, Vec<Hunk>, Vec<Hunk>), // #[if x] ... #[else] ... #[endif]

    // These hunks are technically invalid, and can't be used for anything
    // They are used as values to be passed around while parsing the structure
    Else,
    EndIf
}

// Some predicates which are used to determine when to stop parsing directives
fn until_rbracket_pred(hunk: &Hunk) -> bool {
    match *hunk {
        Hunk::Tok(RBRACKET) => true,
        _ => false
    }
}

fn until_endif_pred(hunk: &Hunk) -> bool {
    match *hunk {
        Hunk::EndIf => true,
        _ => false
    }
}

fn until_else_endif_pred(hunk: &Hunk) -> bool {
    match *hunk {
        Hunk::EndIf => true,
        Hunk::Else => true,
        _ => false
    }
}

fn never_pred(_: &Hunk) -> bool { false }

fn parse_directive(state: &mut State, ty: &Atom, body: Vec<Hunk>) -> Result<Hunk, String> {
    match ty.as_slice() {
        "if" => {
            if let Hunk::Tok(IDENT(ref condition)) = body[0] {
                let mut cons = try!(parse_groups(state, until_else_endif_pred));
                match cons.pop() {
                    Some(Hunk::Else) => {
                        let mut alt = try!(parse_groups(state, until_endif_pred));
                        if let Some(Hunk::EndIf) = alt.pop() {
                            Ok(Hunk::If(condition.clone(), cons, alt))
                        } else {
                            Err(format!("Must terminate else block with #[endif]"))
                        }
                    }
                    Some(Hunk::EndIf) => {
                        Ok(Hunk::If(condition.clone(), cons, vec![]))
                    }
                    _ => {
                        Err(format!("Unexpected end of file while parsing if"))
                    }
                }
            } else {
                Err(format!("Expected condition for if"))
            }
        }
        "else" => {
            if body.is_empty() {
                Ok(Hunk::Else)
            } else {
                Err(format!("Else directive takes no paramaters"))
            }
        }
        "endif" => {
            if body.is_empty() {
                Ok(Hunk::EndIf)
            } else {
                Err(format!("EndIf directive takes no paramaters"))
            }
        }
        "define" => {
            if let Hunk::Tok(IDENT(ref atom)) = body[0] {
                Ok(Hunk::Define(atom.clone(), body[1..].iter().cloned().collect()))
            } else {
                Err(format!("Can only #[define] idents!"))
            }
        }
        _ => Err(format!("Unrecognized directive: {:?}", ty))
    }
}

fn parse_group(state: &mut State) -> Result<Hunk, String> {
    match state.eat() {
        Some(&POUND) => {
            match state.eat() {
                Some(&LBRACKET) => {
                    // Require the next token to be an ident
                    if let Some(&IDENT(ref atom)) = state.eat() {
                        let mut hunks = try!(parse_groups(state, until_rbracket_pred));

                        if let Some(Hunk::Tok(RBRACKET)) = hunks.pop() {
                            parse_directive(state, atom, hunks)
                        } else {
                            Err(format!("Expected ] after directive"))
                        }
                    } else {
                        Err(format!("Expected IDENT as first token in directive"))
                    }
                },
                Some(&IDENT(ref atom)) => {
                    Ok(Hunk::Expand(atom.clone()))
                },
                _ => Err(format!("Expected [ or IDENT after #"))
            }
        }
        Some(tok) => {
            Ok(Hunk::Tok(tok.clone()))
        }
        None => unreachable!()
    }
}

fn parse_groups(state: &mut State, done_pred: fn(&Hunk) -> bool) -> Result<Vec<Hunk>, String> {
    let mut hunks = Vec::new();
    loop {
        if let Some(_) = state.peek() {
            let hunk = try!(parse_group(state));
            let done = done_pred(&hunk);

            hunks.push(hunk);
            if done { break }
        } else { break }
    }

    Ok(hunks)
}

fn main() {
    let test_code = r#"
#[define compose_id]
let id = fn (x) { x };
#[define number 5]

let id2 = #[if compose_id] id(id) #[else] id #[endif];

let a = id2(#number);
#[define number 10]
let b = id2(#number); "#;

    let lexed = lexer::lex(test_code).unwrap();
    let groups = parse_groups(&mut State::new(lexed.as_slice()), never_pred).unwrap();

    for group in groups.iter() {
        println!("{:?}", group);
    }
}
