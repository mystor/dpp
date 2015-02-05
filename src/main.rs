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

    Define(Atom, Vec<Hunk>), // #[define a ...], binds a to the ...
    UnDefine(Atom), // #[undefine a], unbinds a
    Macro(Atom, Vec<Atom>, Vec<Hunk>), // #[macro(a, b, c)] ... #[endmacro]
    Expand(Atom), // #a => maps to whatever a is bound to
    Invoke(Atom, Vec<Vec<Hunk>>), // #a(a, b, c) => maps to the macro a, called with a, b, c
    If(Atom, Vec<Hunk>, Vec<Hunk>), // #[if x] ... #[else] ... #[endif]

    // These hunks are technically invalid, and can't be used for anything
    // They are used as values to be passed around while parsing the structure
    Else,
    EndIf,
    EndMacro
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

fn until_endmacro_pred(hunk: &Hunk) -> bool {
    match *hunk {
        Hunk::EndMacro => true,
        _ => false
    }
}

fn until_comma_rparen_pred(hunk: &Hunk) -> bool {
    // TODO(michael): This function currently has the problem of stopping on RPARENs or commas inside the body of
    // a macro argument, as the lexer doesn't understand/respect () matching. It's too big of a problem to tackle
    // right now, but needs to be tackled at some point for dpp to be useful
    match *hunk {
        Hunk::Tok(COMMA) => true,
        Hunk::Tok(RPAREN) => true,
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
        "undefine" => {
            if let Hunk::Tok(IDENT(ref atom)) = body[0] {
                if body.len() == 1 {
                    Ok(Hunk::UnDefine(atom.clone()))
                } else {
                    Err(format!("Can only #[undefine] one ident!"))
                }
            } else {
                Err(format!("Can only #[undefine] idents!"))
            }
        }
        "macro" => {
            if let Hunk::Tok(IDENT(ref name)) = body[0] {
                match body[1] { Hunk::Tok(LPAREN) => (), _ => return Err(format!("Expected ( after macro")) }
                let mut args = Vec::new();

                let mut idx = 2;
                while idx < body.len() - 1 {
                    if let Hunk::Tok(IDENT(ref atom)) = body[idx] { // The variable's name
                        args.push(atom.clone());
                        match body[idx+1] {
                            Hunk::Tok(COMMA) => { idx += 2; continue }
                            Hunk::Tok(RPAREN) => {
                                if idx+2 == body.len() { // The ) must be the last character in the body
                                    let mut body = try!(parse_groups(state, until_endmacro_pred));
                                    return match body.pop() {
                                        Some(Hunk::EndMacro) => {
                                            Ok(Hunk::Macro(name.clone(), args, body))
                                        }
                                        _ => {
                                            Err(format!("Unexpected end of file while parsing macro body"))
                                        }
                                    }
                                } else {
                                    return Err(format!("In macro definition, no elements allowed after )"));
                                }
                            }
                            _ => return Err(format!("Unexpected token in macro definition"))
                        }
                    } else {
                        return Err(format!("Expected argument name in macro definition!"));
                    }
                }
                Err(format!("Unexpected end of macro definition directive"))
            } else {
                Err(format!("Expected macro name after 'macro' in macro definition"))
            }
        }
        "endmacro" => {
            if body.is_empty() {
                Ok(Hunk::EndMacro)
            } else {
                Err(format!("EndMacro directive takes no parameters"))
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
                    match state.peek() {
                        Some(&LPAREN) => {
                            state.eat(); // eat the (
                            let mut args = Vec::new();
                            loop {
                                let mut arg = try!(parse_groups(state, until_comma_rparen_pred));
                                match arg.pop() {
                                    Some(Hunk::Tok(COMMA)) => {
                                        args.push(arg);
                                    }
                                    Some(Hunk::Tok(RPAREN)) => {
                                        args.push(arg);
                                        break;
                                    }
                                    _ => return Err(format!("Unexpected end of file while parsing macro invocation"))
                                }
                            }
                            Ok(Hunk::Invoke(atom.clone(), args))
                        }
                        _ => {
                            Ok(Hunk::Expand(atom.clone()))
                        }
                    }
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

#[undefine compose_id]

let id2 = #[if compose_id] id(id) #[else] id #[endif];

let a = id2(#number);
#[define number 10]
let b = id2(#number);

#[macro foo(bar, baz, quxx)]
let #bar = #baz * #quxx;
#[endmacro]

#foo(apples, b * 3, #number)

"#;

    let lexed = lexer::lex(test_code).unwrap();
    let groups = parse_groups(&mut State::new(lexed.as_slice()), never_pred).unwrap();

    for group in groups.iter() {
        println!("{:?}", group);
    }
}
