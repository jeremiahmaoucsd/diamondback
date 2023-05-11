use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::Atom::*;
use sexp::*;

use im::HashMap;

//parser enums (Expr) ---------------------------------------------------

#[derive(Debug)]
enum Expr {
    Number(i64),
    Boolean(bool),
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Set(String, Box<Expr>),
    Block(Vec<Expr>),
}


#[derive(Debug)]
enum Op1 {
    Add1,
    Sub1,
    IsNum, 
    IsBool, 
}

#[derive(Debug)]
enum Op2 {
    Plus,
    Minus,
    Times,
    Equal,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}



//Parsing starts here --------------------------------------------------------------------------------------------------------------------------------------------------------
fn parse_expr(s: &Sexp) -> Expr {
  let min_num = -4611686018427387904;
  let max_num = 4611686018427387903;

  match s {
      Sexp::Atom(I(n)) if i64::try_from(*n).unwrap() >= min_num && i64::try_from(*n).unwrap() <= max_num => Expr::Number(i64::try_from(*n).unwrap()), // handle literal overflow here
      Sexp::Atom(S(name)) if name == "true" => Expr::Boolean(true),
      Sexp::Atom(S(name)) if name == "false" => Expr::Boolean(false),

      Sexp::Atom(S(id)) => Expr::Id(id.to_string()),
      Sexp::List(vec) => {
          match &vec[..] {
              [Sexp::Atom(S(op)), e] if op == "add1" => Expr::UnOp(Op1::Add1, Box::new(parse_expr(e))),
              [Sexp::Atom(S(op)), e] if op == "sub1" => Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e))),
              [Sexp::Atom(S(op)), e] if op == "isnum" => Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e))),
              [Sexp::Atom(S(op)), e] if op == "isbool" => Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e))),
              [Sexp::Atom(S(op)), e1, e2] if op == "+" => Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)),Box::new(parse_expr(e2))),
              [Sexp::Atom(S(op)), e1, e2] if op == "-" => Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1)),Box::new(parse_expr(e2))),
              [Sexp::Atom(S(op)), e1, e2] if op == "*" => Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)),Box::new(parse_expr(e2))),
              
              [Sexp::Atom(S(op)), Sexp::List(bindings), body] if op == "let" => {
                  if bindings.len() == 0{
                      panic!("Invalid");
                  }
                  Expr::Let(bindings.iter().map(|x| parse_bind(x)).collect::<Vec<(String, Expr)>>(),Box::new(parse_expr(body)))
              },
              [Sexp::Atom(S(op)), e1, e2] if op == "<" => Expr::BinOp(Op2::Less, Box::new(parse_expr(e1)),Box::new(parse_expr(e2))),
              [Sexp::Atom(S(op)), e1, e2] if op == ">" => Expr::BinOp(Op2::Greater, Box::new(parse_expr(e1)),Box::new(parse_expr(e2))),
              [Sexp::Atom(S(op)), e1, e2] if op == ">=" => Expr::BinOp(Op2::GreaterEqual, Box::new(parse_expr(e1)),Box::new(parse_expr(e2))),
              [Sexp::Atom(S(op)), e1, e2] if op == "<=" => Expr::BinOp(Op2::LessEqual, Box::new(parse_expr(e1)),Box::new(parse_expr(e2))),
              [Sexp::Atom(S(op)), e1, e2] if op == "=" => Expr::BinOp(Op2::Equal, Box::new(parse_expr(e1)),Box::new(parse_expr(e2))),

              [Sexp::Atom(S(op)), Sexp::Atom(S(id)), e] if op == "set!" => Expr::Set(id.to_string(), Box::new(parse_expr(e))),

              [Sexp::Atom(S(op)), e1, e2, e3] if op == "if" => Expr::If(Box::new(parse_expr(e1)),Box::new(parse_expr(e2)),Box::new(parse_expr(e3))),
              [Sexp::Atom(S(op)), exprs @ ..] if op == "block" && exprs.len() > 0 => Expr::Block(exprs.into_iter().map(parse_expr).collect::<Vec<Expr>>()),
              [Sexp::Atom(S(op)), e] if op == "loop" => Expr::Loop(Box::new(parse_expr(e))),
              [Sexp::Atom(S(op)), e] if op == "break" => Expr::Break(Box::new(parse_expr(e))),
              _ => panic!("Invalid"),
          }
      },
      _ => panic!("Invalid"),
  }
}

fn parse_bind(s: &Sexp) -> (String, Expr) {
  match s {
      Sexp::List(vec) => {
          match &vec[..] {
              [Sexp::Atom(S(id)), e] => (id.to_string(), parse_expr(e)),
              _ => panic!("Invalid"),
          }
      },
      _ => panic!("Invalid"),
  }
}


//Compiler (Instr) enums ---------------------------------------------------------------------------------------------------------------------------------------
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum Instr {

    IMov(Val, Val),
    ICmove(Val, Val),
    ICmovne(Val, Val),
    ICmovl(Val, Val),
    ICmovle(Val, Val),
    ICmovg(Val, Val),
    ICmovge(Val, Val),

    // Arithmetic
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    IDiv(Val),
    ISar(Val, Val), // shift arithmetic right

    // Logic
    IXor(Val, Val),

    // working with jumps
    ICmp(Val, Val),
    ITest(Val, Val),
    
    ISetLabel(LabelVal),
    IJmp(LabelVal),
    IJnz(LabelVal),
    IJz(LabelVal),
    IJe(LabelVal),
    IJne(LabelVal),
    IJo(LabelVal),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum Val {
    Reg(Reg),
    Imm(i64),
    RegOffset(Reg, i64),
    ErrorCode(ErrorCode),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum LabelVal {
    Label(Label, i64),
    ERROR,
    NOT_SET,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum Reg {
    RAX,
    RSP,
    RDI,
    RBX,
    R10, // for throw error
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum ErrorCode{
    OVERFLOW, // 3
    INVALID_ARG, // 7
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum Label {
    IFEND,
    IFELSE,
    LOOP,
    LOOPEND,
}


//Compiling starts here ------------------------------------------------------------------------------------------------------------------------------------------------------
/* 
fn print_hash(map: &mut HashMap<String, i64>) {
    let print = map.clone();
    for (key, value) in print.iter() {
        println!("{} / {}", key, value);
    }
}*/

fn int_post_inc(val: &mut i64) -> i64 {
    let current = *val;
    *val += 1;
    current
}

fn compile_to_instrs(e: &Expr, si: i64, env: &HashMap<String, i64>, br: &LabelVal, l: &mut i64)  -> Vec<Instr> {
    //common expressions
    let check_overflow_arith = vec!(Instr::IMov(Val::Reg(Reg::R10), Val::ErrorCode(ErrorCode::OVERFLOW)), Instr::IJo(LabelVal::ERROR));

    let if_bool_err = vec!(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)),
                            Instr::IMov(Val::Reg(Reg::R10), Val::ErrorCode(ErrorCode::INVALID_ARG)), 
                            Instr::IJnz(LabelVal::ERROR)
                        );

    match e{
        Expr::Number(n) => vec!(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n << 1))),
        Expr::Boolean(true) => vec!(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3))),
        Expr::Boolean(false) => vec!(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))),
        Expr::Id(s) if s == "input" => vec!(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI))),
        Expr::Id(s) => {
            if env.contains_key(s) == false {
                panic! ("Unbound variable identifier {}", s);
            }
            let offset = *env.get(s).unwrap();
            vec!(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset)))
        },
        Expr::Let(bindings, body) => {
            if bindings_repeat(bindings){
                panic! ("Duplicate binding");
            }
            let mut instrs = Vec::<Instr>::new();
            let mut si_mut = si;
            let mut nenv = env.clone();
            for (name, val) in bindings.iter(){
                if is_invalid_id(name) {
                    panic!("Invalid variable identifier: {} (is a keyword)", name); //maybe may need to change error to "keyword"
                }
                
                let mut val_is = compile_to_instrs(&val, si_mut, &nenv, br, l);
                let offset = si_mut * 8;
                nenv = nenv.update(name.to_string(), offset);

                instrs.append(&mut val_is);
                instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX)));
                si_mut += 1;
            }
            let mut body_is = compile_to_instrs(body, si_mut, &nenv, br, l);
            instrs.append(&mut body_is);
            instrs
        },
        Expr::UnOp(Op1::Add1, subexpr) => {
            let mut e = compile_to_instrs(subexpr, si, env, br, l);

            //check for invalid type
            e.append(&mut if_bool_err.clone());

            e.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1 << 1)));
            e.append(&mut check_overflow_arith.clone());
            e
        },

        Expr::UnOp(Op1::Sub1, subexpr) => {
            let mut e = compile_to_instrs(subexpr, si, env, br, l);

            //check for invalid type
            e.append(&mut if_bool_err.clone());

            e.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(1 << 1)));
            e.append(&mut check_overflow_arith.clone());
            e
        },

        Expr::UnOp(Op1::IsNum, subexpr) => {
            let mut e = compile_to_instrs(subexpr, si, env, br, l);

            e.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1))); // if rax is a number, this will return 0

            e.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))); // bool::false
            e.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
            e.push(Instr::ICmove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));

            e
        }

        Expr::UnOp(Op1::IsBool, subexpr) => {
            let mut e = compile_to_instrs(subexpr, si, env, br, l);

            e.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1))); // if rax is a number, this will return 0

            e.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3))); // bool::true
            e.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(1)));
            e.push(Instr::ICmove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));

            e
        }

        Expr::BinOp(Op2::Plus, e1, e2) => {
            let mut e1_instr = compile_to_instrs(e1, si, env, br, l);
            let mut e2_instr = compile_to_instrs(e2, si+1, env, br, l);
            let stack_offset = si * 8;

            //check for invalid type
            e1_instr.append(&mut if_bool_err.clone());

            e1_instr.push(Instr::IMov(Val::RegOffset(Reg::RSP,stack_offset), Val::Reg(Reg::RAX)));
            e1_instr.append(&mut e2_instr); 

            //check for invalid type
            e1_instr.append(&mut if_bool_err.clone());

            e1_instr.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
            
            // check for overflow
            e1_instr.append(&mut check_overflow_arith.clone());

            e1_instr
        },

        Expr::BinOp(Op2::Minus, e1, e2) => {
            let mut e1_instr = compile_to_instrs(e1, si, env, br, l);
            let mut e2_instr = compile_to_instrs(e2, si+1, env, br, l);
            let stack_offset = si * 8;

            //check for invalid type
            e1_instr.append(&mut if_bool_err.clone());


            e1_instr.push(Instr::IMov(Val::RegOffset(Reg::RSP,stack_offset), Val::Reg(Reg::RAX)));
            e1_instr.append(&mut e2_instr); 

            //check for invalid type
            e1_instr.append(&mut if_bool_err.clone());


            e1_instr.push(Instr::ISub(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
            e1_instr.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
            
            // check for overflow
            e1_instr.append(&mut check_overflow_arith.clone());

            e1_instr
        },

        Expr::BinOp(Op2::Times, e1, e2) => {
            let mut e1_instr = compile_to_instrs(e1, si, env, br, l);
            let mut e2_instr = compile_to_instrs(e2, si+1, env, br, l);
            let stack_offset = si * 8;

            //check for invalid type
            e1_instr.append(&mut if_bool_err.clone());

            e1_instr.push(Instr::IMov(Val::RegOffset(Reg::RSP,stack_offset), Val::Reg(Reg::RAX)));
            e1_instr.append(&mut e2_instr); 

            //check for invalid type
            e1_instr.append(&mut if_bool_err.clone());

            e1_instr.push(Instr::ISar(Val::Reg(Reg::RAX), Val::Imm(1))); //due to the way int values are stored
            e1_instr.push(Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
            
            // check for overflow
            e1_instr.append(&mut check_overflow_arith.clone());

            e1_instr
        },


/*              {e1_instrs}
                mov [rsp - {offset}], rax
                {e2_instrs}
                mov rbx, rax
                xor rbx, [rsp - {offset}]
                test rbx, 1
                mov rdi, 7
                jne throw_error
                cmp rax, [rsp - {offset}]
                mov rbx, 3
                mov rax, 1
                cmove rax, rbx */


        Expr::BinOp(Op2::Equal, e1, e2) => {
            let mut e1_instr = compile_to_instrs(e1, si, env, br, l);
            let mut e2_instr = compile_to_instrs(e2, si+1, env, br, l);
            let stack_offset = si * 8;

            e1_instr.push(Instr::IMov(Val::RegOffset(Reg::RSP,stack_offset), Val::Reg(Reg::RAX)));
            e1_instr.append(&mut e2_instr); 
            e1_instr.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            e1_instr.push(Instr::IXor(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP,stack_offset)));
            e1_instr.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1)));
            e1_instr.push(Instr::IMov(Val::Reg(Reg::R10), Val::ErrorCode(ErrorCode::INVALID_ARG)));
            e1_instr.push(Instr::IJne(LabelVal::ERROR));

            e1_instr.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP,stack_offset)));
            e1_instr.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
            e1_instr.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            e1_instr.push(Instr::ICmove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
            e1_instr
        },

        Expr::BinOp(Op2::Greater, e1, e2) => {
            let mut e1_instr = compile_to_instrs(e1, si, env, br, l);
            let mut e2_instr = compile_to_instrs(e2, si+1, env, br, l);
            let stack_offset = si * 8;

            //check for invalid type
            e1_instr.append(&mut if_bool_err.clone());

            e1_instr.push(Instr::IMov(Val::RegOffset(Reg::RSP,stack_offset), Val::Reg(Reg::RAX))); //e1 moved to stack
            e1_instr.append(&mut e2_instr); 

            //check for invalid type
            e1_instr.append(&mut if_bool_err.clone());

            e1_instr.push(Instr::ICmp(Val::RegOffset(Reg::RSP,stack_offset), Val::Reg(Reg::RAX)));
            e1_instr.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
            e1_instr.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            e1_instr.push(Instr::ICmovg(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));

            e1_instr
        },

        Expr::BinOp(Op2::GreaterEqual, e1, e2) => {
            let mut e1_instr = compile_to_instrs(e1, si, env, br, l);
            let mut e2_instr = compile_to_instrs(e2, si+1, env, br, l);
            let stack_offset = si * 8;

            //check for invalid type
            e1_instr.append(&mut if_bool_err.clone());

            e1_instr.push(Instr::IMov(Val::RegOffset(Reg::RSP,stack_offset), Val::Reg(Reg::RAX))); //e1 moved to stack
            e1_instr.append(&mut e2_instr); 

            //check for invalid type
            e1_instr.append(&mut if_bool_err.clone());

            e1_instr.push(Instr::ICmp(Val::RegOffset(Reg::RSP,stack_offset), Val::Reg(Reg::RAX)));
            e1_instr.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
            e1_instr.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            e1_instr.push(Instr::ICmovge(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));

            e1_instr
        },

        Expr::BinOp(Op2::Less, e1, e2) => {
            let mut e1_instr = compile_to_instrs(e1, si, env, br, l);
            let mut e2_instr = compile_to_instrs(e2, si+1, env, br, l);
            let stack_offset = si * 8;

            //check for invalid type
            e1_instr.append(&mut if_bool_err.clone());

            e1_instr.push(Instr::IMov(Val::RegOffset(Reg::RSP,stack_offset), Val::Reg(Reg::RAX))); //e1 moved to stack
            e1_instr.append(&mut e2_instr); 

            //check for invalid type
            e1_instr.append(&mut if_bool_err.clone());

            e1_instr.push(Instr::ICmp(Val::RegOffset(Reg::RSP,stack_offset), Val::Reg(Reg::RAX)));
            e1_instr.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
            e1_instr.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            e1_instr.push(Instr::ICmovl(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));

            e1_instr
        },

        Expr::BinOp(Op2::LessEqual, e1, e2) => {
            let mut e1_instr = compile_to_instrs(e1, si, env, br, l);
            let mut e2_instr = compile_to_instrs(e2, si+1, env, br, l);
            let stack_offset = si * 8;

            //check for invalid type
            e1_instr.append(&mut if_bool_err.clone());

            e1_instr.push(Instr::IMov(Val::RegOffset(Reg::RSP,stack_offset), Val::Reg(Reg::RAX))); //e1 moved to stack
            e1_instr.append(&mut e2_instr); 

            //check for invalid type
            e1_instr.append(&mut if_bool_err.clone());

            e1_instr.push(Instr::ICmp(Val::RegOffset(Reg::RSP,stack_offset), Val::Reg(Reg::RAX)));
            e1_instr.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
            e1_instr.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            e1_instr.push(Instr::ICmovle(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));

            e1_instr
        },
        Expr::If(e1, e2, e3) => {
            let end_label = LabelVal::Label(Label::IFEND, int_post_inc(l));
            let else_label = LabelVal::Label(Label::IFELSE, int_post_inc(l));
            let mut cond_is = compile_to_instrs(e1, si, env, br, l);
            let mut thn_is = compile_to_instrs(e2, si, env, br, l);
            let mut els_is = compile_to_instrs(e3, si, env, br, l);

            cond_is.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(1)));
            cond_is.push(Instr::IJe(else_label));
            cond_is.append(&mut thn_is);
            cond_is.push(Instr::IJmp(end_label));
            cond_is.push(Instr::ISetLabel(else_label));
            cond_is.append(&mut els_is);
            cond_is.push(Instr::ISetLabel(end_label));

            cond_is
        },
        Expr::Loop(e) => {
            let startloop = LabelVal::Label(Label::LOOP, int_post_inc(l));
            let endloop = LabelVal::Label(Label::LOOPEND, int_post_inc(l));
            let mut e_is = compile_to_instrs(e, si, env, &endloop, l);
            
            let mut is = vec!(Instr::ISetLabel(startloop));
            is.append(&mut e_is);
            is.push(Instr::IJmp(startloop));
            is.push(Instr::ISetLabel(endloop));

            is
        },
        Expr::Break(e) => {
            if *br == LabelVal::NOT_SET {
                panic!("break exists outside of loop");
            }
            let mut e_is = compile_to_instrs(e, si, env, br, l);
            e_is.push(Instr::IJmp(*br));
            e_is
        },
        Expr::Set(name, val) => {
            if env.contains_key(name) == false {
                panic! ("Unbound variable identifier {}", name);
            }

            let offset = *env.get(name).unwrap();
            let mut val_is = compile_to_instrs(val, si, env, br, l);
            val_is.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX)));
            val_is
        },
        Expr::Block(es) => {
            let mut is = Vec::<Instr>::new();
            for e in es.into_iter() {
                is.append(&mut compile_to_instrs(e, si, env, br, l));
            }
            is
        },
    }
}

// compiler helper functions -------------------------------------------------------------

// helper function for let bindings
fn bindings_repeat(bindings: &Vec<(String, Expr)>) -> bool {
    let mut binds = bindings.iter().map(|(x,_)| x).collect::<Vec<_>>();
    binds.sort();
    binds.dedup();

    bindings.len() != binds.len()
}

// helper function to check if a let binding ID is invalid because it is a keyword
fn is_invalid_id(id: &String) -> bool{
    let invd = vec!("true","false","input",
                    "let", "set!","if","block","loop", "break",
                    "add1","sub1","isnum","isbool",
                    "+","-","*","<",">",">=","<=","="
                  );

    invd.iter().any(|e| e == id)
}

//compiler Instr to string methods -------------------------------------------------------

fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(v1,v2) => {
            let s1 = val_to_str(v1);
            let s2 = val_to_str(v2);
            format! ("  mov {s1}, {s2}")
        },
        Instr::ICmove(v1,v2) => {
            let s1 = val_to_str(v1);
            let s2 = val_to_str(v2);
            format! ("  cmove {s1}, {s2}")
        },
        Instr::ICmovne(v1,v2) => {
            let s1 = val_to_str(v1);
            let s2 = val_to_str(v2);
            format! ("  cmovne {s1}, {s2}")
        },
        Instr::ICmovl(v1,v2) => {
            let s1 = val_to_str(v1);
            let s2 = val_to_str(v2);
            format! ("  cmovl {s1}, {s2}")
        },
        Instr::ICmovle(v1,v2) => {
            let s1 = val_to_str(v1);
            let s2 = val_to_str(v2);
            format! ("  cmovle {s1}, {s2}")
        },
        Instr::ICmovg(v1,v2) => {
            let s1 = val_to_str(v1);
            let s2 = val_to_str(v2);
            format! ("  cmovg {s1}, {s2}")
        },
        Instr::ICmovge(v1,v2) => {
            let s1 = val_to_str(v1);
            let s2 = val_to_str(v2);
            format! ("  cmovge {s1}, {s2}")
        },

        Instr::IAdd(v1,v2) => {
            let s1 = val_to_str(v1);
            let s2 = val_to_str(v2);
            format! ("  add {s1}, {s2}")
        },
        Instr::ISub(v1,v2) => {
            let s1 = val_to_str(v1);
            let s2 = val_to_str(v2);
            format! ("  sub {s1}, {s2}")
        },
        Instr::IMul(v1,v2) => {
            let s1 = val_to_str(v1);
            let s2 = val_to_str(v2);
            format! ("  imul {s1}, {s2}")
        },
        Instr::IDiv(v1) => {
            let s1 = val_to_str(v1);
            format! ("  idiv {s1}")
        },
        Instr::ISar(v1, v2) => {
            let s1 = val_to_str(v1);
            let s2 = val_to_str(v2);
            format! ("  sar {s1}, {s2}")
        },
        Instr::IXor(v1, v2) => {
            let s1 = val_to_str(v1);
            let s2 = val_to_str(v2);
            format! ("  xor {s1}, {s2}")
        },
        Instr::ICmp(v1, v2) => {
            let s1 = val_to_str(v1);
            let s2 = val_to_str(v2);
            format! ("  cmp {s1}, {s2}")
        },
        Instr::ITest(v1, v2) => {
            let s1 = val_to_str(v1);
            let s2 = val_to_str(v2);
            format! ("  test {s1}, {s2}")
        },

        Instr::ISetLabel(l) => {
            let l_s = labelval_to_str(l);
            format! ("{l_s}:")
        },
        Instr::IJmp(l) => {
            let l_s = labelval_to_str(l);
            format! ("  jmp {l_s}")
        },
        Instr::IJnz(l) => {
            let l_s = labelval_to_str(l);
            format! ("  jnz {l_s}")
        },
        Instr::IJz(l) => {
            let l_s = labelval_to_str(l);
            format! ("  jz {l_s}")
        },
        Instr::IJe(l) => {
            let l_s = labelval_to_str(l);
            format! ("  je {l_s}")
        },
        Instr::IJne(l) => {
            let l_s = labelval_to_str(l);
            format! ("  jne {l_s}")
        },
        Instr::IJo(l) => {
            let l_s = labelval_to_str(l);
            format! ("  jo {l_s}")
        },
    }
}

fn val_to_str(v: &Val) -> String {
    match v{
        Val::Reg(r) => reg_to_str(r),
        Val::Imm(n) => format! ("{}",*n),
        Val::RegOffset(r, n) => {
            let reg = reg_to_str(r);
            format! ("[{reg}-{}]",*n)
        }
        Val::ErrorCode(err) => {
            let n = errorcode_to_i64(err);
            format! ("{}", n)
        }
    }
}

fn reg_to_str(r: &Reg) -> String {
    match r {
        Reg::RAX => format!("rax"),
        Reg::RSP => format!("rsp"),
        Reg::RBX => format!("rbx"),
        Reg::RDI => format!("rdi"),
        Reg::R10 => format!("r10"),
    }
}

fn errorcode_to_i64(code: &ErrorCode) -> i64 {
    match code {
        ErrorCode::OVERFLOW => 3,
        ErrorCode::INVALID_ARG => 7,
    }
}

fn labelval_to_str(v: &LabelVal) -> String {
    match v {
        LabelVal::ERROR => format!("throw_error"),
        LabelVal::NOT_SET => panic!("NOT_SET should not appear as a real label"),
        LabelVal::Label(l, n) => {
            let label = label_to_str(l);
            format! ("{label}_{}",*n)
        },
    }
}

fn label_to_str(l: &Label) -> String {
    match l {
        Label::IFEND => format!("ifend"),
        Label::IFELSE => format!("ifelse"),
        Label::LOOP => format!("loop"),
        Label::LOOPEND => format!("loopend"),
    }
}



//bringing it all together --------------------------------------------------------------------------

fn compile(e: &Expr) -> String {
    let mut labels = 0;
    let instrs = compile_to_instrs(e, 2, &HashMap::<String, i64>::new(), &LabelVal::NOT_SET, &mut labels);
    let mut curr: String = "".to_owned();
    for (i, instr) in instrs.iter().enumerate() {
        let is_string: &str = &instr_to_str(instr);
        if i == 0 {
            curr = curr + is_string;
        }
        else {
            curr = curr + "\n" + is_string;
        }
    }
    curr
}


//main function starts here --------------------------------------------------------------------------------------------------------------------------------------------------
fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let file_result = parse(&in_contents);
    let sexpr = match file_result {
        Ok(file) => file,
        Err(_) => panic!("Invalid"),
    };

    let expr = parse_expr(&sexpr);

    println!("{:?}", expr); //to see parse output

    let result = compile(&expr);

    let error_section = format!("
extern snek_error
throw_error:
  mov rdi,r10
  push rsp
  call snek_error
  ret
");
 
    let asm_program = format!(
        "
section .text
global our_code_starts_here
{}
our_code_starts_here:
{}
  ret
",
        error_section, result
    );

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}
