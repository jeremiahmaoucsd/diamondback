use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::Atom::*;
use sexp::*;

use im::HashMap;

const WORD_SIZE: i64 = 8;

//parser enums (Expr) ---------------------------------------------------
#[derive(Debug)]
struct Program {
  defs: Vec<Definition>,
  main: Expr,
}

#[derive(Debug)]
enum Definition {
  Fun(String, Vec<String>, Expr),
}

use Definition::*;

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

    Call(String, Vec<Expr>),
}

#[derive(Debug)]
enum Op1 {
    Add1,
    Sub1,
    IsNum, 
    IsBool, 
    Print,
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

fn parse_program(s: &Sexp) -> Program {
  match s {
      Sexp::List(vec) => {
          let mut defs: Vec<Definition> = vec![];
          for def_or_exp in vec {
              if is_def(def_or_exp) {
                  defs.push(parse_definition(def_or_exp));
              } else {
                  return Program {
                      defs: defs,
                      main: parse_expr(def_or_exp),
                  };
              }
          }
          panic!("Only found definitions");
      }
      _ => panic!("Program should be a list")
  }
}

fn is_def(s: &Sexp) -> bool {
  match s {
      Sexp::List(def_vec) => match &def_vec[..] {
          [Sexp::Atom(S(keyword)), Sexp::List(_), _] if keyword == "fun" => true,
          _ => false
      }
      _ => false,
  }
}

fn parse_definition(s: &Sexp) -> Definition {
  match s {
      Sexp::List(def_vec) => match &def_vec[..] {
          [Sexp::Atom(S(keyword)), Sexp::List(name_vec), body] if keyword == "fun" => match &name_vec[..] {
              [Sexp::Atom(S(funname)), args @ ..] => {

                let args: Vec<String> = args.iter().map(|arg| {
                  match arg {
                      Sexp::Atom(S(string_arg)) => string_arg.to_string(),
                      _ => panic!("Invalid argument type!"),
                  }
                }).collect();
                
                if funparam_repeat(&args) {panic! ("duplicate parameters in function {}", funname)};

                for arg in args.iter(){
                  if is_invalid_id(arg) {panic! ("invalid parameter name {}", arg);}
                }
                  Fun(funname.to_string(), args.into_iter().map(|arg| arg.to_string()).collect::<Vec<String>>(), parse_expr(body))
              }
              _ => panic!("Invalid"),
          },
          _ => panic!("Invalid"),
      },
      _ => panic!("Invalid"),
  }
}
//error in compiling definitions if arguments are repeated
fn funparam_repeat(params: &Vec<String>) -> bool {
  let mut params2 = params.clone();
  params2.sort();
  params2.dedup();

  params2.len() != params.len()
}


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
            //op1
              [Sexp::Atom(S(op)), e] if op == "add1" => Expr::UnOp(Op1::Add1, Box::new(parse_expr(e))),
              [Sexp::Atom(S(op)), e] if op == "sub1" => Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e))),
              [Sexp::Atom(S(op)), e] if op == "isnum" => Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e))),
              [Sexp::Atom(S(op)), e] if op == "isbool" => Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e))),
              [Sexp::Atom(S(op)), e] if op == "isbool" => Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e))),
              [Sexp::Atom(S(op)), e] if op == "print" => Expr::UnOp(Op1::Print, Box::new(parse_expr(e))),
            //op2
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
              [Sexp::Atom(S(op)), e] if op == "print" => Expr::Break(Box::new(parse_expr(e))),

              [Sexp::Atom(S(funname)), exprs @ ..] if !is_invalid_id(funname) =>{
                Expr::Call(funname.to_string(), exprs.into_iter().map(parse_expr).collect::<Vec<Expr>>())
              },
              _ => panic!("Invalid: {}", s),
          }
      },
      _ => panic!("Invalid: {}", s),
  }
}

// helper function to check if a let binding ID is invalid because it is a keyword -- compile time
// also to check if a function binding is a function binding or just a messed up number of arguments after a different key word
fn is_invalid_id(id: &String) -> bool{
  let invd = vec!("true","false","input",
                  "let", "set!","if","block","loop", "break",
                  "add1","sub1","isnum","isbool",
                  "+","-","*","<",">",">=","<=","=",
                  "fun", "print"
                );

  invd.iter().any(|e| e == id)
}

//helper function for let bindings
fn parse_bind(s: &Sexp) -> (String, Expr) {
  match s {
      Sexp::List(vec) => {
          match &vec[..] {
              [Sexp::Atom(S(id)), e] => (id.to_string(), parse_expr(e)),
              _ => panic!("Invalid: {}", s),
          }
      },
      _ => panic!("Invalid: {}", s),
  }
}


//Compiler (Instr) enums ---------------------------------------------------------------------------------------------------------------------------------------
#[derive(Debug, Clone, Eq, PartialEq)]
enum Instr {

    IMov(Val, Val),
    ICmove(Val, Val),
    ICmovl(Val, Val),
    ICmovle(Val, Val),
    ICmovg(Val, Val),
    ICmovge(Val, Val),

    // Arithmetic
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    ISar(Val, Val), // shift arithmetic right

    // Logic
    IXor(Val, Val),

    // working with jumps
    ICmp(Val, Val),
    ITest(Val, Val),
    
    ISetLabel(LabelVal),
    IJmp(LabelVal),
    IJnz(LabelVal),
    IJe(LabelVal),
    IJne(LabelVal),
    IJo(LabelVal),

    ISetFunc(FuncVal),
    ICall(FuncVal),
    IRet,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum Val {
    Reg(Reg),
    RegAccess(Reg),
    Imm(i64),
    RegOffsetAccess(Reg, i64),
    ErrorCode(ErrorCode),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum LabelVal {
    Label(Label, i64),
    ERROR,
    NOTSET,
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum FuncVal {
    SNEKPRINT,
    UserDef(String),
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
    INVALIDARG, // 7
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

fn compile_expr_to_instrs(e: &Expr, si: i64, env: &HashMap<String, i64>, br: &LabelVal, l: &mut i64, main: bool, defs: &Vec<Definition>)  -> Vec<Instr> {
    //common expressions
    let check_overflow_arith = vec!(Instr::IMov(Val::Reg(Reg::R10), Val::ErrorCode(ErrorCode::OVERFLOW)), Instr::IJo(LabelVal::ERROR));

    let if_bool_err = vec!(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)),
                            Instr::IMov(Val::Reg(Reg::R10), Val::ErrorCode(ErrorCode::INVALIDARG)), 
                            Instr::IJnz(LabelVal::ERROR)
                        );

    match e{
        Expr::Number(n) => vec!(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n << 1))),
        Expr::Boolean(true) => vec!(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3))),
        Expr::Boolean(false) => vec!(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))),
        Expr::Id(s) if s == "input" && main => vec!(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI))),
        Expr::Id(s) if s == "input" && !main => panic!("input cannot be used in function definition!"),

        Expr::Id(s) => {
            if env.contains_key(s) == false {
                panic! ("Unbound variable identifier {}", s);
            }
            let offset = *env.get(s).unwrap();
            vec!(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffsetAccess(Reg::RSP, offset)))
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
                
                let mut val_is = compile_expr_to_instrs(&val, si_mut, &nenv, br, l, main, defs);
                let offset = si_mut * WORD_SIZE;
                nenv = nenv.update(name.to_string(), offset);

                instrs.append(&mut val_is);
                instrs.push(Instr::IMov(Val::RegOffsetAccess(Reg::RSP, offset), Val::Reg(Reg::RAX)));
                si_mut += 1;
            }
            let mut body_is = compile_expr_to_instrs(body, si_mut, &nenv, br, l, main, defs);
            instrs.append(&mut body_is);
            instrs
        },
        Expr::UnOp(Op1::Add1, subexpr) => {
            let mut e = compile_expr_to_instrs(subexpr, si, env, br, l, main, defs);

            //check for invalid type
            e.append(&mut if_bool_err.clone());

            e.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1 << 1)));
            e.append(&mut check_overflow_arith.clone());
            e
        },

        Expr::UnOp(Op1::Sub1, subexpr) => {
            let mut e = compile_expr_to_instrs(subexpr, si, env, br, l, main, defs);

            //check for invalid type
            e.append(&mut if_bool_err.clone());

            e.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(1 << 1)));
            e.append(&mut check_overflow_arith.clone());
            e
        },

        Expr::UnOp(Op1::IsNum, subexpr) => {
            let mut e = compile_expr_to_instrs(subexpr, si, env, br, l, main, defs);

            e.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1))); // if rax is a number, this will return 0

            e.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))); // bool::false
            e.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
            e.push(Instr::ICmove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));

            e
        }

        Expr::UnOp(Op1::IsBool, subexpr) => {
            let mut e = compile_expr_to_instrs(subexpr, si, env, br, l, main, defs);

            e.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1))); // if rax is a number, this will return 0

            e.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3))); // bool::true
            e.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(1)));
            e.push(Instr::ICmove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));

            e
        }

        Expr::UnOp(Op1::Print, subexpr) => {
          let mut e = compile_expr_to_instrs(subexpr, si, env, br, l, main, defs);
          let index = if si % 2 == 1 {si + 2} else {si + 1};
          let offset = index * WORD_SIZE;

          e.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(offset)));
          e.push(Instr::IMov(Val::RegAccess(Reg::RSP), Val::Reg(Reg::RDI)));
          e.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Reg(Reg::RAX)));
          e.push(Instr::ICall(FuncVal::SNEKPRINT));
          e.push(Instr::IMov(Val::Reg(Reg::RDI), Val::RegAccess(Reg::RSP)));
          e.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(offset)));

          e
      }

        Expr::BinOp(Op2::Plus, e1, e2) => {
            let mut e1_instr = compile_expr_to_instrs(e1, si, env, br, l, main, defs);
            let mut e2_instr = compile_expr_to_instrs(e2, si+1, env, br, l, main, defs);
            let stack_offset = si * WORD_SIZE;

            //check for invalid type
            e1_instr.append(&mut if_bool_err.clone());

            e1_instr.push(Instr::IMov(Val::RegOffsetAccess(Reg::RSP,stack_offset), Val::Reg(Reg::RAX)));
            e1_instr.append(&mut e2_instr); 

            //check for invalid type
            e1_instr.append(&mut if_bool_err.clone());

            e1_instr.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffsetAccess(Reg::RSP, stack_offset)));
            
            // check for overflow
            e1_instr.append(&mut check_overflow_arith.clone());

            e1_instr
        },

        Expr::BinOp(Op2::Minus, e1, e2) => {
            let mut e1_instr = compile_expr_to_instrs(e1, si, env, br, l, main, defs);
            let mut e2_instr = compile_expr_to_instrs(e2, si+1, env, br, l, main, defs);
            let stack_offset = si * WORD_SIZE;

            //check for invalid type
            e1_instr.append(&mut if_bool_err.clone());


            e1_instr.push(Instr::IMov(Val::RegOffsetAccess(Reg::RSP,stack_offset), Val::Reg(Reg::RAX)));
            e1_instr.append(&mut e2_instr); 

            //check for invalid type
            e1_instr.append(&mut if_bool_err.clone());


            e1_instr.push(Instr::ISub(Val::RegOffsetAccess(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
            e1_instr.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffsetAccess(Reg::RSP, stack_offset)));
            
            // check for overflow
            e1_instr.append(&mut check_overflow_arith.clone());

            e1_instr
        },

        Expr::BinOp(Op2::Times, e1, e2) => {
            let mut e1_instr = compile_expr_to_instrs(e1, si, env, br, l, main, defs);
            let mut e2_instr = compile_expr_to_instrs(e2, si+1, env, br, l, main, defs);
            let stack_offset = si * WORD_SIZE;

            //check for invalid type
            e1_instr.append(&mut if_bool_err.clone());

            e1_instr.push(Instr::IMov(Val::RegOffsetAccess(Reg::RSP,stack_offset), Val::Reg(Reg::RAX)));
            e1_instr.append(&mut e2_instr); 

            //check for invalid type
            e1_instr.append(&mut if_bool_err.clone());

            e1_instr.push(Instr::ISar(Val::Reg(Reg::RAX), Val::Imm(1))); //due to the way int values are stored
            e1_instr.push(Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffsetAccess(Reg::RSP, stack_offset)));
            
            // check for overflow
            e1_instr.append(&mut check_overflow_arith.clone());

            e1_instr
        },


        Expr::BinOp(Op2::Equal, e1, e2) => {
            let mut e1_instr = compile_expr_to_instrs(e1, si, env, br, l, main, defs);
            let mut e2_instr = compile_expr_to_instrs(e2, si+1, env, br, l, main, defs);
            let stack_offset = si * WORD_SIZE;

            e1_instr.push(Instr::IMov(Val::RegOffsetAccess(Reg::RSP,stack_offset), Val::Reg(Reg::RAX)));
            e1_instr.append(&mut e2_instr); 
            e1_instr.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            e1_instr.push(Instr::IXor(Val::Reg(Reg::RBX), Val::RegOffsetAccess(Reg::RSP,stack_offset)));
            e1_instr.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1)));
            e1_instr.push(Instr::IMov(Val::Reg(Reg::R10), Val::ErrorCode(ErrorCode::INVALIDARG)));
            e1_instr.push(Instr::IJne(LabelVal::ERROR));

            e1_instr.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffsetAccess(Reg::RSP,stack_offset)));
            e1_instr.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
            e1_instr.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            e1_instr.push(Instr::ICmove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
            e1_instr
        },

        Expr::BinOp(Op2::Greater, e1, e2) => {
            let mut e1_instr = compile_expr_to_instrs(e1, si, env, br, l, main, defs);
            let mut e2_instr = compile_expr_to_instrs(e2, si+1, env, br, l, main, defs);
            let stack_offset = si * WORD_SIZE;

            //check for invalid type
            e1_instr.append(&mut if_bool_err.clone());

            e1_instr.push(Instr::IMov(Val::RegOffsetAccess(Reg::RSP,stack_offset), Val::Reg(Reg::RAX))); //e1 moved to stack
            e1_instr.append(&mut e2_instr); 

            //check for invalid type
            e1_instr.append(&mut if_bool_err.clone());

            e1_instr.push(Instr::ICmp(Val::RegOffsetAccess(Reg::RSP,stack_offset), Val::Reg(Reg::RAX)));
            e1_instr.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
            e1_instr.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            e1_instr.push(Instr::ICmovg(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));

            e1_instr
        },

        Expr::BinOp(Op2::GreaterEqual, e1, e2) => {
            let mut e1_instr = compile_expr_to_instrs(e1, si, env, br, l, main, defs);
            let mut e2_instr = compile_expr_to_instrs(e2, si+1, env, br, l, main, defs);
            let stack_offset = si * WORD_SIZE;

            //check for invalid type
            e1_instr.append(&mut if_bool_err.clone());

            e1_instr.push(Instr::IMov(Val::RegOffsetAccess(Reg::RSP,stack_offset), Val::Reg(Reg::RAX))); //e1 moved to stack
            e1_instr.append(&mut e2_instr); 

            //check for invalid type
            e1_instr.append(&mut if_bool_err.clone());

            e1_instr.push(Instr::ICmp(Val::RegOffsetAccess(Reg::RSP,stack_offset), Val::Reg(Reg::RAX)));
            e1_instr.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
            e1_instr.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            e1_instr.push(Instr::ICmovge(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));

            e1_instr
        },

        Expr::BinOp(Op2::Less, e1, e2) => {
            let mut e1_instr = compile_expr_to_instrs(e1, si, env, br, l, main, defs);
            let mut e2_instr = compile_expr_to_instrs(e2, si+1, env, br, l, main, defs);
            let stack_offset = si * WORD_SIZE;

            //check for invalid type
            e1_instr.append(&mut if_bool_err.clone());

            e1_instr.push(Instr::IMov(Val::RegOffsetAccess(Reg::RSP,stack_offset), Val::Reg(Reg::RAX))); //e1 moved to stack
            e1_instr.append(&mut e2_instr); 

            //check for invalid type
            e1_instr.append(&mut if_bool_err.clone());

            e1_instr.push(Instr::ICmp(Val::RegOffsetAccess(Reg::RSP,stack_offset), Val::Reg(Reg::RAX)));
            e1_instr.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
            e1_instr.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            e1_instr.push(Instr::ICmovl(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));

            e1_instr
        },

        Expr::BinOp(Op2::LessEqual, e1, e2) => {
            let mut e1_instr = compile_expr_to_instrs(e1, si, env, br, l, main, defs);
            let mut e2_instr = compile_expr_to_instrs(e2, si+1, env, br, l, main, defs);
            let stack_offset = si * WORD_SIZE;

            //check for invalid type
            e1_instr.append(&mut if_bool_err.clone());

            e1_instr.push(Instr::IMov(Val::RegOffsetAccess(Reg::RSP,stack_offset), Val::Reg(Reg::RAX))); //e1 moved to stack
            e1_instr.append(&mut e2_instr); 

            //check for invalid type
            e1_instr.append(&mut if_bool_err.clone());

            e1_instr.push(Instr::ICmp(Val::RegOffsetAccess(Reg::RSP,stack_offset), Val::Reg(Reg::RAX)));
            e1_instr.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
            e1_instr.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            e1_instr.push(Instr::ICmovle(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));

            e1_instr
        },
        Expr::If(e1, e2, e3) => {
            let end_label = LabelVal::Label(Label::IFEND, int_post_inc(l));
            let else_label = LabelVal::Label(Label::IFELSE, int_post_inc(l));
            let mut cond_is = compile_expr_to_instrs(e1, si, env, br, l, main, defs);
            let mut thn_is = compile_expr_to_instrs(e2, si, env, br, l, main, defs);
            let mut els_is = compile_expr_to_instrs(e3, si, env, br, l, main, defs);

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
            let mut e_is = compile_expr_to_instrs(e, si, env, &endloop, l, main, defs);
            
            let mut is = vec!(Instr::ISetLabel(startloop));
            is.append(&mut e_is);
            is.push(Instr::IJmp(startloop));
            is.push(Instr::ISetLabel(endloop));

            is
        },
        Expr::Break(e) => {
            if *br == LabelVal::NOTSET {
                panic!("break exists outside of loop");
            }
            let mut e_is = compile_expr_to_instrs(e, si, env, br, l, main, defs);
            e_is.push(Instr::IJmp(*br));
            e_is
        },
        Expr::Set(name, val) => {
            if env.contains_key(name) == false {
                panic! ("Unbound variable identifier {}", name);
            }

            let offset = *env.get(name).unwrap();
            let mut val_is = compile_expr_to_instrs(val, si, env, br, l, main, defs);
            val_is.push(Instr::IMov(Val::RegOffsetAccess(Reg::RSP, offset), Val::Reg(Reg::RAX)));
            val_is
        },
        Expr::Block(es) => {
            let mut is = Vec::<Instr>::new();
            for e in es.into_iter() {
                is.append(&mut compile_expr_to_instrs(e, si, env, br, l, main, defs));
            }
            is
        },
        Expr::Call(name, args) => {
          let argslength = args.len() as i64;
          if !funname_exists(defs, name) {panic!("function call to undefined function: {name}")};
          if funname_params(defs, name) != argslength as i64 {panic! ("function called with incorrect number of params: {}", name)};

          let offset: i64 = (argslength as i64 + 1) * WORD_SIZE; // one extra word for rdi saving, one for each arg

          let mut e = Vec::<Instr>::new();

          for (index, arg) in args.iter().enumerate(){
            let index = index as i64;
            let mut argcomp = compile_expr_to_instrs(arg, si + index, env, br, l, main, defs);
            let curr_word = (si + index)*WORD_SIZE;

            e.append(&mut argcomp);

            if index != argslength as i64 - 1 {
              e.push(Instr::IMov(Val::RegOffsetAccess(Reg::RSP, curr_word), Val::Reg(Reg::RAX)));
            }
          }
          
          e.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(offset))); // shifting rsp
          
          for (index, _) in args.iter().enumerate(){
            let index = index as i64;
            let curr_word = (si + index)*WORD_SIZE;
            let curr_word_after_sub = curr_word + offset;

            if index == argslength as i64 - 1 {
              e.push(Instr::IMov(Val::RegOffsetAccess(Reg::RSP, index * WORD_SIZE), Val::Reg(Reg::RAX)));
              break;
            }

            e.push(Instr::IMov(Val::Reg(Reg::RBX), Val::RegOffsetAccess(Reg::RSP, curr_word_after_sub)));
            if index == 0 {
              e.push(Instr::IMov(Val::RegAccess(Reg::RSP), Val::Reg(Reg::RBX)));
            }
            else{
              e.push(Instr::IMov(Val::RegOffsetAccess(Reg::RSP, index * WORD_SIZE), Val::Reg(Reg::RBX)));
            }
          }
          e.push(Instr::IMov(Val::RegOffsetAccess(Reg::RSP, argslength as i64 * WORD_SIZE), Val::Reg(Reg::RDI)));
          e.push(Instr::ICall(FuncVal::UserDef(name.to_string())));
          e.push(Instr::IMov(Val::Reg(Reg::RDI), Val::RegOffsetAccess(Reg::RSP, argslength as i64 * WORD_SIZE)));
          e.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(offset)));

          e
        },
      
    }
}

fn compile_def_instrs(d: &Definition, defs: &Vec<Definition>, l: &mut i64)  -> Vec<Instr> {
  if funname_repeat(defs) {panic! ("function name overload not allowed")};
  match d {
    Fun(name, args, body) => {
        let depth = depth(body);
        let offset = depth * WORD_SIZE;
        let mut body_env = HashMap::<String,i64>::new();
        for (index, arg) in args.iter().enumerate() {
          body_env.insert(arg.to_string(), (depth + index as i64 + 1) * WORD_SIZE);  
        }

        let mut body_is = compile_expr_to_instrs(body, 0, &body_env, &LabelVal::NOTSET, l, false, defs);

        let mut e = vec!(Instr::ISetFunc(FuncVal::UserDef(name.to_string())));
        e.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(offset)));
        e.append(&mut body_is);
        e.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(offset)));
        e.push(Instr::IRet);
        e
    }
  } 
}

fn compile_main_instrs(e: &Expr, defs: &Vec<Definition>, l: &mut i64) -> Vec<Instr> {
  let depth = depth(e);
  let offset = depth * WORD_SIZE;

  let mut expr_instrs = compile_expr_to_instrs(e, 0, &HashMap::new(), &LabelVal::NOTSET, l, true, defs);
  let mut main = vec!(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(offset)));

  main.append(&mut expr_instrs);
  main.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(offset)));
  main
}

// compiler helper functions -------------------------------------------------------------

// helper function for let bindings
fn bindings_repeat(bindings: &Vec<(String, Expr)>) -> bool {
    let mut binds = bindings.iter().map(|(x,_)| x).collect::<Vec<_>>();
    binds.sort();
    binds.dedup();

    bindings.len() != binds.len()
}

//error in compiling definitions if a definition is repeated
fn funname_repeat(functions: &Vec<Definition>) -> bool {
  let mut names = functions.iter().map(|x| funname(x)).collect::<Vec<String>>();
  names.sort();
  names.dedup();

  functions.len() != names.len()
}

//error sent out in call1 or call2 due to non-existent function call
fn funname_exists(functions: &Vec<Definition>, name: &String) -> bool {
  let names = functions.iter().map(|x| funname(x)).collect::<Vec<_>>();
  names.iter().any(|e| e == name)
}

fn funname_params(functions: &Vec<Definition>, name: &String) -> i64 {
  functions.iter().map(|func| if *name == funname(func) {number_of_params(func)} else {0}).max().unwrap_or(0)
}

fn number_of_params(def: &Definition) -> i64 {
  match def{
    Definition::Fun(_, args, _) => args.len() as i64,
  }
}

//helper for helpers, gives a function name given a definition type
fn funname(def: &Definition) -> String {
  match def{
    Definition::Fun(name, _, _) => name.to_string(),
  }
}


fn depth(e: &Expr) -> i64 {
  let raw = depth_raw(e);
  if raw % 2 == 1 {raw + 1} else {raw}
}
// Generated by ChatGPT, depth
fn depth_raw(e: &Expr) -> i64 {
  match e {
      Expr::Number(_) => 0,
      Expr::Boolean(_) => 0,
      Expr::Id(_) => 0,
      Expr::Let(expr1s, expr2) => {
        let_depth(expr1s).max(depth_raw(expr2) + expr1s.len() as i64)
      },
      Expr::UnOp(_, expr) => depth_raw(expr),
      Expr::BinOp(_, expr1, expr2) => depth_raw(expr1).max(depth_raw(expr2) + 1),
      Expr::If(expr1, expr2, expr3) => depth_raw(expr1).max(depth_raw(expr2)).max(depth_raw(expr3)),
      Expr::Loop(expr) => depth_raw(expr),
      Expr::Break(expr) => depth_raw(expr),
      Expr::Set(_, expr) => depth_raw(expr),
      Expr::Block(exprs) => exprs.iter().map(|expr| depth_raw(expr)).max().unwrap_or(0),
      Expr::Call(_, exprs) => {
        call_depth(exprs)
      },
  }
}

fn let_depth(exprs: &Vec<(String, Expr)>) -> i64 {
  exprs
      .iter()
      .enumerate()
      .map(|(index, (_, expr))| depth_raw(expr) + index as i64)
      .max()
      .unwrap_or(0)
}

fn call_depth(exprs: &Vec<Expr>) -> i64 {
  exprs
      .iter()
      .enumerate()
      .map(|(index, expr)| depth_raw(expr) + index as i64)
      .max()
      .unwrap_or(0)
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
        Instr::ISetFunc(f) => {
            let f_s = funcval_to_str(f);
            format! ("{f_s}:")
        },
        Instr::ICall(f) => {
          let f_s = funcval_to_str(f);
          format! ("  call {f_s}")
        },
        Instr::IRet => format!("ret"),
    }
}

fn funcval_to_str(f: &FuncVal) -> String{
  match f {
    FuncVal::SNEKPRINT => format!("snek_print"),
    FuncVal::UserDef(s) => format!("{s}"),
  }
}

fn val_to_str(v: &Val) -> String {
    match v{
        Val::Reg(r) => reg_to_str(r),
        Val::RegAccess(r) => {
          let reg = reg_to_str(r);
          format! ("[{reg}]")
        }
        Val::Imm(n) => format! ("{}",*n),
        Val::RegOffsetAccess(r, n) => {
            let reg = reg_to_str(r);
            format! ("[{reg}+{}]",*n)
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
        ErrorCode::INVALIDARG => 7,
    }
}

fn labelval_to_str(v: &LabelVal) -> String {
    match v {
        LabelVal::ERROR => format!("throw_error"),
        LabelVal::NOTSET => panic!("NOTSET should not appear as a real label"),
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

fn compile_program(p: &Program) -> (String, String) {
    let mut labels = 0;
    let mut defs : String = "".to_owned();
    for def in &p.defs[..]{
      let instrs = compile_def_instrs(&def, &p.defs, &mut labels);
      for instr in instrs.iter() {
        let is_string: &str = &instr_to_str(instr);
        defs = defs + is_string + "\n";
      }
    }

    let main_instrs = compile_main_instrs(&p.main, &p.defs, &mut labels);
    let mut mainstr : String = "".to_owned();
    for (i, instr) in main_instrs.iter().enumerate() {
        let is_string: &str = &instr_to_str(instr);
        if i == 0 {
          mainstr = mainstr + is_string;
        }
        else {
          mainstr = mainstr + "\n" + is_string;
        }
    }
    (defs, mainstr)
}


//main function starts here --------------------------------------------------------------------------------------------------------------------------------------------------
fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let prog = "(".to_owned() + &in_contents + ")";

    let file_result = parse(&prog);

    let sexpr = match file_result {
        Ok(file) => file,
        Err(_) => panic!("Invalid"),
    };

    let prog = parse_program(&sexpr);

    println!("{:?}", prog); //to see parse output

    let (defs, main) = compile_program(&prog);

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
extern snek_print
{}
{}
our_code_starts_here:
{}
  ret
",
        error_section, defs, main
    );

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}
