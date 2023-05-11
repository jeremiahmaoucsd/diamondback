use std::env;

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: u64) -> u64;
}

#[export_name = "\x01snek_error"]
pub extern "C" fn snek_error(errcode: i64) {
    if errcode == 3 {
        eprintln!("an overflow occured");
    }
    else if errcode == 7 {
        eprintln!("an invalid argument was used");
    }
    else {eprintln!("an error ocurred {errcode}");}
    std::process::exit(1);
}

#[no_mangle]
#[export_name = "\x01snek_print"]
fn snek_print(val : u64) -> i64 {
    if val == 3 { println!("true"); }
    else if val == 1 { println!("false"); }
    else if val % 2 == 0 { println!("{}", (val as i64) >> 1); }
    else {
      println!("Unknown value: {}", val);
    }
    return val as i64;
}

fn parse_input(input: &str) -> u64 {

    let min_num = -4611686018427387904;
    let max_num = 4611686018427387903;

    // TODO: parse the input string into internal value representation
    if input == "false" {1}
    else if input == "" {1}
    else if input == "true" {3}
    else { 
        match input.parse::<i64>() {
            Ok(n) if n >= min_num && n <= max_num => (n << 1) as u64,
            _ => panic! ("bad input"),
        }
    }
}


fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: u64 = unsafe { our_code_starts_here(input) };
    snek_print(i);
}
