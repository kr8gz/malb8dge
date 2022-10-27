use std::cmp::Ordering;

use itertools::Itertools;
use rand::Rng;

use crate::run::types::*;

use super::*;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum PrecType {
    Before,
    LeftAssoc,
    RightAssoc,
    Compare,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum OpType {
    Before,
    Binary,
    After,
    Other,
}

macro_rules! operators {
    (
        $( $type:ident: $( $op:literal ),+, )*
        % no precedence
        $( $type_n:ident: $( $op_n:literal ),+, )*
    ) => {
        const PREC_LIST: &[PrecType] = &[ $( PrecType::$type, )* ];

        const OP_TABLE: &[(OpType, &str)] = &[
            $( $( (op_type(PrecType::$type), $op), )+ )*
            $( $( (OpType::$type_n, $op_n), )+ )*
        ];

        pub const COMBINED_SYMBOLS: &[&str] = &[ $( $( $op, )+ )* $( $( $op_n, )+ )* ];
        pub const MAX_PREC: usize = PREC_LIST.len();

        pub fn op_prec(op_type: OpType, op: &str) -> usize {
            let mut prec = 0;
            $(
                prec += 1;
                if self::op_type(PrecType::$type) == op_type {
                    match op {
                        $( $op => return prec, )+
                        _ => ()
                    }
                }
            )*
            usize::MAX
        }
    }
}

pub fn is_op(op_type: OpType, op: &str) -> bool {
    OP_TABLE.contains(&(op_type, op))
}

pub fn prec_type(prec: usize) -> PrecType {
    PREC_LIST[prec - 1]
}

pub const fn op_type(prec_type: PrecType) -> OpType {
    match prec_type {
        PrecType::Before => OpType::Before,
        _ => OpType::Binary,
    }
}

// precedence goes from top to bottom, same row = same precedence
// before operator / binary operator + backslash combos are an option for new operators
// bef + is also free
operators! {
    RightAssoc: "||",
    RightAssoc: "&&",
    RightAssoc: "|",
    RightAssoc: "&",
    Before:     "!",

    Compare:    "<", ">", "<=", ">=", "!=", "==",
    LeftAssoc:  "-?",

    LeftAssoc:  "..", "#",
    Before:     "^",

    LeftAssoc:  "?\\",
    Before:     "?\\",

    LeftAssoc:  "%",
    LeftAssoc:  "+",
    LeftAssoc:  "-",
    LeftAssoc:  "*", "/", "//",
    LeftAssoc:  "+-", "/%",

    Before:     "-", ".", "`", "``",

    RightAssoc: "^",

    Before:     "*",

    LeftAssoc:  ".*", "^*",
    LeftAssoc:  "@",

    Before:     "@", "^^", "#", "'",

    % no precedence
    Before:     ";", "/", "|", "/|",
    After:      "^^", "##", "#\\", "#$", "_", "``", "$$", "$", "'", "`",
    Other:      "%%", "++", "--",
}

pub fn run_unary_op(memory: &mut Stack, target_id: usize, op_type: OpType, op: &str, pos: &Pos) -> Result<ValueType> {
    use ValueType::*;

    let mut target = memory[target_id].clone();
            
    let repr = match op_type {
        OpType::Before => format!("{}x", op),
        OpType::After => format!("x{}", op),
        _ => panic!("specified type isn't a unary operator")
    };

    macro_rules! unary_ops {
        (
            $(
                $impl_op:literal {
                    $(
                        $( % convert $from:pat => $to:expr; )+
                    )?
                    $( $a:pat => $ret:expr; )*
                }
            )*
        ) => {

            match op {
                $(
                    $impl_op => {
                        $(
                            match &target.data {
                                $( $from => target.data = $to, )+
                                _ => ()
                            }
                        )?

                        match target.data {
                            $( $a => $ret, )*
                            #[allow(unreachable_patterns)] // not every operator matches all types
                            _ => return Err(
                                Error::err("Type error")
                                    .label(target.pos.clone(), format!("This has type #{}#", target.type_name()))
                                    .label(pos.clone(), format!("#{repr}# is not implemented for type #{}#", target.type_name()))
                            )
                        }
                    }
                )*
                _ => unimplemented!("{repr}")
            }
        }
    }

    let ret = match op_type {
        OpType::Before => unary_ops! {
            "!" {
                a   =>  Boolean(!a.as_bool());
            }

            "^" {
                a   =>  List({
                            let i = a.as_int().ok_or_else(|| {
                                Error::err("Type error")
                                    .label(pos.clone(), format!("Expected an integer for {op}x"))
                                    .label(target.pos.clone(), format!("Cannot convert #{}# to an integer", a.as_repr_string(memory)))
                            })? as i64;
                            if i < 0 { i+1..1 } else { 0..i }.map(|i| memory.push(ValueType::Number(i as f64).into_value(pos))).collect()
                        });
            }

            "?\\" {
                % convert a @ (Boolean(_) | Null()) => Number(a.as_int().unwrap());
                Number(a) => Number(rand::thread_rng().gen_range(0.min(a as i64)..=0.max(a as i64)) as f64);
            }

            "-" {
                a   =>  Number(-a.as_num().ok_or_else(|| {
                            Error::err("Type error")
                                .label(pos.clone(), format!("Expected a number for {op}x"))
                                .label(target.pos.clone(), format!("Cannot convert #{}# to a number", a.as_repr_string(memory)))
                        })?);
            }

            "." {
                
            }

            "`" {

            }

            "``" {

            }

            "*" {

            }

            "@" {
                % convert Boolean(_) | Null() => String(target.as_repr_string(memory));

                String(a)   => String(a.reverse());
                List(mut a) => List({ a.reverse(); a });
                Number(a)   => Number(a.abs().to_string().reverse().parse::<f64>().unwrap().copysign(a));
            }

            "^^" {

            }

            "#" {
                
            }

            "'" {

            }
        },

        OpType::After => unary_ops! {
            "^^" {

            }

            "##" {

            }

            "#\\" {

            }

            "#$" {

            }

            "_" {
                % convert Boolean(_) | Null() => String(target.as_string(memory));

                String(a)   =>  Number(a.len() as f64);
                List(a)     =>  Number(a.len() as f64);
                Number(a)   =>  Number(a.to_string().len() as f64);
            }

            "``" {

            }

            "$$" {

            }

            "$" {
                % convert a @ (Boolean(_) | Null()) => Number(a.as_int().unwrap());

                a @ Number(_)   =>  a;
                String(a)       =>  Number(a.parse().map_err(|_| Error::err("Value error")
                                        .label(pos.clone(), "Found conversion to number")
                                        .label(target.pos, format!("Cannot convert #\"{a}\"# to a number"))
                                    )?);
            }

            "'" {
                % convert a @ (Boolean(_) | Null()) => Number(a.as_int().unwrap());
                Number(a) => Number(a.round());
            }

            "`" {
                % convert a @ (Boolean(_) | Null()) => Number(a.as_int().unwrap());
                Number(a) => Number(a.trunc());
            }
        },

        _ => unreachable!()
    };

    Ok(ret)
}

pub fn run_bin_op(memory: &mut Stack, lhs_id: usize, rhs_id: usize, op: &str, pos: &Pos) -> Result<ValueType> {
    use ValueType::*;

    let mut lhs = memory[lhs_id].clone();
    let mut rhs = memory[rhs_id].clone();

    macro_rules! push {
        ( $value:expr ) => { memory.push($value.into_value(pos)) }
    }

    macro_rules! bin_ops {
        (
            $(
                $op:literal {
                    $(
                        % convert
                        $( $from:pat => $to:expr; )+
                    )?
                    $(
                        % one way
                        $( $a1:pat, $b1:pat => $ret1:expr; )+
                    )?
                    $(
                        % both ways
                        $( $a2:pat, $b2:pat => $ret2:expr; )+
                    )?
                }
            )*
        ) => {
            match op {
                $(
                    $op => {
                        $(
                            match &lhs.data {
                                $( $from => lhs.data = $to, )+
                                _ => ()
                            }
                            match &rhs.data {
                                $( $from => rhs.data = $to, )+
                                _ => ()
                            }
                        )?

                        match (lhs.data.clone(), rhs.data.clone()) {
                            $(
                                $(
                                    ($a1, $b1) => $ret1,
                                )+
                            )?
                            $(
                                $(
                                    ($a2, $b2) => $ret2,
                                    
                                    #[allow(unused_assignments)] // they can be used in the op implementations
                                    ($b2, $a2) => {
                                        (lhs, rhs) = (rhs, lhs);
                                        $ret2
                                    }
                                )+
                            )?
                            #[allow(unreachable_patterns)] // not every operator matches all types
                            _ => {
                                let l_type = lhs.type_name();
                                let r_type = rhs.type_name();
                                return Err(
                                    Error::err("Type error")
                                        .label(lhs.pos, format!("This has type #{l_type}#"))
                                        .label(rhs.pos, format!("This has type #{r_type}#"))
                                        .label(pos.clone(), format!("Binary #{op}# is not implemented for types #{l_type}# and #{r_type}#"))
                                )
                            }
                        }
                    }
                )*
                _ => unimplemented!("a {op} b")
            }
        }
    }

    #[macro_export]
    macro_rules! unique {
        ( $memory:expr, $iter:expr ) => {
            {
                let mut seen = Vec::new();
                let mut unique = Vec::new();
                for el in $iter {
                    if !seen.contains(&$memory[el].data) {
                        unique.push(el);
                        seen.push($memory[el].data.clone());
                    }
                }
                unique
            }
        }
    }

    let ret = bin_ops! {
        "||" {
            % one way
            a, b => Boolean(a.as_bool() || b.as_bool());
        }
    
        "&&" {
            % one way
            a, b => Boolean(a.as_bool() && b.as_bool());
        }
    
        "|" {
            % one way
            a, b => if a.as_bool() { a } else { b };
        }
        
        "&" {
            % one way
            a, b => if a.as_bool() { b } else { a };
        }
    
        "-?" {
            // something with 1 and -1 lol
        }
    
        ".." {
    
        }
    
        "#" {
    
        }
    
        "?\\" {
            % convert
            x @ (Boolean(_) | Null()) => Number(x.as_int().unwrap());
    
            % one way
            Number(a),  Number(b)   => Number(rand::thread_rng().gen_range(a.min(b) as i64..=a.max(b) as i64) as f64);
        }
    
        "%" {
            % convert
            x @ (Boolean(_) | Null()) => Number(x.as_int().unwrap());
    
            % one way
            Number(a),  Number(b)   =>  Number(a % b);
        }
    
        "+" {
            % convert
            x @ (Boolean(_) | Null()) => Number(x.as_int().unwrap());
    
            % one way
            Number(a),  Number(b)   =>  Number(a + b);
            List(a),    List(b)     =>  List(a.into_iter().chain(b.into_iter()).collect());
            String(a),  String(b)   =>  String(a + &b);
            String(a),  b @ List(_) =>  String(a + &b.as_joined_list_string(memory, ""));
            
            List(a),    _           =>  List(a.into_iter().chain([rhs_id]).collect());
            String(a),  _           =>  String(a + &rhs.as_string(memory));
            _,          String(b)   =>  String(lhs.as_string(memory) + &b);
        }
    
        "-" {
            % convert
            x @ (Boolean(_) | Null()) => Number(x.as_int().unwrap());
    
            % one way
            List(mut a), List(b)    =>  List({
                                            for b_id in b {
                                                if let Some(pos) = a.iter().position(|&el| memory[el].data == memory[b_id].data) {
                                                    a.remove(pos);
                                                }
                                            };
                                            a
                                        });
            List(mut a), _          =>  List({
                                            if let Some(pos) = a.iter().position(|&el| memory[el].data == memory[rhs_id].data) {
                                                a.remove(pos);
                                            }
                                            a
                                        });
    
            String(a),  String(b)   =>  String({
                                            if a.len() == 1 && b.len() == 1 {
                                                (a.chars().next().unwrap()..=b.chars().next().unwrap()).collect()
                                            } else {
                                                return Err(
                                                    Error::err("Value error")
                                                        .label(pos.clone(), "Expected strings with #length 1# for character range")
                                                        .label(lhs.pos, format!("This string has length #{}#", a.len()))
                                                        .label(rhs.pos, format!("This string has length #{}#", b.len()))
                                                )
                                            }
                                        });
    
            String(a),  Number(b)   =>  String({
                                            if b >= 0.0 {
                                                a.chars().dropping_back(b as usize).collect()
                                            } else {
                                                return Err(
                                                    Error::err("Value error")
                                                        .label(
                                                            pos.clone(),
                                                            format!(
                                                                "Expected #number >= 0# for right side of {} {op} {}",
                                                                lhs.type_name(), rhs.type_name()
                                                            ),
                                                        )
                                                        .label(rhs.pos, format!("{b} is not >= 0"))
                                                )
                                            }
                                        });
    
            Number(a),  Number(b)   =>  Number(a - b);
        }
    
        "*" {
            % convert
            x @ (Boolean(_) | Null()) => Number(x.as_int().unwrap());
    
            % one way
            Number(a),  Number(b)   =>  Number(a * b);
            List(a),    List(b)     =>  List(unique!(memory, a.into_iter().chain(b.into_iter())));
    
            % both ways
            List(a),    Number(b)   =>  List({
                                            let mut res = Vec::new();
                                            for _ in 0..b.abs() as usize {
                                                res.extend(a.iter().map(|&v| memory.push(memory[v].clone())));
                                            }
                                            if b < 0.0 { res.reverse(); }
                                            res
                                        });
            
            String(mut a), Number(b) => String({
                                            a = a.repeat(b.abs() as usize);
                                            if b < 0.0 { a = a.reverse(); }
                                            a
                                        });
        }
    
        "/" {
            % convert
            x @ (Boolean(_) | Null()) => Number(x.as_int().unwrap());
    
            % one way
            Number(a),  Number(b)   =>  Number(a / b);
            List(a),    List(b)     =>  List(unique!(memory, a.iter().chain(b.iter()).cloned().filter(|id| a.contains(id) && b.contains(id))));
        }
    
        "//" {
            % convert
            x @ (Boolean(_) | Null()) => Number(x.as_int().unwrap());
    
            % one way
            Number(a),  Number(b)   =>  Number((a / b).trunc());
        }
    
        "+-" {
            % convert
            x @ (Boolean(_) | Null()) => Number(x.as_int().unwrap());
    
            % one way
            Number(a),  Number(b)   =>  List(vec![push!(Number(a + b)), push!(Number(a - b))]);
        }
    
        "/%" {
            % convert
            x @ (Boolean(_) | Null()) => Number(x.as_int().unwrap());
    
            % one way
            Number(a),  Number(b)   =>  List(vec![push!(Number((a / b).trunc())), push!(Number(a % b))]);
        }
    
        "^" {
            % convert
            x @ (Boolean(_) | Null()) => Number(x.as_int().unwrap());
    
            % one way
            Number(a),  Number(b)   =>  Number(a.powf(b));
            List(a),    List(b)     =>  List(unique!(memory, a.iter().chain(b.iter()).cloned().filter(|id| a.contains(id) != b.contains(id))));
            
            List(a),    String(b)   =>  String(a.into_iter().map(|el| memory[el].as_string(memory)).join(&b));
            String(a),  String(b)   =>  String(a.chars().map(|c| c.to_string()).intersperse(b).collect());
        }
    
        ".*" {
            % one way
            a, b => match lhs.compare(&rhs, pos)? {
                Ordering::Less | Ordering::Equal => a,
                _ => b
            };
        }
    
        "^*" {
            % one way
            a, b => match lhs.compare(&rhs, pos)? {
                Ordering::Greater => a,
                _ => b
            };
        }
    
        "@" {
    
        }
    };

    Ok(ret)
}
