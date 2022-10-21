use itertools::Itertools;

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

pub fn op_id(op_type: OpType, op: &str) -> usize {
    OP_TABLE.iter().position(|o| o == &(op_type, op)).unwrap()
}

pub fn id_sym(id: usize) -> &'static str {
    OP_TABLE[id].1
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

pub struct Operand {
    pub id: usize,
    pub data: ValueType,
    pub pos: Pos,
    pub type_name: String,
}

pub fn run_unary_op(memory: &mut Stack, pos: &Pos, mut target: Operand, op_type: OpType, op: &str) -> Result<ValueType> {
    use ValueType::*;
    crate::memory_helper_macros!(memory, pos);
            
    let repr = match op_type {
        OpType::Before => format!("{}x", op),
        OpType::After => format!("x{}", op),
        _ => panic!("specified type isn't a unary operator")
    };

    let err = Error::err("Type error")
        .label(target.pos.clone(), format!("This has type #{}#", target.type_name))
        .label(
            pos.clone(),
            format!("#{repr}# is not implemented for type #{}#", target.type_name)
        );

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
                            _ => return Err(err)
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
                            if i < 0 { i..0 } else { 0..i }.map(|i| store!(ValueType::Number(i as f64))).collect()
                        });
            }

            "?\\" {

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
                % convert Boolean(_) | Null() => String(value!(target.id).as_repr_string(memory));

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
                % convert Boolean(_) | Null() => String(value!(target.id).as_string(memory));

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
                
            }

            "`" {
                a => String(a.as_string(memory));
            }
        },

        _ => unreachable!()
    };

    Ok(ret)
}

pub fn run_bin_op(memory: &mut Stack, pos: &Pos, mut lhs: Operand, mut rhs: Operand, op: &str) -> Result<ValueType> {
    use ValueType::*;
    crate::memory_helper_macros!(memory, pos);
    
    let err = Error::err("Type error")
        .label(lhs.pos.clone(), format!("This has type #{}#", lhs.type_name))
        .label(rhs.pos.clone(), format!("This has type #{}#", rhs.type_name))
        .label(
            pos.clone(),
            format!("Binary #{op}# is not implemented for types #{}# and #{}#", lhs.type_name, rhs.type_name)
        );

    macro_rules! bin_ops {
        (
            $(
                $op:literal {
                    $(
                        % convert
                        // can use the id to get the unconverted version
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

                        match (lhs.data, rhs.data) {
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
                                        (lhs.id, rhs.id) = (rhs.id, lhs.id);
                                        $ret2
                                    }
                                )+
                            )?
                            #[allow(unreachable_patterns)] // not every operator matches all types
                            _ => return Err(err)
                        }
                    }
                )*
                _ => unimplemented!("a {op} b")
            }
        }
    }

    macro_rules! set_helper {
        ( $a:ident, $b:ident ) => { $a.iter().chain($b.iter()).unique().copied() }
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
    
        // compare operators (would probably be a lot of copy paste so maybe theres a better solution like with PartialOrd or something)
    
        "-?" {
            // something with 1 and -1 lol
        }
    
        ".." {
    
        }
        
        // might need a Range value type
    
        "#" {
    
        }
    
        "?\\" {
            % convert
            x @ (Boolean(_) | Null()) => Number(x.as_int().unwrap());
    
            // % one way
            // Number(a),  Number(b)   =>  Number( random number from a to b ); // what about floats
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
            String(a),  List(b)     =>  String(a + &join_list(&b, "", memory));
    
            String(a),  _           =>  String(a + &value!(rhs.id).as_string(memory));
            _,          String(b)   =>  String(value!(lhs.id).as_string(memory) + &b);
    
            % both ways
            List(a),    _           =>  List(a.into_iter().chain([rhs.id]).collect());
        }
    
        "-" {
            % convert
            x @ (Boolean(_) | Null()) => Number(x.as_int().unwrap());
    
            % one way
            List(mut a), List(b)    =>  List({ for b_id in b { a.remove_element(&b_id); }; a });
            List(mut a), _          =>  List({ a.remove_element(&rhs.id); a });
    
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
                                                            format!("Expected #number >= 0# for right side of {} {op} {}", lhs.type_name, rhs.type_name),
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
            List(a),    List(b)     =>  List(set_helper!(a, b).collect());
    
            % both ways
            List(a),    Number(b)   =>  List({
                                            let mut res = Vec::new();
                                            for _ in 0..b.abs() as usize {
                                                res.extend(a.iter().map(|&id| clone!(id)));
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
            List(a),    List(b)     =>  List(set_helper!(a, b).filter(|id| a.contains(id) && b.contains(id)).collect());
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
            Number(a),  Number(b)   =>  List(vec![store!(Number(a + b)), store!(Number(a - b))]);
        }
    
        "/%" {
            % convert
            x @ (Boolean(_) | Null()) => Number(x.as_int().unwrap());
    
            % one way
            Number(a),  Number(b)   =>  List(vec![store!(Number(a / b)), store!(Number(a % b))]);
        }
    
        "^" {
            % convert
            x @ (Boolean(_) | Null()) => Number(x.as_int().unwrap());
    
            % one way
            Number(a),  Number(b)   =>  Number(a.powf(b));
            List(a),    List(b)     =>  List(set_helper!(a, b).filter(|id| a.contains(id) != b.contains(id)).collect());
            
            List(a),    String(b)   =>  String(a.into_iter().map(|id| value!(id).as_string(memory)).join(&b));
            String(a),  String(b)   =>  String(a.chars().map(|c| c.to_string()).intersperse(b).collect());
        }
    
        ".*" {
    
        }
    
        "^*" {
            
        }
    
        "@" {
    
        }
    };

    Ok(ret)
}
