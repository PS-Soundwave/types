use core::fmt;

#[derive(Eq, Clone, PartialEq)]
enum Term {
    True,
    False,
    If(Box<Term>, Box<Term>, Box<Term>),
    Zero,
    Succ(Box<Term>),
    Pred(Box<Term>),
    IsZero(Box<Term>)
}

impl Term {
    fn is_numeric(&self) -> bool  {
        return match self {
            Term::Zero => true,
            Term::Succ(arg) => arg.is_numeric(),
            _ => false
        }
    }

    fn step(&self) -> Option<Term> {
        return match self {
            Term::True => Option::None,
            Term::False => Option::None,
            Term::If(guard, true_branch, false_branch) => match guard.as_ref() {
                Term::True => Option::Some(true_branch.as_ref().clone()),
                Term::False => Option::Some(false_branch.as_ref().clone()),
                _ => guard.step().map(|t| { Term::If(Box::new(t), true_branch.clone(), false_branch.clone()) })
            },
            Term::Zero => Option::None,
            Term::Succ(arg) => arg.step().map(|t| { Term::Succ(Box::new(t)) }),
            Term::Pred(arg) => match arg.as_ref() {
                Term::Zero => Option::None,
                Term::Succ(inner_arg) => if inner_arg.is_numeric() {
                    Option::Some(inner_arg.as_ref().clone())
                } else {
                    arg.step().map(|t| { Term::Pred(Box::new(t)) })
                },
                _ => arg.step().map(|t| { Term::Pred(Box::new(t)) })
            },
            Term::IsZero(arg) => match arg.as_ref() {
                Term::Zero => Option::Some(Term::True),
                Term::Succ(inner_arg) => if inner_arg.is_numeric() {
                    Option::Some(Term::True)
                } else {
                    arg.step().map(|t| { Term::IsZero(Box::new(t)) })
                },
                _ => arg.step().map(|t| { Term::IsZero(Box::new(t)) })
            }
        }
    }

    fn normalize(&self) -> Term {
        let mut term = self.clone();
        
        let mut next_term = term.step();

        while let Some(t) = next_term {
            term = t;
            next_term = term.step();
        }

        return term;
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_numeric() {
            let mut term = self;
            let mut value: u64 = 0;
            
            while *term != Term::Zero {
                value += 1;

                term = match term {
                    Term::Succ(arg) => arg.as_ref(),
                    _ => unreachable!()
                }
            }

            return write!(f, "{}", value);
        }

        return match self {
            Term::True => write!(f, "true"),
            Term::False => write!(f, "false"),
            Term::If(guard, true_branch, false_branch) => write!(f, "(if {} then {} else {})", guard, true_branch, false_branch),
            Term::Succ(arg) => write!(f, "(succ {})", arg),
            Term::Pred(arg) => write!(f, "(pred {})", arg),
            Term::IsZero(arg) => write!(f, "(iszero {})", arg),
            _ => unreachable!()
        };
    }
}

fn main() {
    let ex1 = Term::If(Box::new(Term::False), Box::new(Term::Zero), Box::new(Term::Succ(Box::new(Term::Zero))));
    let ex2 = Term::IsZero(Box::new(Term::Pred(Box::new(Term::Succ(Box::new(Term::Zero))))));

    println!("{}", ex1.normalize());
    println!("{}", ex2.normalize());
}
