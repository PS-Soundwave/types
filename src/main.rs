use core::fmt;
use std::fmt::Display;

#[derive(Clone, Eq, PartialEq)]
enum Term {
    True(TermTrue),
    False(TermFalse),
    If(TermIf),
    Zero(TermZero),
    Succ(TermSucc),
    Pred(TermPred),
    IsZero(TermIsZero)
}

trait TTerm: Clone + Display + Eq + Into<Term> {
    fn is_numeric(&self) -> bool;

    fn step(&self) -> Option<Term>;

    fn normalize(&self) -> Term {
        let mut term: Term = self.clone().into();
        
        let mut next_term = term.step();

        while let Some(t) = next_term {
            term = t;
            next_term = term.step();
        }

        return term;
    }
}

impl TTerm for Term {
    fn is_numeric(&self) -> bool {
        return match self {
            Term::True(t) => t.is_numeric(),
            Term::False(t) => t.is_numeric(),
            Term::If(t) => t.is_numeric(),
            Term::Zero(t) => t.is_numeric(),
            Term::Succ(t) => t.is_numeric(),
            Term::Pred(t) => t.is_numeric(),
            Term::IsZero(t) => t.is_numeric()
        }
    }

    fn step(&self) -> Option<Term> {
        return match self {
            Term::True(t) => t.step(),
            Term::False(t) => t.step(),
            Term::If(t) => t.step(),
            Term::Zero(t) => t.step(),
            Term::Succ(t) => t.step(),
            Term::Pred(t) => t.step(),
            Term::IsZero(t) => t.step()
        }
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::True(t) => write!(f, "{}", t),
            Term::False(t) => write!(f, "{}", t),
            Term::If(t) => write!(f, "{}", t),
            Term::Zero(t) => write!(f, "{}", t),
            Term::Succ(t) => write!(f, "{}", t),
            Term::Pred(t) => write!(f, "{}", t),
            Term::IsZero(t) => write!(f, "{}", t)
        }
    }
}

impl From<TermTrue> for Term {
    fn from(value: TermTrue) -> Self {
        return Term::True(value);
    }
}

impl From<TermFalse> for Term {
    fn from(value: TermFalse) -> Self {
        return Term::False(value);
    }
}

impl From<TermIf> for Term {
    fn from(value: TermIf) -> Self {
        return Term::If(value);
    }
}

impl From<TermZero> for Term {
    fn from(value: TermZero) -> Self {
        return Term::Zero(value);
    }
}

impl From<TermSucc> for Term {
    fn from(value: TermSucc) -> Self {
        return Term::Succ(value);
    }
}

impl From<TermPred> for Term {
    fn from(value: TermPred) -> Self {
        return Term::Pred(value);
    }
}

impl From<TermIsZero> for Term {
    fn from(value: TermIsZero) -> Self {
        return Term::IsZero(value);
    }
}

#[derive(Clone, Eq, PartialEq)]
struct TermTrue {}

#[derive(Clone, Eq, PartialEq)]
struct TermFalse {}

#[derive(Clone, Eq, PartialEq)]
struct TermIf {
    guard: Box<Term>,
    true_branch: Box<Term>,
    false_branch: Box<Term>
}

#[derive(Clone, Eq, PartialEq)]
struct TermZero {}

#[derive(Clone, Eq, PartialEq)]
struct TermSucc {
    arg: Box<Term>
}

#[derive(Clone, Eq, PartialEq)]
struct TermPred {
    arg: Box<Term>
}

#[derive(Clone, Eq, PartialEq)]
struct TermIsZero {
    arg: Box<Term>
}

impl TTerm for TermTrue {
    fn is_numeric(&self) -> bool {
        return false;
    }

    fn step(&self) -> Option<Term> {
        return Option::None;
    }
}

impl Display for TermTrue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "true")
    }
}

impl TermTrue {
    fn new() -> TermTrue {
        return TermTrue {};
    }
}

impl TTerm for TermFalse {
    fn is_numeric(&self) -> bool {
        return false;
    }

    fn step(&self) -> Option<Term> {
        return Option::None;
    }
}

impl Display for TermFalse {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "false")
    }
}

impl TermFalse {
    fn new() -> TermFalse {
        return TermFalse {};
    }
}

impl TTerm for TermIf {
    fn is_numeric(&self) -> bool {
        return false;
    }

    fn step(&self) -> Option<Term> {
        return match self.guard.as_ref() {
            Term::True(_) => Option::Some(self.true_branch.as_ref().clone()),
            Term::False(_) => Option::Some(self.false_branch.as_ref().clone()),
            _ => self.guard.step().map(|t| { TermIf { guard: Box::new(t), true_branch: self.true_branch.clone(), false_branch: self.false_branch.clone() } }.into())
        };
    }
}

impl Display for TermIf {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(if {} then {} else {})", self.guard, self.true_branch, self.false_branch)
    }
}

impl TermIf {
    fn new(guard: impl Into<Term>, true_branch: impl Into<Term>, false_branch: impl Into<Term>) -> TermIf {
        return TermIf { guard: Box::new(guard.into()), true_branch: Box::new(true_branch.into()), false_branch: Box::new(false_branch.into()) };
    }
}

impl TTerm for TermZero {
    fn is_numeric(&self) -> bool {
        return true;
    }

    fn step(&self) -> Option<Term> {
        return Option::None;
    }
}

impl Display for TermZero {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "0")
    }
}

impl TermZero {
    fn new() -> TermZero {
        return TermZero {};
    }
}

impl TTerm for TermSucc {
    fn is_numeric(&self) -> bool {
        return self.arg.is_numeric();
    }

    fn step(&self) -> Option<Term> {
        return self.arg.step().map(|t| { TermSucc { arg: Box::new(t) } }.into());
    }
}

impl Display for TermSucc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_numeric() {
            let mut value: u64 = 1;
            let mut term: &Term = self.arg.as_ref();

            while let Term::Succ(t) = term {
                value += 1;

                term = t.arg.as_ref();
            }
            
            write!(f, "{}", value)
        }
        else {
            write!(f, "(succ {})", self.arg)
        }
    }
}

impl TermSucc {
    fn new(arg: impl Into<Term>) -> TermSucc {
        return TermSucc { arg: Box::new(arg.into()) };
    }
}

impl TTerm for TermPred {
    fn is_numeric(&self) -> bool {
        return false;
    }

    fn step(&self) -> Option<Term> {
        return match self.arg.as_ref() {
            Term::Zero(_) => Option::Some(TermZero {}.into()),
            Term::Succ(t) => if t.arg.is_numeric() {
                Option::Some(t.arg.as_ref().clone())
            } else {
                self.arg.step().map(|u| TermPred { arg: Box::new(u) }.into())
            },
            _ => self.arg.step().map(|t| TermPred { arg: Box::new(t) }.into())
        };
    }
}

impl Display for TermPred {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(pred {})", self.arg)
    }
}

impl TermPred {
    fn new(arg: impl Into<Term>) -> TermPred {
        return TermPred { arg: Box::new(arg.into()) };
    }
}

impl TTerm for TermIsZero {
    fn is_numeric(&self) -> bool {
        return false;
    }

    fn step(&self) -> Option<Term> {
        return match self.arg.as_ref() {
            Term::Zero(_) => Option::Some(TermTrue {}.into()),
            Term::Succ(t) => if t.is_numeric() {
                Option::Some(TermFalse {}.into())
            } else {
                self.arg.step().map(|u| TermIsZero { arg: Box::new(u) }.into())
            },
            _ => self.arg.step().map(|t| TermIsZero { arg: Box::new(t) }.into())
        };
    }
}

impl Display for TermIsZero {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(iszero {})", self.arg)
    }
}

impl TermIsZero {
    fn new(arg: impl Into<Term>) -> TermIsZero {
        return TermIsZero { arg: Box::new(arg.into()) };
    }
}

fn main() {
    let ex1 = TermIf::new(TermFalse::new(), TermZero::new(), TermSucc::new(TermZero::new()));
    let ex2 = TermIsZero::new(TermPred::new(TermSucc::new(TermZero::new())));

    println!("{}", ex1.normalize());
    println!("{}", ex2.normalize());
}
