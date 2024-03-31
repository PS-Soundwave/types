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
    IsZero(TermIsZero),
    Var(TermVar),
    App(TermApp),
    Lambda(TermLambda)
}

trait TTerm: Display + Clone + Eq + Into<Term> {
    fn is_numeric(&self) -> bool;

    fn step(&self) -> Term;

    fn substitute(&self, index: usize, arg: &Term) -> Term;

    fn lift(&self, cutoff: usize, amount: isize) -> Term;

    fn fmt_with_context(&self, context: &Vec<&str>, head: bool, tail: bool) -> String;

    fn normalize(&self) -> Term {
        let mut term: Term = self.step();
        let mut next_term = term.step();

        while next_term != term {
            term = next_term;
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
            Term::IsZero(t) => t.is_numeric(),
            Term::Var(t) => t.is_numeric(),
            Term::App(t) => t.is_numeric(),
            Term::Lambda(t) => t.is_numeric()
        }
    }

    fn step(&self) -> Term {
        return match self {
            Term::True(t) => t.step(),
            Term::False(t) => t.step(),
            Term::If(t) => t.step(),
            Term::Zero(t) => t.step(),
            Term::Succ(t) => t.step(),
            Term::Pred(t) => t.step(),
            Term::IsZero(t) => t.step(),
            Term::Var(t) => t.step(),
            Term::App(t) => t.step(),
            Term::Lambda(t) => t.step()
        }
    }

    fn substitute(&self, index: usize, arg: &Term) -> Term {
        return match self {
            Term::True(t) => t.substitute(index, arg),
            Term::False(t) => t.substitute(index, arg),
            Term::If(t) => t.substitute(index, arg),
            Term::Zero(t) => t.substitute(index, arg),
            Term::Succ(t) => t.substitute(index, arg),
            Term::Pred(t) => t.substitute(index, arg),
            Term::IsZero(t) => t.substitute(index, arg),
            Term::Var(t) => t.substitute(index, arg),
            Term::App(t) => t.substitute(index, arg),
            Term::Lambda(t) => t.substitute(index, arg)
        }
    }

    fn lift(&self, cutoff: usize, amount: isize) -> Term {
        return match self {
            Term::True(t) => t.lift(cutoff, amount),
            Term::False(t) => t.lift(cutoff, amount),
            Term::If(t) => t.lift(cutoff, amount),
            Term::Zero(t) => t.lift(cutoff, amount),
            Term::Succ(t) => t.lift(cutoff, amount),
            Term::Pred(t) => t.lift(cutoff, amount),
            Term::IsZero(t) => t.lift(cutoff, amount),
            Term::Var(t) => t.lift(cutoff, amount),
            Term::App(t) => t.lift(cutoff, amount),
            Term::Lambda(t) => t.lift(cutoff, amount),
        }
    }

    fn fmt_with_context(&self, context: &Vec<&str>, head: bool, tail: bool) -> String {
        return match self {
            Term::True(t) => t.fmt_with_context(context, head, tail),
            Term::False(t) => t.fmt_with_context(context, head, tail),
            Term::If(t) => t.fmt_with_context(context, head, tail),
            Term::Zero(t) => t.fmt_with_context(context, head, tail),
            Term::Succ(t) => t.fmt_with_context(context, head, tail),
            Term::Pred(t) => t.fmt_with_context(context, head, tail),
            Term::IsZero(t) => t.fmt_with_context(context, head, tail),
            Term::Var(t) => t.fmt_with_context(context, head, tail),
            Term::App(t) => t.fmt_with_context(context, head, tail),
            Term::Lambda(t) => t.fmt_with_context(context, head, tail)
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
            Term::IsZero(t) => write!(f, "{}", t),
            Term::Var(t) => write!(f, "{}", t),
            Term::App(t) => write!(f, "{}", t),
            Term::Lambda(t) => write!(f, "{}", t)
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

impl From<TermVar> for Term {
    fn from(value: TermVar) -> Self {
        return Term::Var(value);
    }
}

impl From<TermApp> for Term {
    fn from(value: TermApp) -> Self {
        return Term::App(value);
    }
}

impl From<TermLambda> for Term {
    fn from(value: TermLambda) -> Self {
        return Term::Lambda(value);
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
    
    fn step(&self) -> Term {
        return self.clone().into();
    }
    
    fn substitute(&self, _index: usize, _arg: &Term) -> Term {
        return self.clone().into();
    }
    
    fn lift(&self, _cutoff: usize, _amount: isize) -> Term {
        return self.clone().into();
    }
    
    fn fmt_with_context(&self, _context: &Vec<&str>, _head: bool, _tail: bool) -> String {
        return "true".to_owned();
    }
}

impl Display for TermTrue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.fmt_with_context(&vec![], true, true))
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
    
    fn step(&self) -> Term {
        return self.clone().into();
    }
    
    fn substitute(&self, _index: usize, _arg: &Term) -> Term {
        return self.clone().into();
    }
    
    fn lift(&self, _cutoff: usize, _amount: isize) -> Term {
        return self.clone().into();
    }
    
    fn fmt_with_context(&self, _context: &Vec<&str>, _head: bool, _tail: bool) -> String {
        return "false".to_owned();
    }
}

impl Display for TermFalse {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.fmt_with_context(&vec![], true, true))
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
    
    fn step(&self) -> Term {
        return match self.guard.as_ref() {
            Term::True(_) => self.true_branch.as_ref().clone(),
            Term::False(_) => self.false_branch.as_ref().clone(),
            t => TermIf::new(&t.step(), &self.true_branch.as_ref().clone(), &self.false_branch.as_ref().clone()).into()
        };
    }
    
    fn substitute(&self, index: usize, arg: &Term) -> Term {
        return TermIf::new(&self.guard.substitute(index, arg), &self.true_branch.substitute(index, arg), &self.false_branch.substitute(index, arg)).into();
    }
    
    fn lift(&self, cutoff: usize, amount: isize) -> Term {
        return TermIf::new(&self.guard.lift(cutoff, amount), &self.true_branch.lift(cutoff, amount), &self.false_branch.lift(cutoff, amount)).into();
    }
    
    fn fmt_with_context(&self, context: &Vec<&str>, head: bool, tail: bool) -> String {
        let guard = self.guard.fmt_with_context(context, false, false);
        let true_branch = self.true_branch.fmt_with_context(context, false, false);
        let false_branch = self.false_branch.fmt_with_context(context, false, true);

        // Syntax is unambiguous if this appears in non-head without parens because it binds greedily to the guard term, but this can be difficult to parse visually.
        // Similar notes apply to other constant function terms.
        if !head || !tail {
            return format!("(if {} then {} else {})", guard, true_branch, false_branch);
        }
        else {
            return format!("if {} then {} else {}", guard, true_branch, false_branch);
        }
    }
}

impl Display for TermIf {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.fmt_with_context(&vec![], true, true))
    }
}

impl TermIf {
    fn new(guard: &impl TTerm, true_branch: &impl TTerm, false_branch: &impl TTerm) -> TermIf {
        return TermIf { guard: Box::new(guard.clone().into()), true_branch: Box::new(true_branch.clone().into()), false_branch: Box::new(false_branch.clone().into()) };
    }
}

impl TTerm for TermZero {
    fn is_numeric(&self) -> bool {
        return true;
    }

    fn step(&self) -> Term {
        return self.clone().into();
    }
    
    fn substitute(&self, _index: usize, _arg: &Term) -> Term {
        return self.clone().into();
    }
    
    fn lift(&self, _cutoff: usize, _amount: isize) -> Term {
        return self.clone().into();
    }
    
    fn fmt_with_context(&self, _context: &Vec<&str>, _head: bool, _tail: bool) -> String {
        return "0".to_owned();
    }
}

impl Display for TermZero {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.fmt_with_context(&vec![], true, true))
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

    fn step(&self) -> Term {
        return TermSucc::new(&self.arg.step()).into();
    }
    
    fn substitute(&self, index: usize, arg: &Term) -> Term {
        return TermSucc::new(&self.arg.substitute(index, arg)).into();
    }
    
    fn lift(&self, cutoff: usize, amount: isize) -> Term {
        return TermSucc::new(&self.arg.lift(cutoff, amount)).into();
    }
    
    fn fmt_with_context(&self, context: &Vec<&str>, head: bool, tail: bool) -> String {
        if self.is_numeric() {
            let mut value: u64 = 1;
            let mut term: &Term = self.arg.as_ref();

            while let Term::Succ(t) = term {
                value += 1;

                term = t.arg.as_ref();
            }

            return format!("{}", value);
        }

        let arg = self.arg.fmt_with_context(context, false, true);

        if !head || !tail {
            return format!("(succ {})", arg);
        }
        else {
            return format!("succ {}", arg);
        }
    }
}

impl Display for TermSucc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.fmt_with_context(&vec![], true, true))
    }
}

impl TermSucc {
    fn new(arg: &impl TTerm) -> TermSucc {
        return TermSucc { arg: Box::new(arg.clone().into()) };
    }
}

impl TTerm for TermPred {
    fn is_numeric(&self) -> bool {
        return false;
    }

    fn step(&self) -> Term {
        return match self.arg.as_ref() {
            Term::Zero(t) => t.clone().into(),
            Term::Succ(t) => if t.arg.is_numeric() {
                t.arg.as_ref().clone()
            } else {
                TermPred::new(&self.arg.step()).into()
            },
            t => TermPred::new(&t.step()).into()
        };
    }
    
    fn substitute(&self, index: usize, arg: &Term) -> Term {
        return TermPred::new(&self.arg.substitute(index, arg)).into();
    }
    
    fn lift(&self, cutoff: usize, amount: isize) -> Term {
        return TermPred::new(&self.arg.lift(cutoff, amount)).into();
    }
    
    fn fmt_with_context(&self, context: &Vec<&str>, head: bool, tail: bool) -> String {
        let arg = self.arg.fmt_with_context(context, false, true);

        if !head || !tail {
            return format!("(pred {})", arg);
        }
        else {
            return format!("pred {}", arg);
        }
    }
}

impl Display for TermPred {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.fmt_with_context(&vec![], true, true))
    }
}

impl TermPred {
    fn new(arg: &impl TTerm) -> TermPred {
        return TermPred { arg: Box::new(arg.clone().into()) };
    }
}

impl TTerm for TermIsZero {
    fn is_numeric(&self) -> bool {
        return false;
    }
    
    fn step(&self) -> Term {
        return match self.arg.as_ref() {
            Term::Zero(_) => TermTrue::new().into(),
            Term::Succ(t) => if t.is_numeric() {
                TermFalse::new().into()
            } else {
                TermIsZero::new(&self.arg.step()).into()
            },
            t => TermIsZero::new(&t.step()).into()
        };
    }
    
    fn substitute(&self, index: usize, arg: &Term) -> Term {
        return TermIsZero::new(&self.arg.substitute(index, arg)).into();
    }
    
    fn lift(&self, cutoff: usize, amount: isize) -> Term {
        return TermIsZero::new(&self.arg.lift(cutoff, amount)).into();
    }
    
    fn fmt_with_context(&self, context: &Vec<&str>, head: bool, tail: bool) -> String {
        let arg = self.arg.fmt_with_context(context, false, true);

        if !head || !tail {
            return format!("(iszero {})", arg);
        }
        else {
            return format!("iszero {}", arg);
        }
    }
}

impl Display for TermIsZero {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.fmt_with_context(&vec![], true, true))
    }
}

impl TermIsZero {
    fn new(arg: &impl TTerm) -> TermIsZero {
        return TermIsZero { arg: Box::new(arg.clone().into()) };
    }
}

#[derive(Clone, Eq, PartialEq)]
struct TermVar {
    index: usize
}

impl TTerm for TermVar {
    fn is_numeric(&self) -> bool {
        return false;
    }

    fn step(&self) -> Term {
        return self.clone().into();
    }
    
    fn substitute(&self, index: usize, arg: &Term) -> Term {
        if index == self.index {
            return arg.clone();
        } else {
            return self.clone().into();
        }
    }
    
    fn lift(&self, cutoff: usize, amount: isize) -> Term {
        if self.index >= cutoff {
            if isize::try_from(self.index).unwrap() + amount < 0 {
                panic!("Tried to lower variable from {} to {}", self.index, amount);
            }

            return TermVar::new((isize::try_from(self.index).unwrap() + amount).try_into().unwrap()).into();
        }

        return self.clone().into();
    }
    
    fn fmt_with_context(&self, context: &Vec<&str>, _head: bool, _tail: bool) -> String {
        if self.index < context.len() {
            return context[self.index].to_owned();
        }
        else {
            return format!("<var: {}>", self.index);
        }
    }
}

impl Display for TermVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
       write!(f, "{}", self.fmt_with_context(&vec![], true, true))
    }
}

impl TermVar {
    fn new(index: usize) -> TermVar {
        return TermVar { index };
    }
}

#[derive(Clone, Eq, PartialEq)]
struct TermApp {
    f: Box<Term>,
    arg: Box<Term>
}

impl TTerm for TermApp {
    fn is_numeric(&self) -> bool {
        return false;
    }
    
    fn step(&self) -> Term {
        return match self.f.as_ref() {
            Term::Lambda(t) => t.body.substitute(0, &self.arg.lift(0, 1)).lift(0, -1),
            t => TermApp::new(&t.step(), &self.arg.as_ref().clone()).into()
        }
    }
    
    fn substitute(&self, index: usize, arg: &Term) -> Term {
        return TermApp::new(&self.f.substitute(index, arg), &self.arg.substitute(index, arg)).into();
    }
    
    fn lift(&self, cutoff: usize, amount: isize) -> Term {
        return TermApp::new(&self.f.lift(cutoff, amount), &self.arg.lift(cutoff, amount)).into();
    }
    
    fn fmt_with_context(&self, context: &Vec<&str>, head: bool, _tail: bool) -> String {
        let f = self.f.fmt_with_context(context, true, false);

        // Syntax is unambiguous if arg is considered in tail position for !head || tail, but lambdas in tail position are awkward to read particularly if they have an application head
        let arg = self.arg.fmt_with_context(context, false, false);
        
        if head {
            return format!("{} {}", f, arg);
        }
        else {
            return format!("({} {})", f, arg);
        }
    }
}

impl Display for TermApp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.fmt_with_context(&vec![], true, true))
    }
}

impl TermApp {
    fn new(f: &impl TTerm, arg: &impl TTerm) -> TermApp {
        return TermApp { f: Box::new(f.clone().into()), arg: Box::new(arg.clone().into()) };
    }
}

#[derive(Clone, Eq, PartialEq)]
struct TermLambda {
    name: String,
    body: Box<Term>
}

impl TTerm for TermLambda {
    fn is_numeric(&self) -> bool {
        return false;
    }

    fn step(&self) -> Term {
        return self.clone().into();
    }

    fn substitute(&self, index: usize, arg: &Term) -> Term {
        return TermLambda::new(&self.name, &self.body.substitute(index + 1, &arg.lift(0, 1))).into();
    }
    
    fn lift(&self, cutoff: usize, amount: isize) -> Term {
        return TermLambda::new(&self.name, &self.body.lift(cutoff + 1, amount)).into();
    }

    fn fmt_with_context(&self, context: &Vec<&str>, _head: bool, tail: bool) -> String {
        let mut name = self.name.clone();
        let mut i: isize = 0;
        
        while context.contains(&&*name) {
            name = self.name.clone() + &i.to_string();
            i += 1;
        }

        let mut new_context: Vec<&str> = vec![&name];
        new_context.extend(context);

        let body = self.body.fmt_with_context(&new_context, true, true);

        if tail {
            return format!("λ{}.{}", name, body);
        }
        else {
            return format!("(λ{}.{})", name, body);
        }
    }
}

impl Display for TermLambda {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.fmt_with_context(&vec![], true, true))
    }
}

impl TermLambda {
    fn new(name: &str, body: &impl TTerm) -> TermLambda {
        return TermLambda { name: name.to_owned(), body: Box::new(body.clone().into()) };
    }
}

fn main() {
    let ex1 = TermIf::new(&TermFalse::new(), &TermZero::new(), &TermSucc::new(&TermZero::new()));
    let ex2 = TermIsZero::new(&TermPred::new(&TermSucc::new(&TermZero::new())));

    let tru = TermLambda::new("t", &TermLambda::new("f", &TermVar::new(1)));
    let fls = TermLambda::new("t", &TermLambda::new("f", &TermVar::new(0)));
    let test = TermLambda::new("l", &TermLambda::new("m", &TermLambda::new("n", &TermApp::new(&TermApp::new(&TermVar::new(2), &TermVar::new(1)), &TermVar::new(0)))));

    let ex3 = TermApp::new(&TermApp::new(&TermApp::new(&test, &tru), &TermVar::new(0)), &TermVar::new(1));

    let and = TermLambda::new("b", &TermLambda::new("c", &TermApp::new(&TermApp::new(&TermVar::new(1), &TermVar::new(0)), &fls)));

    let ex4 = TermApp::new(&TermApp::new(&and, &tru), &tru);
    let ex5 = TermApp::new(&TermApp::new(&and, &tru), &fls);

    let pair = TermLambda::new("f", &TermLambda::new("s", &TermLambda::new("b", &TermApp::new(&TermApp::new(&TermVar::new(0), &TermVar::new(2)), &TermVar::new(1)))));
    let fst = TermLambda::new("p", &TermApp::new(&TermVar::new(0), &tru));
    let snd= TermLambda::new("p", &TermApp::new(&TermVar::new(0), &fls));

    let ex6 = TermApp::new(&fst, &TermApp::new(&TermApp::new(&pair, &TermVar::new(0)), &TermVar::new(1)));

    let c_0 = TermLambda::new("s", &TermLambda::new("z", &TermVar::new(0)));
    let c_1 = TermLambda::new("s", &TermLambda::new("z", &TermApp::new(&TermVar::new(1), &TermVar::new(0))));
    let c_2 = TermLambda::new("s", &TermLambda::new("z", &TermApp::new(&TermVar::new(1), &TermApp::new(&TermVar::new(1), &TermVar::new(0)))));
    let c_3 = TermLambda::new("s", &TermLambda::new("z", &TermApp::new(&TermVar::new(1), &TermApp::new(&TermVar::new(1), &TermApp::new(&TermVar::new(1), &TermVar::new(0))))));
    let c_4 = TermLambda::new("s", &TermLambda::new("z", &TermApp::new(&TermVar::new(1), &TermApp::new(&TermVar::new(1), &TermApp::new(&TermVar::new(1), &TermApp::new(&TermVar::new(1), &TermVar::new(0)))))));
    
    let scc = TermLambda::new("n", &TermLambda::new("s", &TermLambda::new("z", &TermApp::new(&TermVar::new(1), &TermApp::new(&TermApp::new(&TermVar::new(2), &TermVar::new(1)), &TermVar::new(0))))));
    let plus = TermLambda::new("m", &TermLambda::new("n", &TermLambda::new("s", &TermLambda::new("z", &TermApp::new(&TermApp::new(&TermVar::new(3), &TermVar::new(1)), &TermApp::new(&TermApp::new(&TermVar::new(2), &TermVar::new(1)), &TermVar::new(0)))))));
    let times = TermLambda::new("m", &TermLambda::new("n", &TermApp::new(&TermApp::new(&TermVar::new(1), &TermApp::new(&plus, &TermVar::new(0))), &c_0)));
    let iszro = TermLambda::new("m", &TermApp::new(&TermApp::new(&TermVar::new(0), &TermLambda::new("x", &fls)), &tru));

    let ex7 = TermApp::new(&iszro, &c_1);
    let ex8 = TermApp::new(&iszro, &TermApp::new(&TermApp::new(&times, &c_0), &c_2));

    let zz = TermApp::new(&TermApp::new(&pair, &c_0), &c_0);
    let ss = TermLambda::new("p", &TermApp::new(&TermApp::new(&pair, &TermApp::new(&snd, &TermVar::new(0))), &TermApp::new(&scc, &TermApp::new(&snd, &TermVar::new(0)))));
    let prd = TermLambda::new("m", &TermApp::new(&fst, &TermApp::new(&TermApp::new(&TermVar::new(0), &ss), &zz)));

    let equal = TermLambda::new("m", &TermLambda::new("n", &TermApp::new(&TermApp::new(&and, &TermApp::new(&iszro, &TermApp::new(&TermApp::new(&TermVar::new(1), &prd), &TermVar::new(0)))), &TermApp::new(&iszro, &TermApp::new(&TermApp::new(&TermVar::new(0), &prd), &TermVar::new(1))))));

    let ex9 = TermApp::new(&TermApp::new(&equal, &c_3), &c_3);
    let ex10 = TermApp::new(&TermApp::new(&equal, &c_3), &c_2);

    let realnat = TermLambda::new("m", &TermApp::new(&TermApp::new(&TermVar::new(0), &TermLambda::new("x", &TermSucc::new(&TermVar::new(0)))), &TermZero::new()));

    let ex11 = TermApp::new(&scc, &c_1);
    let ex12 = TermApp::new(&TermApp::new(&times, &c_2), &c_2);
    let ex13 = TermApp::new(&TermApp::new(&equal, &c_4), &TermApp::new(&TermApp::new(&times, &c_2), &c_2));
    let ex14 = TermApp::new(&realnat, &TermApp::new(&TermApp::new(&times, &c_2), &c_2));

    let y = TermLambda::new("f", &TermApp::new(&TermLambda::new("x", &TermApp::new(&TermVar::new(1), &TermApp::new(&TermVar::new(0), &TermVar::new(0)))), &TermLambda::new("x", &TermApp::new(&TermVar::new(1), &TermApp::new(&TermVar::new(0), &TermVar::new(0))))));
    let g = TermLambda::new("fct", &TermLambda::new("n", &TermApp::new(&TermApp::new(&TermApp::new(&test, &TermApp::new(&iszro, &TermVar::new(0))), &c_1), &TermApp::new(&TermApp::new(&times, &TermVar::new(0)), &TermApp::new(&TermVar::new(1), &TermApp::new(&prd, &TermVar::new(0)))))));
    let factorial = TermApp::new(&y, &g);

    let ex15 = TermApp::new(&factorial, &c_3);
    let ex16 = TermApp::new(&realnat, &TermApp::new(&factorial, &c_3));

    println!("{} = {}", ex1, ex1.normalize());
    println!("{} = {}", ex2, ex2.normalize());
    println!("tru = {}", tru);
    println!("fls = {}", fls);
    println!("test = {}", test);
    println!("(test tru <var: 0> <var: 1>) = {}\n -> {}", ex3, ex3.normalize());
    println!("and = {}", and);
    println!("(and tru tru) = {}\n -> {}", ex4, ex4.normalize());
    println!("(and tru fls) = {}\n -> {}", ex5, ex5.normalize());
    println!("pair = {}", pair);
    println!("fst = {}", fst);
    println!("snd = {}", snd);
    println!("(fst (pair <var: 0> <var: 1>)) = {}\n -> {}", ex6, ex6.normalize());
    println!("c_0 = {}", c_0);
    println!("c_1 = {}", c_1);
    println!("c_2 = {}", c_2);
    println!("c_3 = {}", c_3);
    println!("c_4 = {}", c_4);
    println!("scc = {}", scc);
    println!("plus = {}", plus);
    println!("times = {}", times);
    println!("iszro = {}", iszro);
    println!("(iszro c_1) = {}\n -> {}", ex7, ex7.normalize());
    println!("(iszro (times c_0 c_2)) = {}\n -> {}", ex8, ex8.normalize());
    println!("zz = {}", zz);
    println!("ss = {}", ss);
    println!("prd = {}", prd);
    println!("equal = {}", equal);
    println!("(equal c_3 c_3) = {}\n -> {}", ex9, ex9.normalize());
    println!("(equal c_3 c_2) = {}\n -> {}", ex10, ex10.normalize());
    println!("realnat = {}", realnat);
    println!("(scc c_1) = {}\n -> {}", ex11, ex11.normalize());
    println!("(times c_2 c_2) = {}\n -> {}", ex12, ex12.normalize());
    println!("(equal c_4 (times c_2 c_2)) = {}\n -> {}", ex13, ex13.normalize());
    println!("(realnat (times c_2 c_2)) = {}\n -> {}", ex14, ex14.normalize());
    println!("y = {}\n -> {}", y, y.normalize());
    println!("g = {}", g);
    println!("factorial = {}\n -> {}", factorial, factorial.normalize());
    println!("(factorial c_3) = {}\n -> {}", ex15, ex15.normalize());
    println!("(realnat (factorial c_3)) = {}\n -> {}", ex16, ex16.normalize());
}
