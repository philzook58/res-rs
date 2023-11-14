// https://www.cl.cam.ac.uk/~jrh13/atp/OCaml/fol.ml
use std::collections::{HashMap, HashSet};

// Define Term as an enum with variants for Var and Fn
#[derive(Clone)]
enum Term {
    Var(String),
    Fn(String, Vec<Term>),
}

// Define Fol as an enum for logical formulas
enum Fol {
    False,
    True,
    Atom(String, Vec<Term>),
    Not(Box<Fol>),
    And(Box<Fol>, Box<Fol>),
    Or(Box<Fol>, Box<Fol>),
    Imp(Box<Fol>, Box<Fol>),
    Iff(Box<Fol>, Box<Fol>),
    Forall(String, Box<Fol>),
    Exists(String, Box<Fol>),
}

impl Term {
    // Recursively find the free variables in a term
    fn free_vars(&self) -> HashSet<String> {
        match self {
            Term::Var(x) => {
                let mut set = HashSet::new();
                set.insert(x.clone());
                set
            }
            Term::Fn(_, args) => args.iter().flat_map(Term::free_vars).collect(),
        }
    }
}

impl Fol {
    // Recursively find the variables in a formula
    fn vars(&self) -> HashSet<String> {
        match self {
            Fol::False | Fol::True => HashSet::new(),
            Fol::Atom(_, args) => args.iter().flat_map(Term::free_vars).collect(),
            Fol::Not(p) => p.vars(),
            Fol::And(p, q) | Fol::Or(p, q) | Fol::Imp(p, q) | Fol::Iff(p, q) => {
                p.vars().union(&q.vars()).cloned().collect()
            }
            Fol::Forall(x, p) | Fol::Exists(x, p) => {
                let mut vars = p.vars();
                vars.insert(x.clone());
                vars
            }
        }
    }

    // Recursively find the free variables in a formula
    fn free_vars(&self) -> HashSet<String> {
        match self {
            Fol::False | Fol::True => HashSet::new(),
            Fol::Atom(_, args) => args.iter().flat_map(Term::free_vars).collect(),
            Fol::Not(p) => p.free_vars(),
            Fol::And(p, q) | Fol::Or(p, q) | Fol::Imp(p, q) | Fol::Iff(p, q) => {
                p.free_vars().union(&q.free_vars()).cloned().collect()
            }
            Fol::Forall(x, p) | Fol::Exists(x, p) => {
                let mut vars = p.free_vars();
                vars.remove(x);
                vars
            }
        }
    }

    // Apply a substitution to a term
    fn subst(&self, sfn: &HashMap<String, Term>) -> Self {
        match self {
            Fol::Atom(p, args) => {
                Fol::Atom(p.clone(), args.iter().map(|arg| arg.subst(sfn)).collect())
            }
            // Other variants would be handled similarly...
            _ => unimplemented!(),
        }
    }
}

impl Term {
    // Apply a substitution to a term
    fn subst(&self, sfn: &HashMap<String, Term>) -> Self {
        match self {
            Term::Var(x) => sfn.get(x).cloned().unwrap_or_else(|| self.clone()),
            Term::Fn(f, args) => {
                Term::Fn(f.clone(), args.iter().map(|arg| arg.subst(sfn)).collect())
            }
        }
    }
}

// Find a variant of a string that is not in a given set
fn variant(x: &str, vars: &HashSet<String>) -> String {
    let mut new_var = x.to_string();
    while vars.contains(&new_var) {
        new_var.push('\'');
    }
    new_var
}
