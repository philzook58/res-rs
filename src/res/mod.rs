use std::collections::{HashMap, HashSet};

type Variable = String;
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Term {
    Var(Variable),
    Const(String, Vec<Term>),
}

impl Term {
    // Collect all variables in the term
    fn collect_vars(&self, res: &mut HashSet<String>) {
        match self {
            Term::Var(var) => {
                res.insert(var.clone());
            }
            Term::Const(_, args) => {
                for arg in args {
                    arg.collect_vars(res);
                }
            }
        }
    }

    fn subst_var(&mut self, var: &str, term: &Term) {
        match self {
            Term::Var(v) => {
                if v == var {
                    *self = term.clone();
                }
            }
            Term::Const(_, args) => {
                for arg in args {
                    arg.subst_var(var, term);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn var(name: &str) -> Term {
        Term::Var(name.to_string())
    }

    // Helper function to create a compound term
    fn const_(f: &str, terms: Vec<Term>) -> Term {
        Term::Const(f.to_string(), terms)
    }
    #[test]
    fn test_subst() {
        let mut t = var("x");
        t.subst_var("x", &var("y"));
        assert_eq!(t, var("y"));

        let mut t = const_("foo", vec![var("x"), var("y")]);
        t.subst_var("x", &var("y"));
        assert_eq!(t, const_("foo", vec![var("y"), var("y")]));
    }
}

type Subst = HashMap<String, Term>;

fn apply(subst: &Subst, term: &Term) -> Term {
    match term {
        Term::Var(var) => subst.get(var).unwrap_or(term).clone(),
        Term::Const(func, args) => Term::Const(
            func.clone(),
            args.iter().map(|arg| apply(subst, arg)).collect(),
        ),
    }
}

// Check if a variable occurs in a term
fn occurs_check(x: &str, t: &Term) -> bool {
    match t {
        Term::Var(var) => x == var,
        Term::Const(_, args) => args.iter().any(|arg| occurs_check(x, arg)),
    }
}

// Unify two lists of terms with a common substitution
fn mgu_term_list(l1: &mut Vec<Term>, l2: &mut Vec<Term>, subst: &mut Subst) -> Option<()> {
    assert_eq!(l1.len(), l2.len());
    while let (Some(t1), Some(t2)) = (l1.pop(), l2.pop()) {
        match &t1 {
            Term::Var(x) => {
                if Term::Var(x.clone()) == t2 {
                    continue;
                }
                if occurs_check(&x, &t2) {
                    return None; // Occurs check failed
                }
                // Apply the new binding to the remaining terms
                for term in l1.iter_mut() {
                    term.subst_var(&x, &t2)
                }
                // Apply the new binding to the remaining terms
                for term in l2.iter_mut() {
                    term.subst_var(&x, &t2)
                }

                // Add the new binding to the substitution
                subst.insert(x.clone(), t2);
            }
            Term::Const(f1, args1) => match &t2 {
                Term::Var(x) => {
                    if occurs_check(&x, &t1) {
                        return None; // Occurs check failed
                    }
                    // Apply the new binding to the remaining terms
                    for term in l1.iter_mut() {
                        term.subst_var(&x, &t1)
                    }
                    // Apply the new binding to the remaining terms
                    for term in l2.iter_mut() {
                        term.subst_var(&x, &t1)
                    }
                    // Add the new binding to the substitution
                    subst.insert(x.clone(), t1);
                }
                Term::Const(f2, args2) => {
                    if f1 != f2 || args1.len() != args2.len() {
                        return None;
                    }
                    l1.extend(args1.iter().cloned());
                    l2.extend(args2.iter().cloned());
                }
            },
        }
    }
    Some(())
}

// Try to unify two terms
fn mgu(t1: &Term, t2: &Term) -> Option<Subst> {
    let mut subst = HashMap::new();
    let mut l1 = vec![t1.clone()];
    let mut l2 = vec![t2.clone()];

    mgu_term_list(&mut l1, &mut l2, &mut subst).map(|_| subst)
}

type Atom = Term;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Literal {
    atom: Atom,
    negative: bool,
    inference_lit: bool,
}
impl Literal {
    fn is_prop_false(&self) -> bool {
        self.atom == Term::Const("false".to_string(), vec![]) && self.negative == true
            || self.atom == Term::Const("true".to_string(), vec![]) && self.negative == false
    }
    fn collect_vars(&self, res: &mut HashSet<String>) {
        self.atom.collect_vars(res);
    }
    fn instantiate(&self, subst: &Subst) -> Literal {
        Literal {
            atom: apply(subst, &self.atom),
            negative: self.negative,
            inference_lit: self.inference_lit,
        }
    }
    fn is_inference_lit(&self) -> bool {
        self.inference_lit
    }
}

#[derive(Debug, Clone)]
pub struct Clause {
    literals: Vec<Literal>,
    name: Option<String>,
}

impl Clause {
    /// Initialize the clause.
    pub fn new(literals: Vec<Literal>, name: Option<String>) -> Self {
        let filtered_literals: Vec<Literal> = literals
            .into_iter()
            .filter(|l| !l.is_prop_false())
            .collect();
        Clause {
            literals: filtered_literals,
            //clause_type: clause_type.unwrap_or_else(|| "plain".to_string()),
            name,
        }
    }

    /// Return the number of literals in the clause.
    pub fn len(&self) -> usize {
        self.literals.len()
    }
    pub fn get_literal(&self, index: usize) -> &Literal {
        &self.literals[index]
    }

    /// Return true if the clause is empty.
    pub fn is_empty(&self) -> bool {
        self.literals.is_empty()
    }

    /// Collect all variables in self into the set res and return it.
    pub fn collect_vars(&self) -> HashSet<Variable> {
        let mut res = HashSet::new();
        for lit in &self.literals {
            lit.collect_vars(&mut res);
        }
        res
    }
    fn remove_duplicate_literals(&mut self) {
        let mut seen = HashSet::new();
        self.literals.retain(|lit| seen.insert(lit.clone()));
    }
    /// Return a copy of self with fresh variables.
    pub fn fresh_var_copy(&self) -> Clause {
        // TODO: nonsensical
        let vars = self.collect_vars();
        //let subst = substitutions::fresh_var_subst(&vars);
        let mut subst = HashMap::new();
        for v in vars {
            subst.insert(v.clone(), Term::Var(v.clone()));
        }
        self.instantiate(&subst)
    }

    /// Instantiate a copy of self with the given substitution.
    pub fn instantiate(&self, subst: &Subst) -> Clause {
        let lits = self.literals.iter().map(|l| l.instantiate(subst)).collect();
        Clause::new(lits, self.name.clone())
    }

    fn factor(&self, lit1_index: usize, lit2_index: usize) -> Option<Clause> {
        let l1 = self.get_literal(lit1_index);
        let l2 = self.get_literal(lit2_index);
        if l1.negative != l2.negative {
            return None;
        }
        if let Some(sigma) = mgu(&l1.atom, &l2.atom) {
            let lits: Vec<Literal> = self
                .literals
                .iter()
                .cloned()
                .filter(|l| l != l2)
                .map(|l| l.instantiate(&sigma))
                .collect();
            let mut res = Clause {
                name: None,
                literals: lits,
            };
            res.remove_duplicate_literals();
            // Set derivation for res here
            Some(res)
        } else {
            None
        }
    }
    fn compute_all_factors(&self) -> Vec<Clause> {
        let mut res = Vec::new();
        for i in 0..self.len() {
            for j in (i + 1)..self.len() {
                if self.get_literal(i).inference_lit || self.get_literal(j).inference_lit {
                    if let Some(factor) = self.factor(i, j) {
                        res.push(factor);
                    }
                }
            }
        }
        res
    }
}

fn resolution(
    clause1: &Clause,
    lit1_index: usize,
    clause2: &Clause,
    lit2_index: usize,
) -> Option<Clause> {
    let l1 = clause1.get_literal(lit1_index);
    let l2 = clause2.get_literal(lit2_index);
    if l1.negative == l2.negative {
        return None;
    }
    if let Some(sigma) = mgu(&l1.atom, &l2.atom) {
        let mut lits1: Vec<Literal> = clause1
            .literals
            .iter()
            .cloned()
            .filter(|l| l != l1)
            .map(|l| l.instantiate(&sigma))
            .collect();
        let lits2: Vec<Literal> = clause2
            .literals
            .iter()
            .cloned()
            .filter(|l| l != l2)
            .map(|l| l.instantiate(&sigma))
            .collect();
        lits1.extend(lits2);
        let mut res = Clause {
            name: None,
            literals: lits1,
        };
        res.remove_duplicate_literals();
        // Set derivation for res here
        Some(res)
    } else {
        None
    }
}
use std::fmt;

use std::collections::VecDeque;

// Assuming Clause is defined elsewhere.

#[derive(Debug, Clone)]
pub struct ClauseSet {
    clauses: VecDeque<Clause>,
}

impl ClauseSet {
    /// Initialize the clause set with an optional set of initial clauses.
    pub fn new(clauses: Vec<Clause>) -> Self {
        ClauseSet {
            clauses: VecDeque::from(clauses),
        }
    }

    /// Add a clause to the end of the clause set.
    pub fn add_clause(&mut self, clause: Clause) {
        self.clauses.push_back(clause);
    }

    /// Remove a clause from the front of the clause set and return it.
    pub fn extract_first(&mut self) -> Option<Clause> {
        self.clauses.pop_front()
    }

    pub fn is_empty(&self) -> bool {
        self.clauses.is_empty()
    }
    pub fn iter(&self) -> std::collections::vec_deque::Iter<Clause> {
        self.clauses.iter()
    }
    // Method to get all literals that can potentially be resolved against a given literal
    fn get_resolution_literals(&self, lit: &Literal) -> Vec<(&Clause, usize)> {
        let mut res = Vec::new();
        for (c_index, clause) in self.clauses.iter().enumerate() {
            for (l_index, literal) in clause.literals.iter().enumerate() {
                if literal.negative != lit.negative {
                    if let Some(subst) = mgu(&literal.atom, &lit.atom) {
                        /* some condition to match against `lit` */
                        {
                            res.push((clause, l_index))
                        }
                    }
                }
            }
        }
        res
    }
    fn compute_all_resolvents(&self, clause: &Clause) -> Vec<Clause> {
        /* TODO
        let mut res = Vec::new();
        for lit in 0..clause.len() {
            if clause.get_literal(lit).inference_lit {
                let partners = self.get_resolution_literals(clause.get_literal(lit));
                for (cl2, lit2) in partners {
                    if let Some(resolvent) = resolution(clause, lit, cl2, lit2) {
                        res.push(resolvent);
                    }
                }
            }
        }
        res */
        vec![]
    }
}

struct SimpleProofState {
    unprocessed: ClauseSet,
    processed: ClauseSet,
}

impl SimpleProofState {
    /// Initialize the proof state with a set of clauses.
    fn new(clauses: ClauseSet) -> Self {
        let mut unprocessed = ClauseSet::new(vec![]);
        let processed = ClauseSet::new(vec![]);

        for c in clauses.iter() {
            unprocessed.add_clause(c.clone());
        }

        SimpleProofState {
            unprocessed,
            processed,
        }
    }

    /// Pick a clause from unprocessed and process it. If the empty
    /// clause is found, return it. Otherwise return None.
    fn process_clause(&mut self) -> Option<Clause> {
        if let Some(given_clause) = self.unprocessed.extract_first() {
            let given_clause = given_clause.fresh_var_copy();
            println!("% {:?}", given_clause);
            if given_clause.is_empty() {
                // We have found an explicit contradiction
                return Some(given_clause);
            }

            let mut new = Vec::new();
            let factors = given_clause.compute_all_factors();
            new.extend(factors);
            let resolvents = self.processed.compute_all_resolvents(&given_clause);
            new.extend(resolvents);

            self.processed.add_clause(given_clause);

            for c in new {
                self.unprocessed.add_clause(c);
            }
        }
        None
    }

    /// Main proof procedure. If the clause set is found
    /// unsatisfiable, return the empty clause as a witness. Otherwise
    /// return None.
    fn saturate(&mut self) -> Option<Clause> {
        while !self.unprocessed.is_empty() {
            if let Some(res) = self.process_clause() {
                return Some(res);
            }
        }
        None
    }
}
