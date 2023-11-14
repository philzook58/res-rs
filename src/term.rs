use std::collections::{HashMap, HashSet};
use symbol_table::GlobalSymbol;

type Var = GlobalSymbol;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Term {
    Var(Var),
    Const(GlobalSymbol, Vec<Term>),
}

type Subst = HashMap<Var, Term>;
impl Term {
    pub fn var(name: &str) -> Term {
        Term::Var(name.into())
    }

    // Helper function to create a compound term
    pub fn const_(f: &str, terms: Vec<Term>) -> Term {
        Term::Const(f.into(), terms)
    }

    // Collect all variables in the term
    pub fn free_vars_aux(&self, res: &mut HashSet<Var>) {
        match self {
            Term::Var(var) => {
                res.insert(*var);
            }
            Term::Const(_, args) => {
                for arg in args {
                    arg.free_vars_aux(res);
                }
            }
        }
    }
    pub fn free_vars(&self) -> HashSet<Var> {
        let mut vars = Default::default();
        self.free_vars_aux(&mut vars);
        vars
    }

    pub fn subst_var(&mut self, var: Var, term: &Term) {
        match self {
            Term::Var(v) => {
                if *v == var {
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

    pub fn apply_subst(&mut self, subst: &Subst) {
        match self {
            Term::Var(v) => {
                if let Some(t) = subst.get(v) {
                    *self = t.clone();
                }
            }
            Term::Const(_, args) => {
                for arg in args {
                    arg.apply_subst(subst);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Term::*;

    #[test]
    fn test_subst() {
        let x = "x".into();
        let mut t = Term::var("x");
        t.subst_var(x, &Term::var("y"));
        assert_eq!(t, Term::var("y"));

        let mut t = Term::const_("foo", vec![Term::var("x"), Term::var("y")]);
        t.subst_var("x".into(), &Term::var("y"));
        assert_eq!(t, Term::const_("foo", vec![Term::var("y"), Term::var("y")]));
    }
}
