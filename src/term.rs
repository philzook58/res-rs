use std::collections::{HashMap, HashSet};
use symbol_table::GlobalSymbol;

type Var = GlobalSymbol;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Term {
    Var(Var),
    Const(GlobalSymbol, Vec<Term>),
}
type Sig = HashMap<GlobalSymbol, usize>;
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

    // Check if a variable occurs in a term
    fn occurs_check(&self, x: Var) -> bool {
        match self {
            Term::Var(var) => x == *var,
            Term::Const(_, args) => args.iter().any(|arg| arg.occurs_check(x)),
        }
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

    // Unify two lists of terms with a common substitution
    fn mgu_term_list(l1: &mut Vec<Term>, l2: &mut Vec<Term>, subst: &mut Subst) -> Option<()> {
        assert_eq!(l1.len(), l2.len());
        while let (Some(t1), Some(t2)) = (l1.pop(), l2.pop()) {
            match &t1 {
                Term::Var(x) => {
                    if Term::Var(x.clone()) == t2 {
                        continue;
                    }
                    if t2.occurs_check(*x) {
                        return None; // Occurs check failed
                    }
                    // Apply the new binding to the remaining terms
                    for term in l1.iter_mut() {
                        term.subst_var(*x, &t2)
                    }
                    // Apply the new binding to the remaining terms
                    for term in l2.iter_mut() {
                        term.subst_var(*x, &t2)
                    }

                    // Add the new binding to the substitution
                    subst.insert(x.clone(), t2);
                }
                Term::Const(f1, args1) => match &t2 {
                    Term::Var(x) => {
                        if t1.occurs_check(*x) {
                            return None; // Occurs check failed
                        }
                        // Apply the new binding to the remaining terms
                        for term in l1.iter_mut() {
                            term.subst_var(*x, &t1)
                        }
                        // Apply the new binding to the remaining terms
                        for term in l2.iter_mut() {
                            term.subst_var(*x, &t1)
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

        Self::mgu_term_list(&mut l1, &mut l2, &mut subst).map(|_| subst)
    }

    fn match_(&self, t: &Term) -> Option<Subst> {
        Self::mgu(self, t)
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

    #[test]
    fn test_unify() {
        let mut t1 = Term::const_("foo", vec![Term::var("x"), Term::var("y")]);
        let mut t2 = Term::const_("foo", vec![Term::var("y"), Term::var("x")]);
        let subst = Term::mgu(&t1, &t2).unwrap();
        t1.apply_subst(&subst);
        t2.apply_subst(&subst);
        assert_eq!(subst.len(), 1);
        assert_eq!(subst[&"y".into()], Term::var("x"));
        assert_eq!(t1, t2);

        let mut t1 = Term::const_("foo", vec![Term::var("x"), Term::var("y")]);
        let mut t2 = Term::const_("foo", vec![Term::var("y"), Term::var("z")]);
        let subst = Term::mgu(&t1, &t2).unwrap();
        t1.apply_subst(&subst);
        t2.apply_subst(&subst);
        assert_eq!(subst.len(), 2);
        assert_eq!(t1, t2);

        let t1 = Term::const_("foo", vec![Term::var("x"), Term::var("y")]);
        let t2 = Term::const_("bar", vec![Term::var("y"), Term::var("x")]);
        assert_eq!(Term::mgu(&t1, &t2), None);

        // occurs check
        let t1 = Term::const_("foo", vec![Term::var("x")]);
        let t2 = Term::var("x");
        assert_eq!(Term::mgu(&t1, &t2), None);

        // self unification
        let t1 = Term::const_("foo", vec![Term::var("x")]);
        assert_eq!(Term::mgu(&t1, &t1), Some(Default::default()));
    }
}
