// https://www21.in.tum.de/~nipkow/TRaAT/programs/trs.ML
type VName = (String, i32);

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Term {
    V(VName),
    T(String, Vec<Term>),
}

// indom: Checks if a vname is in a substitution
pub fn indom(x: &VName, s: &[(VName, Term)]) -> bool {
    s.iter().any(|(y, _)| x == y)
}

// app: Applies a substitution to a vname
pub fn app(s: &[(VName, Term)], x: &VName) -> Term {
    for (y, t) in s {
        if x == y {
            return t.clone();
        }
    }
    panic!("vname not found in substitution");
}

// lift: Applies a substitution to a term
pub fn lift(s: &[(VName, Term)], term: &Term) -> Term {
    match term {
        Term::V(x) => {
            if indom(x, s) {
                app(s, x)
            } else {
                term.clone()
            }
        }
        Term::T(f, ts) => Term::T(f.clone(), ts.iter().map(|t| lift(s, t)).collect()),
    }
}

// occurs: Checks if a vname occurs in a term
pub fn occurs(x: &VName, term: &Term) -> bool {
    match term {
        Term::V(y) => x == y,
        Term::T(_, ts) => ts.iter().any(|t| occurs(x, t)),
    }
}

pub fn solve(mut eqs: Vec<(Term, Term)>, mut s: Vec<(VName, Term)>) -> Option<Vec<(VName, Term)>> {
    while let Some((lhs, rhs)) = eqs.pop() {
        match (lhs, rhs) {
            (Term::V(x), t) | (t, Term::V(x)) => {
                if Term::V(x.clone()) != t {
                    elim(&x, &t, &mut eqs, &mut s)?;
                }
            }
            (Term::T(f, ts), Term::T(g, us)) if f == g => {
                eqs.extend(ts.into_iter().zip(us));
            }
            _ => return None,
        }
    }
    Some(s)
}

pub fn elim(
    x: &VName,
    t: &Term,
    eqs: &mut Vec<(Term, Term)>,
    s: &mut Vec<(VName, Term)>,
) -> Option<()> {
    if occurs(x, t) {
        None
    } else {
        let xt_subst = vec![(x.clone(), lift(s, t))];
        eqs.iter_mut()
            .for_each(|eq| *eq = (lift(&xt_subst, &eq.0), lift(&xt_subst, &eq.1)));
        s.iter_mut()
            .for_each(|sub| *sub = (sub.0.clone(), lift(&xt_subst, &sub.1)));
        s.push((x.clone(), t.clone()));
        Some(())
    }
}

// unify: Tries to find a unifying substitution for two terms
pub fn unify(t1: &Term, t2: &Term) -> Option<Vec<(VName, Term)>> {
    solve(vec![(t1.clone(), t2.clone())], vec![])
}

// matchs: Tries to match two terms under a substitution
pub fn matchs(mut eqs: Vec<(Term, Term)>, mut s: Vec<(VName, Term)>) -> Option<Vec<(VName, Term)>> {
    while let Some((lhs, rhs)) = eqs.pop() {
        match (&lhs, &rhs) {
            (Term::V(x), t) => {
                if indom(x, &s) {
                    if app(&s, x) == *t {
                        continue;
                    } else {
                        return None;
                    }
                } else {
                    s.push((x.clone(), t.clone()));
                }
            }
            (Term::T(f, ts), Term::T(g, us)) if f == g => {
                eqs.extend(ts.iter().cloned().zip(us.iter().cloned()));
            }
            _ => return None,
        }
    }
    Some(s)
}

// match_term: Wrapper for matchs to match a single term pair
pub fn match_term(pat: &Term, obj: &Term) -> Option<Vec<(VName, Term)>> {
    matchs(vec![(pat.clone(), obj.clone())], vec![])
}

const NORM: &str = "normal form";

// rewrite: Attempts to rewrite a term using a list of rewrite rules
pub fn rewrite(rules: &[(Term, Term)], term: &Term) -> Option<Term> {
    rules
        .iter()
        .find_map(|(l, r)| match_term(l, term).map(|subst| lift(&subst, r)))
        .or_else(|| None)
}

// norm: Normalizes a term by repeatedly applying rewrite until no more rules can be applied
pub fn norm(rules: &[(Term, Term)], term: &Term) -> Term {
    let mut current_term = term.clone();
    while let Some(new_term) = rewrite(rules, &current_term) {
        if new_term == current_term {
            break;
        }
        current_term = new_term;
    }
    current_term
}

#[cfg(test)]
mod tests {
    use super::*;

    fn var(name: &str, id: i32) -> Term {
        Term::V((name.to_string(), id))
    }

    // Helper function to create a compound term
    fn term(f: &str, terms: Vec<Term>) -> Term {
        Term::T(f.to_string(), terms)
    }

    #[test]
    fn test_unify_simple() {
        let x = var("x", 0);
        let a = term("a", vec![]);
        let b = term("b", vec![]);

        assert_eq!(unify(&x, &a), Some(vec![(("x".to_string(), 0), a.clone())]));
        assert_eq!(unify(&a, &b), None);
    }

    #[test]
    fn test_match_simple() {
        let x = var("x", 0);
        let a = term("a", vec![]);
        let b = term("b", vec![]);

        assert_eq!(
            match_term(&x, &a),
            Some(vec![(("x".to_string(), 0), a.clone())])
        );
        assert_eq!(match_term(&a, &b), None);
    }

    #[test]
    fn test_rewrite_simple() {
        let a = term("a", vec![]);
        let b = term("b", vec![]);
        let rules = vec![(a.clone(), b.clone())];

        assert_eq!(rewrite(&rules, &a), Some(b.clone()));
        assert_eq!(rewrite(&rules, &b), None);
    }

    #[test]
    fn test_norm_simple() {
        let a = term("a", vec![]);
        let b = term("b", vec![]);
        let c = term("c", vec![]);
        let rules = vec![(a.clone(), b.clone()), (b.clone(), c.clone())];

        assert_eq!(norm(&rules, &a), c);
        assert_eq!(norm(&rules, &b), c);
        assert_eq!(norm(&rules, &c), c);
    }

    // Tests with more complex terms and substitutions

    #[test]
    fn test_unify_complex() {
        let x = var("x", 1);
        let y = var("y", 2);
        let f_x = term("f", vec![x.clone()]);
        let f_a = term("f", vec![term("a", vec![])]);
        let g_y = term("g", vec![y.clone()]);

        // x unifies with f(a)
        assert_eq!(
            unify(&x, &f_a),
            Some(vec![(("x".to_string(), 1), f_a.clone())])
        );

        // f(x) unifies with f(a)
        assert_eq!(
            unify(&f_x, &f_a),
            Some(vec![(("x".to_string(), 1), term("a", vec![]))])
        );

        // g(y) does not unify with f(a)
        assert_eq!(unify(&g_y, &f_a), None);
    }

    #[test]
    fn test_match_complex() {
        let x = var("x", 1);
        let y = var("y", 2);
        let f_x_y = term("f", vec![x.clone(), y.clone()]);
        let f_a_b = term("f", vec![term("a", vec![]), term("b", vec![])]);

        // Matching f(x, y) with f(a, b) should succeed
        assert_eq!(
            match_term(&f_x_y, &f_a_b),
            Some(vec![
                (("y".to_string(), 2), term("b", vec![])),
                (("x".to_string(), 1), term("a", vec![]))
            ])
        );

        // Matching f(a, b) with f(x, y) should fail since we cannot match constants to variables
        assert_eq!(match_term(&f_a_b, &f_x_y), None);
    }

    #[test]
    fn test_rewrite_complex() {
        let x = var("x", 1);
        let f_x = term("f", vec![x.clone()]);
        let a = term("a", vec![]);
        let b = term("b", vec![]);
        let f_a = term("f", vec![a.clone()]);
        let rules = vec![(f_x.clone(), b.clone())];

        // f(a) rewrites to b using the rule f(x) -> b
        assert_eq!(rewrite(&rules, &f_a), Some(b.clone()));

        // a does not rewrite to anything since there is no matching rule
        assert_eq!(rewrite(&rules, &a), None);
    }

    #[test]
    fn test_norm_complex() {
        let x = var("x", 1);
        let f_x = term("f", vec![x.clone()]);
        let g_x = term("g", vec![x.clone()]);
        let a = term("a", vec![]);
        let b = term("b", vec![]);
        let f_a = term("f", vec![a.clone()]);
        let g_b = term("g", vec![b.clone()]);
        let rules = vec![
            (f_x.clone(), g_x.clone()), // f(x) -> g(x)
            (g_x.clone(), a.clone()),   // g(x) -> a
        ];

        // f(a) normalizes to a using the rules
        assert_eq!(norm(&rules, &f_a), a);

        // g(b) normalizes to a using the rules
        assert_eq!(norm(&rules, &g_b), a);
    }

    // Add even more complex test cases here...
}
