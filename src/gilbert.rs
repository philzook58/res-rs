use Term::*;

struct TermIter {
    terms: Vec<Term>,
    sig: Sig,
    n: usize,
}

fn expand_univ(sig: &Sig, &mut univ: Vec<Term>) {
    let fvars = t.free_vars();
    for (name, arity) in sig {
        let mut multi_prod = (0..arity).map(|i| univ.clone()).multi_cartesian_product();
        for args in multi_prod {
            let mut term = Const(name.clone(), args);
            for var in fvars {
                term.subst_var(var, &Var(format!("x{}", n)));
            }
            univ.push(term);
        }
    }
}

/*
We should generate semi-naively. Every arg should contain at least one term that didn't exist last expansion.
term(foo(X,Y)) :- term(X), term(Y).
term(c).
term(d).

Of course, the thing that is annoying here is writing this out for arbitrary runtime sigs.
It isn't hard to write the loops here. And it would be much faster.

Types may help reduce nonsensical terms
Adding rewrite rules or primitive sets, multisets may help.


f(f(g(X,Y)) as a "psuedo constructor"?
Seeding enumeration wth subformula frm the problem
And seeding with (redundant) pseudo constructors (to speed up getting to relevant stuff).

{ true(D) : clause(D,C)}


It almost seems crazy. We're generating them so uniformly that I could explicitly name the number that corresponds to
term t
c1 c2 c3 = 0,1,2,3


term_num(Term) -> usize
const(Sig, Symbol, Vec<usize>) -> usize {
    lookup(siz)
}


*/

/*
uf
terms : Vec<>
pred :


args = vec![0;n];
marker = 0
if args[marker] < sig.len() {
    args[marker] += 1;
    if args[marker] == univ.len() {
        args[marker] = 0;
        marker += 1;
    }
}

}


for c in consts {
    terms.push(c)
}
while true {
for f in fun1 {
    for t in terms {
        terms.push(f(t))
    }
}
for f in fun2 {
    for t1 in terms {
        for t2 in terms{
            terms.push(f(t1, t2))
        }
    }
}
}
*/
