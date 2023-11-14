// https://docs.rs/egg/latest/src/egg/language.rs.html#368-370

enum Expr {
    Var(usize),
    //C0(&str),
    //C1(&str, usize),
    //C2(&str, usize, usize),
    // Const(Symbol, u8, [usize;4])
    Const(Symbol, Vec<usize>),
}

struct RecExpr {
    nvars: usize,
    subterms: Vec<Expr>, // var_offset for freshening // minvar // maxvar // root(s) // var : Vec<Option<Expr>> an inline notion of subst
                         // compact : bool
                         // ground : Vec<bool>
}

impl RecExpr {
    fn new() -> Self {
        Vec::new()
    }

    fn add_var(&mut self, var: usize) {
        self.push(Expr::Var(var));
    }

    fn add_c0(&mut self, name: &str) {
        self.push(Expr::C0(name));
    }

    fn add_c1(&mut self, name: &str, arg: usize) {
        self.push(Expr::C1(name, arg));
    }

    fn add_c2(&mut self, name: &str, arg1: usize, arg2: usize) {
        self.push(Expr::C2(name, arg1, arg2));
    }

    fn compact(&self) -> Self {
        // hashcons
    }
    fn subst(&self, var: usize, expr: &Self) -> Self {
        // substitute var with expr
    }
    fn apply(&self, subst: Subst) -> Self {}
    fn occurs_check() {}
    fn unify() {}
}

// Clauses are also RecExpr
