    fn is_variable(&self) -> bool {
        matches!(self, Term::Variable(_))
    }

    fn is_compound(&self) -> bool {
        matches!(self, Term::Compound(_, _))
    }

    fn function(&self) -> Option<&String> {
        match self {
            Term::Compound(func, _) => Some(func),
            _ => None,
        }
    }

    fn arguments(&self) -> Option<&Vec<Term>> {
        match self {
            Term::Compound(_, args) => Some(args),
            _ => None,
        }
    }

    // Convert a term to a string
    fn to_string(&self) -> String {
        match self {
            Term::Variable(var) => var.clone(),
            Term::Compound(func, args) => {
                if args.is_empty() {
                    func.clone()
                } else {
                    let arg_rep: Vec<String> = args.iter().map(|s| s.to_string()).collect();
                    format!("{}({})", func, arg_rep.join(","))
                }
            }
        }
    }

    // Check if the term is ground (has no variables)
    fn is_ground(&self) -> bool {
        match self {
            Term::Variable(_) => false,
            Term::Compound(_, args) => args.iter().all(|arg| arg.is_ground()),
        }
    }
    
    // Collect all function symbols in the term
    fn collect_funs(&self, res: &mut HashSet<String>) {
        match self {
            Term::Compound(func, args) => {
                res.insert(func.clone());
                for arg in args {
                    arg.collect_funs(res);
                }
            }
            _ => {}
        }
    }

    // Calculate the weight of the term
    fn weight(&self, fweight: usize, vweight: usize) -> usize {
        match self {
            Term::Variable(_) => vweight,
            Term::Compound(_, args) => {
                fweight
                    + args
                        .iter()
                        .map(|arg| arg.weight(fweight, vweight))
                        .sum::<usize>()
            }
        }
    }

    // ... Other methods ...

# [derive(Debug, Clone)]

struct Substitution {
    subst: HashMap<String, Term>,
    var_counter: i32
    //var_counter: Rc<RefCell<i32>>, // Shared counter to generate fresh variables.
}

    fn value(&self, var: &str) -> Term {
        self.subst
            .get(var)
            .cloned()
            .unwrap_or_else(|| Term::Variable(var.to_string()))
    }

    fn is_bound(&self, var: &str) -> bool {
        self.subst.contains_key(var)
    }

    // ... Other methods ...
        fn new(init: Vec<(String, Term)>) -> Self {
        let subst = init.into_iter().collect::<HashMap<_, _>>();
        Self {
            subst,
            var_counter: 0 //Rc::new(RefCell::new(1)),
        }
    }

// Placeholder structs and implementations for types like Literal, Variable, Substitution.
// These will need to be defined based on the actual application logic.

/*
impl fmt::Display for Clause {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "cnf({}, {}, [{}])",
            self.name.as_ref().unwrap_or(&"unnamed".to_string()),
            self.clause_type,
            self.literals
                .iter()
                .map(|l| l.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}*/

// Implementing display for ClauseSet for debugging purposes.
impl fmt::Display for ClauseSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for clause in &self.clauses {
            writeln!(f, "{:?}", clause)?;
        }
        Ok(())
    }
}
