#[derive(Debug, Clone, PartialEq, Eq)]
enum FOL {
    Atom(Term),
    Not(Box<FOL>),
    And(Box<FOL>, Box<FOL>),
    Or(Box<FOL>, Box<FOL>),
    Imp(Box<FOL>, Box<FOL>),
    Iff(Box<FOL>, Box<FOL>),
    Forall(String, Box<FOL>),
    Exists(String, Box<FOL>),
}
