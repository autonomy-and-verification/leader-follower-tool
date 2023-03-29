use std::fmt;

fn format<T: fmt::Display>(t: &T) -> String {
    format!("{}", t)
}

fn join<T: fmt::Display>(ts: &Vec<T>) -> String {
    ts.iter().map(format).collect::<Vec<String>>().join(" ")
}

#[derive(Debug, Clone)]
pub enum ArithmeticTerm {
    NumberLiteral(usize),
    NumericVariable(String),
    Add(Vec<ArithmeticTerm>),
    Mul(Vec<ArithmeticTerm>),
}

impl fmt::Display for ArithmeticTerm {
    #[cfg_attr(rustfmt, rustfmt_skip)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ArithmeticTerm::NumberLiteral(number)          => write!(f, "{}", number),
            ArithmeticTerm::NumericVariable(variable)      => write!(f, "{}", variable),
            ArithmeticTerm::Add(terms) if terms.len() == 0 => write!(f, "0"),
            ArithmeticTerm::Add(terms)                     => write!(f, "(+ {})", join(terms)),
            ArithmeticTerm::Mul(terms) if terms.len() == 0 => write!(f, "1"),
            ArithmeticTerm::Mul(terms)                     => write!(f, "(* {})", join(terms)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ArithmeticRelation {
    Equal(Vec<ArithmeticTerm>),
    LessThan(Vec<ArithmeticTerm>),
    GreaterThan(Vec<ArithmeticTerm>),
    LessThanOrEqual(Vec<ArithmeticTerm>),
    GreaterThanOrEqual(Vec<ArithmeticTerm>),
}

impl fmt::Display for ArithmeticRelation {
    #[cfg_attr(rustfmt, rustfmt_skip)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ArithmeticRelation::Equal(terms)              => write!(f, "(= {})", join(terms)),
            ArithmeticRelation::LessThan(terms)           => write!(f, "(< {})", join(terms)),
            ArithmeticRelation::GreaterThan(terms)        => write!(f, "(> {})", join(terms)),
            ArithmeticRelation::LessThanOrEqual(terms)    => write!(f, "(<= {})", join(terms)),
            ArithmeticRelation::GreaterThanOrEqual(terms) => write!(f, "(>= {})", join(terms)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Formula {
    PropositionalConstantTrue,
    PropositionalConstantFalse,
    PropositionalVariable(String),
    ArithmeticRelation(ArithmeticRelation),
    Not(Box<Formula>),
    And(Vec<Formula>),
    Or(Vec<Formula>),
    Implies(Vec<Formula>),
    Equivalent(Vec<Formula>),
    ExactlyOne(Vec<Formula>),
}

impl fmt::Display for Formula {
    #[cfg_attr(rustfmt, rustfmt_skip)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Formula::PropositionalConstantTrue            => write!(f, "true"),
            Formula::PropositionalConstantFalse           => write!(f, "false"),
            Formula::PropositionalVariable(variable)      => write!(f, "{}", variable),
            Formula::ArithmeticRelation(relation)         => write!(f, "{}", relation),
            Formula::Not(formula)                         => write!(f, "(not {})", formula),
            Formula::And(formulas) if formulas.len() == 0 => write!(f, "true"),
            Formula::And(formulas)                        => write!(f, "(and {})", join(formulas)),
            Formula::Or(formulas) if formulas.len() == 0  => write!(f, "false"),
            Formula::Or(formulas)                         => write!(f, "(or {})", join(formulas)),
            Formula::Implies(formulas)                    => write!(f, "(=> {})", join(formulas)),
            Formula::Equivalent(formulas)                 => write!(f, "(= {})", join(formulas)),
            Formula::ExactlyOne(formulas)                 => write!(f, "(and ((_ at-least 1) {join}) ((_ at-most 1) {join}))", join=join(formulas))
        }
    }
}
