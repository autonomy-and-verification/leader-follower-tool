use crate::ast;
use crate::smt;

#[derive(Debug)]
pub struct Symbols {
    leader: ast::Leader,
    follower: ast::Follower,
    property: ast::TemporalProperty,
    horizon: usize,              // the number of time steps (n)
    ls: Vec<Vec<smt::Formula>>, // ls[m] contains the symbols ls_{q, m} (q represented as an index to leader.states)
    lt: Vec<Vec<smt::Formula>>, // lt[m] contains the symbols lt_{τ, m} (τ represented as an index to leader.transitions)
    inb: Vec<Vec<smt::Formula>>, // inb[m] contains the symbols inb_{μ, m} (μ represented as an index to follower.m_in)
    k: smt::ArithmeticTerm,
    x: Vec<Vec<smt::ArithmeticTerm>>, // x[m] contains the symbols x_{μ, m} (μ represented as an index to leader.m_in)
    y: Vec<Vec<smt::ArithmeticTerm>>, // y[m] contains the symbols y_{q, m} (q represented as an index to follower.states)
    z: Vec<Vec<smt::ArithmeticTerm>>, // z[m] contains the symbols z_{q, τ, m} ((q, τ) represented as an index to leader.pres)
    zi: Vec<Vec<smt::ArithmeticTerm>>, // zi[m] contains the symbols z_{q, idle, m} (q represented as an index to follower.states)
    enbl: Vec<Vec<Option<smt::Formula>>>, // enbl[m] contains the leader's enb_{τ, m} formulas (τ represented as an index to leader.transitions, None for idle)
    enbf: Vec<Vec<Option<smt::Formula>>>, // enbf[m] contains the followers's enb_{τ, m} formulas (τ represented as an index to follower.transitions, None for idle)
}

impl Symbols {
    pub fn new(
        leader: ast::Leader,
        follower: ast::Follower,
        property: ast::TemporalProperty,
    ) -> Self {
        Symbols {
            leader,
            follower,
            property,
            horizon: 0,
            ls: Vec::<Vec<smt::Formula>>::new(),
            lt: Vec::<Vec<smt::Formula>>::new(),
            inb: Vec::<Vec<smt::Formula>>::new(),
            k: smt::ArithmeticTerm::NumericVariable("K".into()),
            x: Vec::<Vec<smt::ArithmeticTerm>>::new(),
            y: Vec::<Vec<smt::ArithmeticTerm>>::new(),
            z: Vec::<Vec<smt::ArithmeticTerm>>::new(),
            zi: Vec::<Vec<smt::ArithmeticTerm>>::new(),
            enbl: Vec::<Vec<Option<smt::Formula>>>::new(),
            enbf: Vec::<Vec<Option<smt::Formula>>>::new(),
        }
    }

    fn populate_horizon(&mut self) {
        self.horizon = match self.property {
            ast::TemporalProperty::Always(horizon, _) => horizon,
            ast::TemporalProperty::Eventually(horizon, _) => horizon,
        };
    }

    fn populate_ls(&mut self) {
        for m in 0..=self.horizon {
            let mut res = Vec::<smt::Formula>::new();
            for i in 0..self.leader.states.len() {
                let text = format!("ls_q{}_m{}", i, m);
                res.push(smt::Formula::PropositionalVariable(text));
            }

            self.ls.push(res);
        }
    }

    fn populate_lt(&mut self) {
        for m in 0..=self.horizon {
            let mut res = Vec::<smt::Formula>::new();
            for i in 0..self.leader.transitions.len() {
                let text = format!("lt_tau{}_m{}", i, m);
                res.push(smt::Formula::PropositionalVariable(text));
            }

            self.lt.push(res);
        }
    }

    fn populate_inb(&mut self) {
        for m in 0..=self.horizon {
            let mut res = Vec::<smt::Formula>::new();
            for i in 0..self.follower.m_in.len() {
                let text = format!("inb_mu{}_m{}", i, m);
                res.push(smt::Formula::PropositionalVariable(text));
            }

            self.inb.push(res);
        }
    }

    fn populate_x(&mut self) {
        for m in 0..=self.horizon {
            let mut res = Vec::<smt::ArithmeticTerm>::new();
            for i in 0..self.leader.m_in.len() {
                let text = format!("x_mu{}_m{}", i, m);
                res.push(smt::ArithmeticTerm::NumericVariable(text));
            }

            self.x.push(res);
        }
    }

    fn populate_y(&mut self) {
        for m in 0..=self.horizon {
            let mut res = Vec::<smt::ArithmeticTerm>::new();
            for i in 0..self.follower.states.len() {
                let text = format!("y_q{}_m{}", i, m);
                res.push(smt::ArithmeticTerm::NumericVariable(text));
            }

            self.y.push(res);
        }
    }

    fn populate_z(&mut self) {
        for m in 0..=self.horizon {
            let mut res = Vec::<smt::ArithmeticTerm>::new();
            for i in 0..self.follower.pres.len() {
                let text = format!("z_pres{}_m{}", i, m);
                res.push(smt::ArithmeticTerm::NumericVariable(text));
            }

            self.z.push(res);
        }
    }

    fn populate_zi(&mut self) {
        for m in 0..=self.horizon {
            let mut res = Vec::<smt::ArithmeticTerm>::new();
            for i in 0..self.follower.states.len() {
                let text = format!("z_q{}_idle_m{}", i, m);
                res.push(smt::ArithmeticTerm::NumericVariable(text));
            }

            self.zi.push(res);
        }
    }

    fn populate_enbl(&mut self) {
        let ltimes = |condition, terms| match condition {
            ast::TransitionCondition::Equal => smt::ArithmeticRelation::Equal(terms),
            ast::TransitionCondition::LessThan => smt::ArithmeticRelation::LessThan(terms),
            ast::TransitionCondition::GreaterThan => smt::ArithmeticRelation::GreaterThan(terms),
            ast::TransitionCondition::LessThanOrEqual => {
                smt::ArithmeticRelation::LessThanOrEqual(terms)
            }
            ast::TransitionCondition::GreaterThanOrEqual => {
                smt::ArithmeticRelation::GreaterThanOrEqual(terms)
            }
        };

        for m in 0..=self.horizon {
            let mut res = Vec::<Option<smt::Formula>>::new();
            for transition in &self.leader.transitions {
                let option = match transition {
                    ast::LeaderTransition::IngoingWithCondition1(condition, c, mu) => {
                        let index = self.leader.m_in.iter().position(|m| m == mu).unwrap();
                        let x_mu_m = self.x[m][index].to_owned();
                        let c = smt::ArithmeticTerm::NumberLiteral(*c);

                        let relation = ltimes(condition.to_owned(), vec![x_mu_m, c]);

                        Some(smt::Formula::ArithmeticRelation(relation))
                    }
                    ast::LeaderTransition::IngoingWithCondition2(condition, c, d, mu) => {
                        let index = self.leader.m_in.iter().position(|m| m == mu).unwrap();
                        let x_mu_m = self.x[m][index].to_owned();
                        let c = smt::ArithmeticTerm::NumberLiteral(*c);
                        let d = smt::ArithmeticTerm::NumberLiteral(*d);
                        let d_times_x_mu_m = smt::ArithmeticTerm::Mul(vec![d, x_mu_m]);
                        let c_times_k = smt::ArithmeticTerm::Mul(vec![c, self.k.to_owned()]);

                        let relation =
                            ltimes(condition.to_owned(), vec![d_times_x_mu_m, c_times_k]);

                        Some(smt::Formula::ArithmeticRelation(relation))
                    }
                    ast::LeaderTransition::Outgoing(_) | ast::LeaderTransition::Local(_) => {
                        Some(smt::Formula::PropositionalConstantTrue)
                    }
                    ast::LeaderTransition::Idle => None,
                };

                res.push(option)
            }

            self.enbl.push(res);
        }
    }

    fn populate_enbf(&mut self) {
        for m in 0..=self.horizon {
            let mut res = Vec::<Option<smt::Formula>>::new();
            for transition in &self.follower.transitions {
                let option = match transition {
                    ast::FollowerTransition::Ingoing(mu) => {
                        let index = self.follower.m_in.iter().position(|m| m == mu).unwrap();
                        Some(self.inb[m][index].to_owned())
                    }
                    ast::FollowerTransition::Outgoing(_) | ast::FollowerTransition::Local(_) => {
                        Some(smt::Formula::PropositionalConstantTrue)
                    }
                    ast::FollowerTransition::Idle => None,
                };

                res.push(option)
            }

            self.enbf.push(res);
        }
    }

    pub fn populate(&mut self) {
        self.populate_horizon();
        self.populate_ls();
        self.populate_lt();
        self.populate_inb();
        self.populate_x();
        self.populate_y();
        self.populate_z();
        self.populate_zi();
        self.populate_enbl();
        self.populate_enbf();
    }

    fn smt_definitions(&self) -> String {
        let mut definitions = String::new();

        // ls definitions
        for m in 0..=self.horizon {
            for symbol in self.ls[m].iter() {
                let definition = format!("(declare-fun {} () Bool)\n", symbol);
                definitions.push_str(definition.as_str());
            }
        }

        // lt definitions
        definitions.push_str("\n");
        for m in 0..=self.horizon {
            for symbol in self.lt[m].iter() {
                let definition = format!("(declare-fun {} () Bool)\n", symbol);
                definitions.push_str(definition.as_str());
            }
        }

        // inb definitions
        definitions.push_str("\n");
        for m in 0..=self.horizon {
            for symbol in self.inb[m].iter() {
                let definition = format!("(declare-fun {} () Bool)\n", symbol);
                definitions.push_str(definition.as_str());
            }
        }

        // K definition
        definitions.push_str("\n");
        definitions.push_str("(declare-fun K () Int)\n");

        // x definitions
        definitions.push_str("\n");
        for m in 0..=self.horizon {
            for symbol in self.x[m].iter() {
                let definition = format!("(declare-fun {} () Int)\n", symbol);
                definitions.push_str(definition.as_str());
            }
        }

        // y definitions
        definitions.push_str("\n");
        for m in 0..=self.horizon {
            for symbol in self.y[m].iter() {
                let definition = format!("(declare-fun {} () Int)\n", symbol);
                definitions.push_str(definition.as_str());
            }
        }

        // z definitions
        definitions.push_str("\n");
        for m in 0..=self.horizon {
            for symbol in self.z[m].iter() {
                let definition = format!("(declare-fun {} () Int)\n", symbol);
                definitions.push_str(definition.as_str());
            }
        }

        // z_idle definitions
        definitions.push_str("\n");
        for m in 0..=self.horizon {
            for symbol in self.zi[m].iter() {
                let definition = format!("(declare-fun {} () Int)\n", symbol);
                definitions.push_str(definition.as_str());
            }
        }

        definitions
    }
}

fn chi0(symbols: &Symbols) -> smt::Formula {
    let initial_state_indices = symbols
        .leader
        .states
        .iter()
        .enumerate() // pairs (index, state)
        .filter(|&(_, s)| symbols.leader.initial_states.contains(s))
        .map(|(i, _)| i) // drop the state from the filtered pairs (index, state)
        .collect::<Vec<usize>>();

    smt::Formula::Or(
        symbols.ls[0]
            .iter()
            .enumerate() // pairs (index, formula)
            .filter(|&(i, _)| initial_state_indices.contains(&i))
            .map(|(_, e)| e.to_owned()) // drop the index from the filtered pairs (index, formula)
            .collect(),
    )
}

fn chi1(symbols: &Symbols) -> smt::Formula {
    smt::Formula::And(
        symbols.x[0]
            .iter()
            .map(|x_mu_0| {
                smt::Formula::ArithmeticRelation(smt::ArithmeticRelation::Equal(vec![
                    x_mu_0.to_owned(),
                    smt::ArithmeticTerm::NumberLiteral(0),
                ]))
            })
            .collect(),
    )
}

fn chi2(symbols: &Symbols, m: usize) -> smt::Formula {
    smt::Formula::ExactlyOne(
        symbols.ls[m]
            .iter()
            .map(|ls_q_m| ls_q_m.to_owned())
            .collect(),
    )
}

fn chi3(symbols: &Symbols, m: usize) -> smt::Formula {
    smt::Formula::ExactlyOne(
        symbols.lt[m]
            .iter()
            .map(|lt_tau_m| lt_tau_m.to_owned())
            .collect(),
    )
}

fn chi4(symbols: &Symbols, m: usize) -> smt::Formula {
    smt::Formula::Or(
        symbols
            .leader
            .pres
            .iter() // (q, tau)
            .enumerate() // (index, (q, tau))
            .map(|(i, (q, tau))| {
                let index_of_q = symbols.leader.states.iter().position(|q_| q == q_).unwrap();
                let index_of_tau = symbols
                    .leader
                    .transitions
                    .iter()
                    .position(|tau_| tau == tau_)
                    .unwrap();

                let tail_q_t = symbols.leader.tl[i].to_owned();
                let index_of_tail_q_t = symbols
                    .leader
                    .states
                    .iter()
                    .position(|tail_q_t_| tail_q_t == tail_q_t_.to_owned())
                    .unwrap();

                smt::Formula::And(vec![
                    symbols.ls[m][index_of_q].to_owned(),
                    symbols.enbl[m][index_of_tau].to_owned().unwrap(),
                    symbols.lt[m][index_of_tau].to_owned(),
                    symbols.ls[m + 1][index_of_tail_q_t].to_owned(),
                ])
            })
            .collect(),
    )
}

fn chi5(symbols: &Symbols, m: usize) -> smt::Formula {
    smt::Formula::Or(
        symbols
            .leader
            .states
            .iter()
            .enumerate() // (index, q)
            .map(|(i, q)| {
                let no_enabled = smt::Formula::And(
                    symbols
                        .leader
                        .pres
                        .iter()
                        .filter(|(q_, _)| q == q_)
                        .map(|(_, tau)| {
                            let index_of_tau = symbols
                                .leader
                                .transitions
                                .iter()
                                .position(|tau_| tau == tau_)
                                .unwrap();

                            smt::Formula::Not(Box::new(
                                symbols.enbl[m][index_of_tau].to_owned().unwrap(),
                            ))
                        })
                        .collect(),
                );

                smt::Formula::And(vec![
                    symbols.ls[m][i].to_owned(),
                    no_enabled,
                    symbols.lt[m][0].to_owned(),
                    symbols.ls[m + 1][i].to_owned(),
                ])
            })
            .collect(),
    )
}

fn chi6(symbols: &Symbols) -> smt::Formula {
    let first_part =
        (0..=symbols.horizon).map(|m| smt::Formula::And(vec![chi2(symbols, m), chi3(symbols, m)]));
    let first_part = smt::Formula::And(first_part.collect());

    let second_part =
        (0..symbols.horizon).map(|m| smt::Formula::Or(vec![chi4(symbols, m), chi5(symbols, m)]));
    let second_part = smt::Formula::And(second_part.collect());

    smt::Formula::And(vec![first_part, second_part])
}

fn chi7(symbols: &Symbols) -> smt::Formula {
    let non_initial_state_indices = symbols
        .follower
        .states
        .iter()
        .enumerate() // pairs (index, state)
        .filter(|&(_, s)| !symbols.leader.initial_states.contains(s))
        .map(|(i, _)| i) // drop the state from the filtered pairs (index, state)
        .collect::<Vec<usize>>();

    smt::Formula::And(
        symbols.y[0]
            .iter()
            .enumerate()
            .filter(|&(i, _)| non_initial_state_indices.contains(&i))
            .map(|(_, y_q_0)| {
                smt::Formula::ArithmeticRelation(smt::ArithmeticRelation::Equal(vec![
                    y_q_0.to_owned(),
                    smt::ArithmeticTerm::NumberLiteral(0),
                ]))
            })
            .collect(),
    )
}

fn chi8(symbols: &Symbols) -> smt::Formula {
    smt::Formula::And(
        symbols.inb[0]
            .iter()
            .map(|inb_mu_0| smt::Formula::Not(Box::new(inb_mu_0.to_owned())))
            .collect(),
    )
}

fn chi9(symbols: &Symbols, m: usize) -> smt::Formula {
    smt::Formula::And(
        symbols.x[m]
            .iter()
            .map(|x_mu_m| {
                smt::Formula::ArithmeticRelation(smt::ArithmeticRelation::GreaterThanOrEqual(vec![
                    x_mu_m.to_owned(),
                    smt::ArithmeticTerm::NumberLiteral(0),
                ]))
            })
            .collect(),
    )
}

fn chi10(symbols: &Symbols, m: usize) -> smt::Formula {
    smt::Formula::And(
        symbols.y[m]
            .iter()
            .map(|y_q_m| {
                smt::Formula::ArithmeticRelation(smt::ArithmeticRelation::GreaterThanOrEqual(vec![
                    y_q_m.to_owned(),
                    smt::ArithmeticTerm::NumberLiteral(0),
                ]))
            })
            .collect(),
    )
}

fn chi11(symbols: &Symbols, m: usize) -> smt::Formula {
    let first_part = symbols
        .follower
        .pres
        .iter()
        .enumerate()
        .map(|(i, _)| {
            smt::Formula::ArithmeticRelation(smt::ArithmeticRelation::GreaterThanOrEqual(vec![
                symbols.z[m][i].to_owned(),
                smt::ArithmeticTerm::NumberLiteral(0),
            ]))
        })
        .collect();
    let first_part = smt::Formula::And(first_part);

    let second_part = symbols
        .follower
        .states
        .iter()
        .enumerate()
        .map(|(i, _)| {
            smt::Formula::ArithmeticRelation(smt::ArithmeticRelation::GreaterThanOrEqual(vec![
                symbols.zi[m][i].to_owned(),
                smt::ArithmeticTerm::NumberLiteral(0),
            ]))
        })
        .collect();
    let second_part = smt::Formula::And(second_part);

    smt::Formula::And(vec![first_part, second_part])
}

fn chi12(symbols: &Symbols) -> smt::Formula {
    smt::Formula::And(
        (0..=symbols.horizon)
            .map(|m| {
                smt::Formula::And(vec![chi9(symbols, m), chi10(symbols, m), chi11(symbols, m)])
            })
            .collect(),
    )
}

fn chi13(symbols: &Symbols, m: usize) -> smt::Formula {
    smt::Formula::ArithmeticRelation(smt::ArithmeticRelation::Equal(vec![
        smt::ArithmeticTerm::Add(symbols.y[m].iter().map(|y_q_m| y_q_m.to_owned()).collect()),
        symbols.k.to_owned(),
    ]))
}

fn chi14(symbols: &Symbols, m: usize) -> smt::Formula {
    smt::Formula::And(
        symbols
            .follower
            .states
            .iter()
            .enumerate()
            .map(|(i, q)| {
                let y_q_m = symbols.y[m][i].to_owned();
                let sum_z_q_tau_m = smt::ArithmeticTerm::Add(
                    symbols
                        .follower
                        .pres
                        .iter()
                        .enumerate()
                        .filter(|(_, (q_, _))| q == q_)
                        .map(|(j, _)| symbols.z[m][j].to_owned())
                        .collect::<Vec<_>>(),
                );
                let z_q_idle_m = symbols.zi[m][i].to_owned();

                smt::Formula::ArithmeticRelation(smt::ArithmeticRelation::Equal(vec![
                    y_q_m,
                    smt::ArithmeticTerm::Add(vec![sum_z_q_tau_m, z_q_idle_m]),
                ]))
            })
            .collect(),
    )
}

fn chi15(symbols: &Symbols, m: usize) -> smt::Formula {
    smt::Formula::And(
        symbols
            .follower
            .states
            .iter()
            .enumerate()
            .map(|(i, q_)| {
                let y_q_prime_m_plus_1 = symbols.y[m + 1][i].to_owned();
                let sum_z_q_tau_m = smt::ArithmeticTerm::Add(
                    symbols
                        .follower
                        .pres
                        .iter()
                        .enumerate()
                        .filter(|(j, _)| symbols.follower.tl[*j] == *q_)
                        .map(|(j, _)| symbols.z[m][j].to_owned())
                        .collect::<Vec<_>>(),
                );
                let z_q_prime_idle_m = symbols.zi[m][i].to_owned();

                smt::Formula::ArithmeticRelation(smt::ArithmeticRelation::Equal(vec![
                    y_q_prime_m_plus_1,
                    smt::ArithmeticTerm::Add(vec![sum_z_q_tau_m, z_q_prime_idle_m]),
                ]))
            })
            .collect(),
    )
}

fn chi16(symbols: &Symbols, m: usize) -> smt::Formula {
    smt::Formula::And(
        symbols
            .follower
            .m_in
            .iter()
            .enumerate()
            .map(|(i, mu)| {
                smt::Formula::Implies(vec![
                    smt::Formula::Not(Box::new(symbols.inb[m][i].to_owned())),
                    smt::Formula::And(
                        symbols
                            .follower
                            .pres
                            .iter()
                            .enumerate()
                            .filter(|(_, (q, tau))| {
                                (q.to_owned(), tau.to_owned())
                                    == (
                                        q.to_owned(),
                                        ast::FollowerTransition::Ingoing(mu.to_string()),
                                    )
                            })
                            .map(|(j, _)| {
                                smt::Formula::ArithmeticRelation(smt::ArithmeticRelation::Equal(
                                    vec![
                                        symbols.z[m][j].to_owned(),
                                        smt::ArithmeticTerm::NumberLiteral(0),
                                    ],
                                ))
                            })
                            .collect(),
                    ),
                ])
            })
            .collect(),
    )
}

fn chi17(symbols: &Symbols, m: usize) -> smt::Formula {
    smt::Formula::And(
        symbols
            .follower
            .states
            .iter()
            .enumerate()
            .map(|(i, q)| {
                smt::Formula::Equivalent(vec![
                    smt::Formula::ArithmeticRelation(smt::ArithmeticRelation::Equal(vec![
                        symbols.zi[m][i].to_owned(),
                        smt::ArithmeticTerm::NumberLiteral(0),
                    ])),
                    smt::Formula::Or(vec![
                        smt::Formula::ArithmeticRelation(smt::ArithmeticRelation::Equal(vec![
                            symbols.y[m][i].to_owned(),
                            smt::ArithmeticTerm::NumberLiteral(0),
                        ])),
                        smt::Formula::Or(
                            symbols
                                .follower
                                .pres
                                .iter()
                                .filter(|(q_, _)| q == q_)
                                .map(|(_, tau)| {
                                    let index_of_tau = symbols
                                        .follower
                                        .transitions
                                        .iter()
                                        .position(|tau_| tau == tau_)
                                        .unwrap();

                                    symbols.enbf[m][index_of_tau].to_owned().unwrap()
                                })
                                .collect(),
                        ),
                    ]),
                ])
            })
            .collect(),
    )
}

fn chi18(symbols: &Symbols, m: usize) -> smt::Formula {
    smt::Formula::And(
        symbols
            .follower
            .m_in
            .iter()
            .enumerate()
            .map(|(i, mu)| {
                smt::Formula::Implies(vec![
                    symbols.inb[m][i].to_owned(),
                    symbols.inb[m + 1][i].to_owned(),
                ])
            })
            .collect(),
    )
}

fn chi19(symbols: &Symbols, m: usize) -> smt::Formula {
    smt::Formula::And(vec![
        chi14(symbols, m),
        chi15(symbols, m),
        chi16(symbols, m),
        chi17(symbols, m),
        chi18(symbols, m),
    ])
}

fn chi20(symbols: &Symbols) -> smt::Formula {
    smt::Formula::And(vec![
        smt::Formula::And((0..=symbols.horizon).map(|m| chi13(symbols, m)).collect()),
        smt::Formula::And((0..symbols.horizon).map(|m| chi19(symbols, m)).collect()),
    ])
}

fn chi21(symbols: &Symbols, m: usize) -> smt::Formula {
    smt::Formula::And(
        symbols
            .leader
            .m_out
            .iter()
            .map(|mu| {
                let index_of_outgoing_mu = symbols
                    .leader
                    .transitions
                    .iter()
                    .position(|tau| *tau == ast::LeaderTransition::Outgoing(mu.to_string()))
                    .unwrap();

                let index_of_mu = symbols
                    .follower
                    .m_in
                    .iter()
                    .position(|mu_| mu == mu_)
                    .unwrap();

                smt::Formula::Implies(vec![
                    symbols.lt[m][index_of_outgoing_mu].to_owned(),
                    symbols.inb[m + 1][index_of_mu].to_owned(),
                ])
            })
            .collect(),
    )
}

fn chi22(symbols: &Symbols, m: usize) -> smt::Formula {
    smt::Formula::And(
        symbols
            .follower
            .m_in
            .iter()
            .enumerate()
            .map(|(i, mu)| {
                let index_of_outgoing_mu = symbols
                    .leader
                    .transitions
                    .iter()
                    .position(|tau| *tau == ast::LeaderTransition::Outgoing(mu.to_string()))
                    .unwrap();

                smt::Formula::Implies(vec![
                    smt::Formula::And(vec![
                        smt::Formula::Not(Box::new(symbols.inb[m][i].to_owned())),
                        symbols.inb[m + 1][i].to_owned(),
                    ]),
                    symbols.lt[m][index_of_outgoing_mu].to_owned(),
                ])
            })
            .collect(),
    )
}

fn chi23(symbols: &Symbols, m: usize) -> smt::Formula {
    smt::Formula::And(
        symbols
            .leader
            .m_in
            .iter()
            .enumerate()
            .map(|(i, mu)| {
                smt::Formula::ArithmeticRelation(smt::ArithmeticRelation::Equal(vec![
                    symbols.x[m + 1][i].to_owned(),
                    smt::ArithmeticTerm::Add(vec![
                        symbols.x[m][i].to_owned(),
                        smt::ArithmeticTerm::Add(
                            symbols
                                .follower
                                .pres
                                .iter()
                                .enumerate()
                                .filter(|(_, (_, tau))| {
                                    *tau == ast::FollowerTransition::Outgoing(mu.to_string())
                                })
                                .map(|(i, _)| symbols.z[m][i].to_owned())
                                .collect(),
                        ),
                    ]),
                ]))
            })
            .collect(),
    )
}

fn chi24(symbols: &Symbols) -> smt::Formula {
    smt::Formula::And(
        (0..symbols.horizon)
            .map(|m| {
                smt::Formula::And(vec![
                    chi21(symbols, m),
                    chi22(symbols, m),
                    chi23(symbols, m),
                ])
            })
            .collect(),
    )
}

fn phi(symbols: &Symbols) -> smt::Formula {
    smt::Formula::And(vec![
        smt::Formula::ArithmeticRelation(smt::ArithmeticRelation::GreaterThan(vec![
            symbols.k.to_owned(),
            smt::ArithmeticTerm::NumberLiteral(0),
        ])),
        chi0(symbols),
        chi1(symbols),
        chi6(symbols),
        chi7(symbols),
        chi8(symbols),
        chi12(symbols),
        chi20(symbols),
        chi24(symbols),
    ])
}

fn bar_arithmetic_term(
    symbols: &Symbols,
    term: &ast::ArithmeticTerm,
    horizon: usize,
) -> Vec<smt::ArithmeticTerm> {
    match term {
        ast::ArithmeticTerm::K => (0..=horizon).map(|m| symbols.k.to_owned()).collect(),
        ast::ArithmeticTerm::NumberLiteral(number) => (0..=horizon)
            .map(|m| smt::ArithmeticTerm::NumberLiteral(*number))
            .collect(),
        ast::ArithmeticTerm::HashInbox(i) => {
            (0..=horizon).map(|m| symbols.x[m][*i].to_owned()).collect()
        }
        ast::ArithmeticTerm::HashState(i) => {
            (0..=horizon).map(|m| symbols.y[m][*i].to_owned()).collect()
        }
        ast::ArithmeticTerm::HashStateTransition(i, j) => {
            let q = symbols.follower.states[*i].to_owned();
            let tau = symbols.follower.transitions[*j].to_owned();

            let index_in_pres = symbols
                .follower
                .pres
                .iter()
                .position(|(q_, tau_)| q == *q_ && tau == *tau_)
                .unwrap();

            (0..=horizon)
                .map(|m| symbols.z[m][index_in_pres].to_owned())
                .collect()
        }
        ast::ArithmeticTerm::Add(terms) => {
            let bars = terms
                .iter()
                .map(|t| bar_arithmetic_term(symbols, t, horizon))
                .collect::<Vec<_>>();

            (0..=horizon)
                .map(|m| smt::ArithmeticTerm::Add(bars.iter().map(|b| b[m].to_owned()).collect()))
                .collect()
        }
        ast::ArithmeticTerm::Mul(terms) => {
            let bars = terms
                .iter()
                .map(|t| bar_arithmetic_term(symbols, t, horizon))
                .collect::<Vec<_>>();

            (0..=horizon)
                .map(|m| smt::ArithmeticTerm::Mul(bars.iter().map(|b| b[m].to_owned()).collect()))
                .collect()
        }
    }
}

fn bar_arithmetic_relation(
    symbols: &Symbols,
    relation: &ast::ArithmeticRelation,
    horizon: usize,
) -> Vec<smt::ArithmeticRelation> {
    match relation {
        ast::ArithmeticRelation::Equal(terms) => {
            let bars = terms
                .iter()
                .map(|t| bar_arithmetic_term(symbols, t, horizon))
                .collect::<Vec<_>>();

            (0..=horizon)
                .map(|m| {
                    smt::ArithmeticRelation::Equal(bars.iter().map(|b| b[m].to_owned()).collect())
                })
                .collect()
        }
        ast::ArithmeticRelation::LessThan(terms) => {
            let bars = terms
                .iter()
                .map(|t| bar_arithmetic_term(symbols, t, horizon))
                .collect::<Vec<_>>();

            (0..=horizon)
                .map(|m| {
                    smt::ArithmeticRelation::LessThan(
                        bars.iter().map(|b| b[m].to_owned()).collect(),
                    )
                })
                .collect()
        }
        ast::ArithmeticRelation::GreaterThan(terms) => {
            let bars = terms
                .iter()
                .map(|t| bar_arithmetic_term(symbols, t, horizon))
                .collect::<Vec<_>>();

            (0..=horizon)
                .map(|m| {
                    smt::ArithmeticRelation::GreaterThan(
                        bars.iter().map(|b| b[m].to_owned()).collect(),
                    )
                })
                .collect()
        }
        ast::ArithmeticRelation::LessThanOrEqual(terms) => {
            let bars = terms
                .iter()
                .map(|t| bar_arithmetic_term(symbols, t, horizon))
                .collect::<Vec<_>>();

            (0..=horizon)
                .map(|m| {
                    smt::ArithmeticRelation::LessThanOrEqual(
                        bars.iter().map(|b| b[m].to_owned()).collect(),
                    )
                })
                .collect()
        }
        ast::ArithmeticRelation::GreaterThanOrEqual(terms) => {
            let bars = terms
                .iter()
                .map(|t| bar_arithmetic_term(symbols, t, horizon))
                .collect::<Vec<_>>();

            (0..=horizon)
                .map(|m| {
                    smt::ArithmeticRelation::GreaterThanOrEqual(
                        bars.iter().map(|b| b[m].to_owned()).collect(),
                    )
                })
                .collect()
        }
    }
}

fn bar_non_temporal_property(
    symbols: &Symbols,
    property: &ast::NonTemporalProperty,
    horizon: usize,
) -> Vec<smt::Formula> {
    match property {
        ast::NonTemporalProperty::ArithmeticRelation(relation) => {
            bar_arithmetic_relation(symbols, relation, horizon)
                .iter()
                .map(|r| smt::Formula::ArithmeticRelation(r.to_owned()))
                .collect()
        }
        ast::NonTemporalProperty::Not(boxed_non_temporal_property) => {
            bar_non_temporal_property(symbols, &boxed_non_temporal_property, horizon)
                .iter()
                .map(|f| smt::Formula::Not(Box::new(f.to_owned())))
                .collect()
        }
        ast::NonTemporalProperty::And(non_temporal_properties) => {
            let bars = non_temporal_properties
                .iter()
                .map(|ntp| bar_non_temporal_property(symbols, ntp, horizon))
                .collect::<Vec<_>>();

            (0..=horizon)
                .map(|m| smt::Formula::And(bars.iter().map(|b| b[m].to_owned()).collect()))
                .collect()
        }
        ast::NonTemporalProperty::Or(non_temporal_properties) => {
            let bars = non_temporal_properties
                .iter()
                .map(|ntp| bar_non_temporal_property(symbols, ntp, horizon))
                .collect::<Vec<_>>();

            (0..=horizon)
                .map(|m| smt::Formula::Or(bars.iter().map(|b| b[m].to_owned()).collect()))
                .collect()
        }
        ast::NonTemporalProperty::Implies(non_temporal_properties) => {
            let bars = non_temporal_properties
                .iter()
                .map(|ntp| bar_non_temporal_property(symbols, ntp, horizon))
                .collect::<Vec<_>>();

            (0..=horizon)
                .map(|m| smt::Formula::Implies(bars.iter().map(|b| b[m].to_owned()).collect()))
                .collect()
        }
        ast::NonTemporalProperty::Equivalent(non_temporal_properties) => {
            let bars = non_temporal_properties
                .iter()
                .map(|ntp| bar_non_temporal_property(symbols, ntp, horizon))
                .collect::<Vec<_>>();

            (0..=horizon)
                .map(|m| smt::Formula::Equivalent(bars.iter().map(|b| b[m].to_owned()).collect()))
                .collect()
        }
    }
}

fn bar(symbols: &Symbols, property: &ast::TemporalProperty) -> smt::Formula {
    match property.to_owned() {
        ast::TemporalProperty::Always(horizon, boxed_property) => {
            smt::Formula::And(bar_non_temporal_property(symbols, &boxed_property, horizon))
        }
        ast::TemporalProperty::Eventually(horizon, boxed_property) => {
            smt::Formula::Or(bar_non_temporal_property(symbols, &boxed_property, horizon))
        }
    }
}

fn to_check(symbols: &Symbols) -> smt::Formula {
    smt::Formula::And(vec![
        phi(symbols),
        smt::Formula::Not(Box::new(bar(symbols, &symbols.property))),
    ])
}

fn smt_assertion(symbols: &Symbols) -> String {
    format!("(assert {})\n", to_check(symbols))
}

pub fn smt_file_content(symbols: &Symbols) -> String {
    format!(
        "{}\n{}\n(check-sat)\n(exit)\n",
        symbols.smt_definitions(),
        smt_assertion(symbols)
    )
}
