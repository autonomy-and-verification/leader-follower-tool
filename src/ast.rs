#[derive(Clone, Debug, PartialEq)]
pub struct State(pub String);

#[derive(Clone, Debug, PartialEq)]
pub enum TransitionCondition {
    Equal,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
}

#[derive(Clone, Debug, PartialEq)]
pub enum LeaderTransition {
    IngoingWithCondition1(TransitionCondition, usize, String), // ?^{⋉ c} μ
    IngoingWithCondition2(TransitionCondition, usize, usize, String), // ?^{⋉ c/d} μ
    Outgoing(String),
    Local(String),
    Idle,
}

#[derive(Clone, Debug, PartialEq)]
pub enum FollowerTransition {
    Ingoing(String),
    Outgoing(String),
    Local(String),
    Idle,
}

#[derive(Clone, Debug)]
pub struct Leader {
    pub m_in: Vec<String>,
    pub m_out: Vec<String>,
    pub l: Vec<String>,
    pub states: Vec<State>,
    pub initial_states: Vec<State>,
    pub transitions: Vec<LeaderTransition>,
    pub delta: Vec<(State, LeaderTransition, State)>,
    pub pres: Vec<(State, LeaderTransition)>,
    pub tl: Vec<State>,
}

#[derive(Clone, Debug)]
pub struct Follower {
    pub m_in: Vec<String>,
    pub m_out: Vec<String>,
    pub l: Vec<String>,
    pub states: Vec<State>,
    pub initial_states: Vec<State>,
    pub transitions: Vec<FollowerTransition>,
    pub delta: Vec<(State, FollowerTransition, State)>,
    pub pres: Vec<(State, FollowerTransition)>,
    pub tl: Vec<State>,
}

impl Follower {
    fn is_transmission_recurrent_case_a(
        &self,
        start_state: State,
        transitions_taken: Vec<FollowerTransition>,
    ) -> bool {
        let outgoing_from_last_state = self
            .delta
            .iter()
            .filter(|(s, _, _)| s == &start_state)
            .map(|(_, t, s)| (t.to_owned(), s.to_owned()))
            .collect::<Vec<_>>();

        for (t, s) in outgoing_from_last_state.iter() {
            match t {
                FollowerTransition::Outgoing(_) => {
                    if transitions_taken.contains(t) {
                        return true;
                    } else {
                        let mut new_transitions_taken = transitions_taken.clone();
                        new_transitions_taken.push(t.to_owned());

                        return self
                            .is_transmission_recurrent_case_a(s.to_owned(), new_transitions_taken);
                    }
                }
                _ => continue,
            }
        }

        false
    }

    fn is_transmission_recurrent_case_b(&self, visited_states: Vec<State>) -> bool {
        let last_state = &visited_states[visited_states.len() - 1];
        let children_of_last_state = self
            .delta
            .iter()
            .filter(|(s, _, _)| s == last_state)
            .map(|(_, t, s)| (t.to_owned(), s.to_owned()))
            .collect::<Vec<_>>();

        let transitions_to_last_state = if visited_states.len() > 1 {
            visited_states[..visited_states.len() - 1]
                .iter()
                .enumerate()
                .map(|(i, _)| {
                    let index_in_delta = self
                        .delta
                        .iter()
                        .position(|(s0, _, s1)| s0 == &self.states[i] && s1 == &self.states[i + 1])
                        .unwrap();

                    let (_, t, _) = self.delta[index_in_delta].to_owned();
                    t
                })
                .collect::<Vec<_>>()
        } else {
            vec![]
        };

        for (t, s) in children_of_last_state.iter() {
            if visited_states.contains(&s) {
                // found a cycle; find the index where the state appears first
                let first_index_of_state = self.states.iter().position(|s_| s == s_).unwrap();

                if let FollowerTransition::Outgoing(_) = t {
                    // the cycle contains outgoing transitions
                    return true;
                }

                if transitions_to_last_state[first_index_of_state..]
                    .iter()
                    .filter(|t| match t {
                        FollowerTransition::Outgoing(_) => true,
                        _ => false,
                    })
                    .collect::<Vec<_>>()
                    .len()
                    != 0
                {
                    // the cycle contains outgoing transitions
                    return true;
                }
            } else {
                let mut new_visited_states = visited_states.clone();
                new_visited_states.push(s.to_owned());

                return self.is_transmission_recurrent_case_b(new_visited_states);
            }
        }

        false
    }

    pub fn is_transmission_recurrent(&self) -> bool {
        for state in self.initial_states.iter() {
            if self.is_transmission_recurrent_case_a(state.to_owned(), vec![]) {
                return true;
            }

            if self.is_transmission_recurrent_case_b(vec![state.to_owned()]) {
                return true;
            }
        }

        false
    }
}

#[derive(Clone, Debug)]
pub enum ArithmeticTerm {
    K, // universe size
    NumberLiteral(usize),
    HashInbox(usize), // #_{μ}; μ represented as an index to the leader's `m_in` vector
    HashState(usize), // #_{q}; q represented as an index to the follower's `states` vector
    HashStateTransition(usize, usize), // #_{q,τ}; q as above, τ represented as an index to the follower's `transitions` vector
    Add(Vec<ArithmeticTerm>),
    Mul(Vec<ArithmeticTerm>),
}

#[derive(Clone, Debug)]
pub enum ArithmeticRelation {
    Equal(Vec<ArithmeticTerm>),
    LessThan(Vec<ArithmeticTerm>),
    GreaterThan(Vec<ArithmeticTerm>),
    LessThanOrEqual(Vec<ArithmeticTerm>),
    GreaterThanOrEqual(Vec<ArithmeticTerm>),
}

#[derive(Clone, Debug)]
pub enum NonTemporalProperty {
    ArithmeticRelation(ArithmeticRelation),
    Not(Box<NonTemporalProperty>),
    And(Vec<NonTemporalProperty>),
    Or(Vec<NonTemporalProperty>),
    Implies(Vec<NonTemporalProperty>),
    Equivalent(Vec<NonTemporalProperty>),
}

#[derive(Clone, Debug)]
pub enum TemporalProperty {
    Always(usize, Box<NonTemporalProperty>),
    Eventually(usize, Box<NonTemporalProperty>),
}
