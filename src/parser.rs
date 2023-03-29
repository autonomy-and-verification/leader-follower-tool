use crate::ast;

use crate::error::*;
use crate::token::{Token, TokenKind};

pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, index: 0 }
    }

    #[inline]
    fn advance(&mut self) {
        self.index += 1
    }

    #[inline]
    fn peek(&self, offset: usize) -> Option<&Token> {
        match self.index.checked_add(offset) {
            Some(index) if index < self.tokens.len() => Some(&self.tokens[index]),
            _ => None,
        }
    }

    fn consume(&mut self, kind: TokenKind, error_message: &str) -> usize {
        let index = self.index;
        let current_token = self.peek(0).unwrap();

        if !(current_token.kind == kind) {
            error(error_message, current_token.line, current_token.column);
        }

        self.advance();
        index
    }

    fn consume_one_of(&mut self, kinds: Vec<TokenKind>, error_message: &str) -> usize {
        let index = self.index;
        let current_token = self.peek(0).unwrap();

        if !(kinds.contains(&current_token.kind)) {
            error(error_message, current_token.line, current_token.column);
        }

        self.advance();
        index
    }

    fn consume_keyword(&mut self, text: &str, error_message: &str) -> usize {
        let index = self.consume(TokenKind::Word, error_message);
        let token = &self.tokens[index];

        if token.text != text {
            error(error_message, token.line, token.column);
        }

        index
    }

    fn consume_keyword_one_of(&mut self, texts: Vec<&str>, error_message: &str) -> usize {
        let index = self.consume(TokenKind::Word, error_message);
        let token = &self.tokens[index];

        if !texts.contains(&token.text.as_str()) {
            error(error_message, token.line, token.column);
        }

        index
    }

    fn parse_state(&mut self) -> ast::State {
        self.consume(TokenKind::LeftParenthesis, "Expecting left parenthesis.");
        self.consume_keyword("state", "Expecting keyword `state`.");

        let index = self.consume(TokenKind::Word, "Expecting state name.");
        let name = self.tokens[index].text.to_owned();

        self.consume(TokenKind::RightParenthesis, "Expecting right parenthesis.");

        ast::State(name)
    }

    fn parse_initial_states(&mut self) -> Vec<ast::State> {
        self.consume(TokenKind::LeftParenthesis, "Expecting left parenthesis.");
        self.consume_keyword("initial", "Expecting keyword `initial`.");

        let mut initial_states = Vec::<ast::State>::new();
        while self.peek(0).unwrap().kind == TokenKind::LeftParenthesis {
            let state = self.parse_state();
            initial_states.push(state);
        }

        self.consume(TokenKind::RightParenthesis, "Expecting right parenthesis.");

        initial_states
    }

    fn parse_leader_transition_condition(&mut self) -> ast::TransitionCondition {
        let kinds = vec![
            TokenKind::Equal,
            TokenKind::LessThan,
            TokenKind::GreaterThan,
            TokenKind::LessThanOrEqual,
            TokenKind::GreaterThanOrEqual,
        ];

        let index = self.consume_one_of(kinds, "Expecting a transition condition.");
        let condition = match self.tokens[index].kind {
            TokenKind::Equal => ast::TransitionCondition::Equal,
            TokenKind::LessThan => ast::TransitionCondition::LessThan,
            TokenKind::GreaterThan => ast::TransitionCondition::GreaterThan,
            TokenKind::LessThanOrEqual => ast::TransitionCondition::LessThanOrEqual,
            TokenKind::GreaterThanOrEqual => ast::TransitionCondition::GreaterThanOrEqual,
            _ => unreachable!("The above should cover all possible cases."),
        };

        condition
    }

    fn parse_leader_transition(&mut self) -> ast::LeaderTransition {
        self.consume(TokenKind::LeftParenthesis, "Expecting left parenthesis.");

        let texts = vec!["in", "out", "local"];
        let index = self.consume_keyword_one_of(texts, "Expecting a transition type.");
        let transition = match self.tokens[index].text.as_str() {
            "in" => {
                let condition = self.parse_leader_transition_condition();
                let index = self.consume(TokenKind::Number, "Expecting a number.");

                match self.tokens[index].text.parse::<usize>() {
                    Ok(number1) => {
                        let option2 = if self.peek(0).unwrap().kind == TokenKind::Number {
                            let index = self.consume(TokenKind::Number, "");
                            match self.tokens[index].text.parse::<usize>() {
                                Ok(number2) => Some(number2),
                                Err(_) => {
                                    error(
                                        "Number too large.",
                                        self.tokens[index].line,
                                        self.tokens[index].column,
                                    );
                                    unreachable!("The above function terminates the program.");
                                }
                            }
                        } else {
                            None
                        };

                        let index = self.consume(TokenKind::Word, "Expecting a message (word).");
                        let message = self.tokens[index].text.to_owned();

                        if let Some(number2) = option2 {
                            ast::LeaderTransition::IngoingWithCondition2(
                                condition, number1, number2, message,
                            )
                        } else {
                            ast::LeaderTransition::IngoingWithCondition1(
                                condition, number1, message,
                            )
                        }
                    }
                    Err(_) => {
                        error(
                            "Number too large.",
                            self.tokens[index].line,
                            self.tokens[index].column,
                        );
                        unreachable!("The above function terminates the program.");
                    }
                }
            }
            "out" => {
                let index = self.consume(TokenKind::Word, "Expecting a message (word).");
                let message = self.tokens[index].text.to_owned();
                ast::LeaderTransition::Outgoing(message)
            }
            "local" => {
                let index = self.consume(TokenKind::Word, "Expecting a local transition (word).");
                let name = self.tokens[index].text.to_owned();
                ast::LeaderTransition::Local(name)
            }
            _ => unreachable!("The above should cover all possible cases."),
        };

        self.consume(TokenKind::RightParenthesis, "Expecting right parenthesis.");

        transition
    }

    fn parse_leader_delta_triple(&mut self) -> (ast::State, ast::LeaderTransition, ast::State) {
        self.consume(TokenKind::LeftParenthesis, "Expecting left parenthesis.");

        let state0 = self.parse_state();
        let transition = self.parse_leader_transition();
        let state1 = self.parse_state();

        self.consume(TokenKind::RightParenthesis, "Expecting right parenthesis.");

        (state0, transition, state1)
    }

    fn parse_leader_delta(&mut self) -> Vec<(ast::State, ast::LeaderTransition, ast::State)> {
        self.consume(TokenKind::LeftParenthesis, "Expecting left parenthesis.");
        self.consume_keyword("delta", "Expecting keyword `delta`.");

        let mut delta = Vec::<(ast::State, ast::LeaderTransition, ast::State)>::new();
        while self.peek(0).unwrap().kind == TokenKind::LeftParenthesis {
            let triple = self.parse_leader_delta_triple();
            delta.push(triple);
        }

        self.consume(TokenKind::RightParenthesis, "Expecting right parenthesis.");

        delta
    }

    fn parse_leader(&mut self) -> ast::Leader {
        self.consume(TokenKind::LeftParenthesis, "Expecting left parenthesis.");
        self.consume_keyword("leader", "Expecting keyword `leader`.");

        let delta = self.parse_leader_delta();
        let initial_states = self.parse_initial_states();

        self.consume(TokenKind::RightParenthesis, "Expecting right parenthesis.");

        let mut states = Vec::<ast::State>::new();
        let mut transitions = Vec::<ast::LeaderTransition>::new();
        transitions.push(ast::LeaderTransition::Idle); // first transition is Idle

        for (state0, transition, state1) in delta.iter() {
            if !states.contains(state0) {
                states.push(state0.to_owned());
            }

            if !transitions.contains(transition) {
                transitions.push(transition.to_owned());
            }

            if !states.contains(state1) {
                states.push(state1.to_owned());
            }
        }

        let mut m_in = Vec::<String>::new();
        let mut m_out = Vec::<String>::new();
        let mut l = Vec::<String>::new();

        for transition in &transitions[1..] {
            match transition {
                ast::LeaderTransition::IngoingWithCondition1(_, _, message)
                | ast::LeaderTransition::IngoingWithCondition2(_, _, _, message) => {
                    if !m_in.contains(message) {
                        m_in.push(message.to_owned())
                    }
                }
                ast::LeaderTransition::Outgoing(message) => {
                    if !m_out.contains(message) {
                        m_out.push(message.to_owned())
                    }
                }
                ast::LeaderTransition::Local(name) => {
                    if !l.contains(name) {
                        l.push(name.to_owned())
                    }
                }
                _ => unreachable!("Idle transition should not show up."),
            }
        }

        let mut pres = Vec::<(ast::State, ast::LeaderTransition)>::new();
        let mut tl = Vec::<ast::State>::new();

        for (state0, transition, state1) in delta.iter() {
            let pair = (state0.to_owned(), transition.to_owned());
            if !pres.contains(&pair) {
                pres.push(pair);
                tl.push(state1.to_owned());
            } else {
                error_plain("Found two triples in delta whose first two components are the same.")
            }
        }

        ast::Leader {
            m_in,
            m_out,
            l,
            states,
            initial_states,
            transitions,
            delta,
            pres,
            tl,
        }
    }

    fn parse_follower_transition(&mut self) -> ast::FollowerTransition {
        self.consume(TokenKind::LeftParenthesis, "Expecting left parenthesis.");

        let texts = vec!["in", "out", "local"];
        let index = self.consume_keyword_one_of(texts, "Expecting a transition type.");

        let transition = match self.tokens[index].text.as_str() {
            "in" => {
                let index = self.consume(TokenKind::Word, "Expecting a message (word).");
                let message = self.tokens[index].text.to_owned();
                ast::FollowerTransition::Ingoing(message)
            }
            "out" => {
                let index = self.consume(TokenKind::Word, "Expecting a message (word).");
                let message = self.tokens[index].text.to_owned();
                ast::FollowerTransition::Outgoing(message)
            }
            "local" => {
                let index = self.consume(TokenKind::Word, "Expecting a local transition (word).");
                let name = self.tokens[index].text.to_owned();
                ast::FollowerTransition::Local(name)
            }
            _ => unreachable!("The above should cover all possible cases."),
        };

        self.consume(TokenKind::RightParenthesis, "Expecting right parenthesis.");

        transition
    }

    fn parse_follower_delta_triple(&mut self) -> (ast::State, ast::FollowerTransition, ast::State) {
        self.consume(TokenKind::LeftParenthesis, "Expecting left parenthesis.");

        let state0 = self.parse_state();
        let transition = self.parse_follower_transition();
        let state1 = self.parse_state();

        self.consume(TokenKind::RightParenthesis, "Expecting right parenthesis.");

        (state0, transition, state1)
    }

    fn parse_follower_delta(&mut self) -> Vec<(ast::State, ast::FollowerTransition, ast::State)> {
        self.consume(TokenKind::LeftParenthesis, "Expecting left parenthesis.");
        self.consume_keyword("delta", "Expecting keyword `delta`.");

        let mut delta = Vec::<(ast::State, ast::FollowerTransition, ast::State)>::new();
        while self.peek(0).unwrap().kind == TokenKind::LeftParenthesis {
            let triple = self.parse_follower_delta_triple();
            delta.push(triple);
        }

        self.consume(TokenKind::RightParenthesis, "Expecting right parenthesis.");

        delta
    }

    fn parse_follower(&mut self) -> ast::Follower {
        self.consume(TokenKind::LeftParenthesis, "Expecting left parenthesis.");
        self.consume_keyword("follower", "Expecting keyword `leader`.");

        let delta = self.parse_follower_delta();
        let initial_states = self.parse_initial_states();

        self.consume(TokenKind::RightParenthesis, "Expecting right parenthesis.");

        let mut states = Vec::<ast::State>::new();
        let mut transitions = Vec::<ast::FollowerTransition>::new();
        transitions.push(ast::FollowerTransition::Idle);

        for (state0, transition, state1) in delta.iter() {
            if !states.contains(state0) {
                states.push(state0.to_owned());
            }

            if !transitions.contains(transition) {
                transitions.push(transition.to_owned());
            }

            if !states.contains(state1) {
                states.push(state1.to_owned());
            }
        }

        let mut m_in = Vec::<String>::new();
        let mut m_out = Vec::<String>::new();
        let mut l = Vec::<String>::new();

        for transition in &transitions[1..] {
            match transition {
                ast::FollowerTransition::Ingoing(message) => {
                    if !m_in.contains(message) {
                        m_in.push(message.to_owned())
                    }
                }
                ast::FollowerTransition::Outgoing(message) => {
                    if !m_out.contains(message) {
                        m_out.push(message.to_owned())
                    }
                }
                ast::FollowerTransition::Local(name) => {
                    if !l.contains(name) {
                        l.push(name.to_owned())
                    }
                }
                _ => unreachable!("Idle transition should not show up."),
            }
        }

        let mut pres = Vec::<(ast::State, ast::FollowerTransition)>::new();
        let mut tl = Vec::<ast::State>::new();

        for (state0, transition, state1) in delta.iter() {
            let pair = (state0.to_owned(), transition.to_owned());
            if !pres.contains(&pair) {
                pres.push(pair);
                tl.push(state1.to_owned());
            } else {
                error_plain("Found two triples in delta whose first two components are the same.")
            }
        }

        ast::Follower {
            m_in,
            m_out,
            l,
            states,
            initial_states,
            transitions,
            delta,
            pres,
            tl,
        }
    }

    fn parse_leader_inbox(&mut self, leader: &ast::Leader) -> ast::ArithmeticTerm {
        self.consume(TokenKind::LeftParenthesis, "Expecting left parenthesis.");
        self.consume_keyword("inbox", "Expecting keyword `inbox`.");

        let index = self.consume(
            TokenKind::Word,
            "Expecting one of the leader's ingoing messages (word).",
        );

        let message = self.tokens[index].text.to_owned();
        if !leader.m_in.contains(&message) {
            error(
                "Unrecognized message",
                self.tokens[index].line,
                self.tokens[index].column,
            )
        }

        let index_in_m_in = leader.m_in.iter().position(|m| *m == message).unwrap();
        let res = ast::ArithmeticTerm::HashInbox(index_in_m_in);

        self.consume(TokenKind::RightParenthesis, "Expecting right parenthesis.");

        res
    }

    fn parse_arithmetic_hash_term(
        &mut self,
        leader: &ast::Leader,
        follower: &ast::Follower,
    ) -> ast::ArithmeticTerm {
        match (self.peek(0), self.peek(1)) {
            (Some(token0), Some(token1))
                if token0.kind == TokenKind::LeftParenthesis && token1.text.as_str() == "inbox" =>
            {
                self.parse_leader_inbox(leader)
            }
            (Some(token0), Some(token1))
                if token0.kind == TokenKind::LeftParenthesis && token1.text.as_str() == "state" =>
            {
                let state = self.parse_state();
                if !follower.states.contains(&state) {
                    error(
                        "Unrecognized state.",
                        self.tokens[self.index - 2].line,
                        self.tokens[self.index - 2].column,
                    )
                }

                // state is in follower.states
                let index_of_state_in_follower_states =
                    follower.states.iter().position(|s| *s == state).unwrap();

                let transition = match self.peek(0) {
                    Some(token) if token.kind == TokenKind::RightParenthesis => None,
                    Some(token) if token.kind == TokenKind::LeftParenthesis => {
                        Some(self.parse_follower_transition())
                    }
                    Some(token) if token.kind != TokenKind::LeftParenthesis => {
                        error("Expecting right parenthesis.", token.line, token.column);
                        unreachable!("The above function terminates the program.");
                    }
                    _ => unreachable!("The above should cover all possible cases."),
                };

                let res = if transition == None {
                    ast::ArithmeticTerm::HashState(index_of_state_in_follower_states)
                } else {
                    if !follower
                        .pres
                        .iter()
                        .any(|(s, t)| s == &state && t == transition.as_ref().unwrap())
                    {
                        error(
                            "No such transition from the last state exists.",
                            self.tokens[self.index - 4].line,
                            self.tokens[self.index - 4].column,
                        )
                    }

                    // transition is in follower.transitions
                    let index_of_transition_in_follower_transitions = follower
                        .transitions
                        .iter()
                        .position(|t| t == transition.as_ref().unwrap())
                        .unwrap();

                    ast::ArithmeticTerm::HashStateTransition(
                        index_of_state_in_follower_states,
                        index_of_transition_in_follower_transitions,
                    )
                };

                res
            }
            (Some(token0), Some(token1))
                if token0.kind == TokenKind::LeftParenthesis
                    && token1.text.as_str() != "inbox"
                    && token1.text.as_str() != "state" =>
            {
                error(
                    "Expecting keyword `inbox` or `state`.",
                    token1.line,
                    token1.column,
                );
                unreachable!("The above function terminates the program.");
            }
            (Some(token0), _) if token0.kind != TokenKind::LeftParenthesis => {
                error("Expecting left parenthesis.", token0.line, token0.column);
                unreachable!("The above function terminates the program.");
            }
            _ => unreachable!("The above should cover all possible cases."),
        }
    }

    fn parse_arithmetic_atomic_term(
        &mut self,
        leader: &ast::Leader,
        follower: &ast::Follower,
    ) -> Option<ast::ArithmeticTerm> {
        match (self.peek(0), self.peek(1)) {
            (Some(token0), _) if token0.text == "K" => {
                self.advance();
                Some(ast::ArithmeticTerm::K)
            }
            (Some(token0), _) if token0.kind == TokenKind::Number => {
                match token0.text.parse::<usize>() {
                    Ok(number) => {
                        self.advance();
                        Some(ast::ArithmeticTerm::NumberLiteral(number))
                    }
                    Err(_) => {
                        error("Number too large.", token0.line, token0.column);
                        unreachable!("The above function terminates the program.");
                    }
                }
            }
            (Some(token0), Some(token1))
                if token0.kind == TokenKind::LeftParenthesis && token1.kind == TokenKind::Hash =>
            {
                self.consume(TokenKind::LeftParenthesis, "Expecting left parenthesis.");
                self.consume(TokenKind::Hash, "");

                let res = self.parse_arithmetic_hash_term(leader, follower);

                self.consume(TokenKind::RightParenthesis, "Expecting right parenthesis.");

                Some(res)
            }
            _ => None,
        }
    }

    fn parse_arithmetic_term(
        &mut self,
        leader: &ast::Leader,
        follower: &ast::Follower,
    ) -> ast::ArithmeticTerm {
        if let Some(atomic_term) = self.parse_arithmetic_atomic_term(leader, follower) {
            atomic_term
        } else {
            self.consume(TokenKind::LeftParenthesis, "Expecting left parenthesis.");

            let kinds = vec![TokenKind::PlusSign, TokenKind::Asterisk];
            let index = self.consume_one_of(
                kinds,
                "Expecting one of: atom (k, number, hash expression); `+`; or `*`.",
            );

            let mut terms = Vec::<ast::ArithmeticTerm>::new();
            while self.peek(0).unwrap().kind != TokenKind::RightParenthesis {
                terms.push(self.parse_arithmetic_term(leader, follower));
            }

            if terms.len() == 0 {
                error(
                    "Empty arithmetic terms not allowed.",
                    self.tokens[self.index].line,
                    self.tokens[self.index].column,
                )
            }

            let res = match self.tokens[index].kind {
                TokenKind::PlusSign => ast::ArithmeticTerm::Add(terms),
                TokenKind::Asterisk => ast::ArithmeticTerm::Mul(terms),
                _ => unreachable!("The above should cover all possible cases."),
            };

            self.consume(TokenKind::RightParenthesis, "Expecting right parenthesis.");

            res
        }
    }

    fn parse_arithmetic_relation(
        &mut self,
        leader: &ast::Leader,
        follower: &ast::Follower,
    ) -> ast::ArithmeticRelation {
        self.consume(TokenKind::LeftParenthesis, "Expecting left parenthesis.");

        let kinds = vec![
            TokenKind::Equal,
            TokenKind::LessThan,
            TokenKind::GreaterThan,
            TokenKind::LessThanOrEqual,
            TokenKind::GreaterThanOrEqual,
        ];
        let index = self.consume_one_of(kinds, "Expecting =, <, >, <=, or =>");

        let mut terms = Vec::<ast::ArithmeticTerm>::new();
        while self.peek(0).unwrap().kind != TokenKind::RightParenthesis {
            terms.push(self.parse_arithmetic_term(leader, follower));
        }

        if terms.len() == 0 {
            error(
                "Empty relations not allowed.",
                self.tokens[self.index].line,
                self.tokens[self.index].column,
            )
        }

        let res = match self.tokens[index].kind {
            TokenKind::Equal => ast::ArithmeticRelation::Equal(terms),
            TokenKind::LessThan => ast::ArithmeticRelation::LessThan(terms),
            TokenKind::GreaterThan => ast::ArithmeticRelation::GreaterThan(terms),
            TokenKind::LessThanOrEqual => ast::ArithmeticRelation::LessThanOrEqual(terms),
            TokenKind::GreaterThanOrEqual => ast::ArithmeticRelation::GreaterThanOrEqual(terms),
            _ => unreachable!("The above should cover all possible cases."),
        };

        self.consume(TokenKind::RightParenthesis, "Expecting right parenthesis.");

        res
    }

    fn parse_non_temporal_property(
        &mut self,
        leader: &ast::Leader,
        follower: &ast::Follower,
    ) -> ast::NonTemporalProperty {
        let texts = vec!["not", "and", "or", "implies", "equivalent"];

        match (self.peek(0), self.peek(1)) {
            (Some(token0), Some(token1))
                if token0.kind == TokenKind::LeftParenthesis
                    && texts.contains(&token1.text.as_str()) =>
            {
                self.consume(TokenKind::LeftParenthesis, "");

                let index = self.consume_keyword_one_of(
                    texts,
                    "Expecting `not`, `and`, `or`, `implies`, or `equivalent`.",
                );

                if self.tokens[index].text.as_str() == "not" {
                    let res = Box::new(self.parse_non_temporal_property(leader, follower));
                    return ast::NonTemporalProperty::Not(res);
                }

                // if we did not return above, we have `and`, `or`, `implies`, or `equivalent`
                let mut terms = Vec::<ast::NonTemporalProperty>::new();
                while self.peek(0).unwrap().kind != TokenKind::RightParenthesis {
                    terms.push(self.parse_non_temporal_property(leader, follower));
                }

                if terms.len() == 0 {
                    error(
                        "Empty `and`, `or`, `implies`, or `equivalent` not allowed.",
                        self.tokens[self.index].line,
                        self.tokens[self.index].column,
                    )
                }

                let res = match self.tokens[index].text.as_str() {
                    "and" => ast::NonTemporalProperty::And(terms),
                    "or" => ast::NonTemporalProperty::Or(terms),
                    "implies" => ast::NonTemporalProperty::Implies(terms),
                    "equivalent" => ast::NonTemporalProperty::Equivalent(terms),
                    _ => unreachable!("The above should cover all possible cases."),
                };

                self.consume(TokenKind::RightParenthesis, "Expecting right parenthesis.");

                res
            }
            _ => ast::NonTemporalProperty::ArithmeticRelation(
                self.parse_arithmetic_relation(leader, follower),
            ),
        }
    }

    fn parse_temporal_property(
        &mut self,
        leader: &ast::Leader,
        follower: &ast::Follower,
    ) -> ast::TemporalProperty {
        self.consume(TokenKind::LeftParenthesis, "Expecting left parenthesis.");

        let index0 = self.consume_keyword_one_of(
            vec!["always", "eventually"],
            "Expecting `always` or `eventually`.",
        );

        let index1 = self.consume(TokenKind::Number, "Expecting a number.");
        let token2 = &self.tokens[index1];

        match token2.text.parse::<usize>() {
            Ok(horizon) => {
                let property = self.parse_non_temporal_property(leader, follower);

                let res = match self.tokens[index0].text.as_str() {
                    "always" => ast::TemporalProperty::Always(horizon, Box::new(property)),
                    "eventually" => ast::TemporalProperty::Eventually(horizon, Box::new(property)),
                    _ => unreachable!("The above should cover all possible cases."),
                };

                self.consume(TokenKind::RightParenthesis, "Expecting right parenthesis.");

                res
            }
            Err(_) => {
                error("Number too large.", token2.line, token2.column);
                unreachable!("The above function terminates the program.");
            }
        }
    }

    fn parse_property(
        &mut self,
        leader: &ast::Leader,
        follower: &ast::Follower,
    ) -> ast::TemporalProperty {
        self.consume(TokenKind::LeftParenthesis, "Expecting left parenthesis.");
        self.consume_keyword("property", "Expecting keyword `property`.");

        let res = self.parse_temporal_property(leader, follower);

        self.consume(TokenKind::RightParenthesis, "Expecting right parenthesis.");

        res
    }

    pub fn parse(&mut self) -> (ast::Leader, ast::Follower, ast::TemporalProperty) {
        self.consume(TokenKind::LeftParenthesis, "Expecting left parenthesis.");

        let leader = self.parse_leader();
        let follower = self.parse_follower();
        let property = self.parse_property(&leader, &follower);

        self.consume(TokenKind::RightParenthesis, "Expecting right parenthesis.");

        (leader, follower, property)
    }
}
