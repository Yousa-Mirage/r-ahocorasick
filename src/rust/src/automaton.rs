use aho_corasick::{AhoCorasick, AhoCorasickBuilder, AhoCorasickKind, MatchKind};
use extendr_api::prelude::*;
use extendr_api::Result;

// The Aho-Corasick automaton and its metadata.
pub(crate) struct AcAutomaton {
    pub(crate) ac: AhoCorasick,
    pub(crate) patterns_len: usize,
    pub(crate) min_pattern_len: usize,
    pub(crate) max_pattern_len: usize,
    pub(crate) match_kind: MatchKind,
    pub(crate) implementation: AhoCorasickKind,
    pub(crate) ascii_case_insensitive: bool,
    pub(crate) memory_usage: usize,
}

pub(crate) fn parse_match_kind(match_kind: &str) -> MatchKind {
    match match_kind {
        "standard" => MatchKind::Standard,
        "leftmost_first" => MatchKind::LeftmostFirst,
        "leftmost_longest" => MatchKind::LeftmostLongest,
        _ => unreachable!("`match_kind` should have been validated by R"),
    }
}

pub(crate) fn parse_implementation(implementation: &str) -> Option<AhoCorasickKind> {
    match implementation {
        "auto" => None,
        "noncontiguous_nfa" => Some(AhoCorasickKind::NoncontiguousNFA),
        "contiguous_nfa" => Some(AhoCorasickKind::ContiguousNFA),
        "dfa" => Some(AhoCorasickKind::DFA),
        _ => unreachable!("`implementation` should have been validated by R"),
    }
}

pub(crate) fn match_kind_name(match_kind: MatchKind) -> &'static str {
    match match_kind {
        MatchKind::Standard => "standard",
        MatchKind::LeftmostFirst => "leftmost_first",
        MatchKind::LeftmostLongest => "leftmost_longest",
        _ => unreachable!("unsupported MatchKind"),
    }
}

pub(crate) fn implementation_name(implementation: AhoCorasickKind) -> &'static str {
    match implementation {
        AhoCorasickKind::NoncontiguousNFA => "noncontiguous_nfa",
        AhoCorasickKind::ContiguousNFA => "contiguous_nfa",
        AhoCorasickKind::DFA => "dfa",
        _ => unreachable!("unsupported AhoCorasickKind"),
    }
}

// Build an Aho-Corasick automaton.
#[extendr]
pub fn rust_ac_build(
    patterns: Vec<String>,
    match_kind: String,
    implementation: String,
    ascii_case_insensitive: bool,
) -> Result<ExternalPtr<AcAutomaton>> {
    let parsed_match_kind = parse_match_kind(&match_kind);
    let parsed_implementation = parse_implementation(&implementation);

    let ac = AhoCorasickBuilder::new()
        .match_kind(parsed_match_kind)
        .kind(parsed_implementation)
        .ascii_case_insensitive(ascii_case_insensitive)
        .build(patterns.iter().map(String::as_bytes))
        .map_err(|err| Error::Other(err.to_string()))?;

    let automaton = AcAutomaton {
        patterns_len: patterns.len(),
        min_pattern_len: ac.min_pattern_len(),
        max_pattern_len: ac.max_pattern_len(),
        match_kind: parsed_match_kind,
        implementation: ac.kind(),
        ascii_case_insensitive,
        memory_usage: ac.memory_usage(),
        ac,
    };

    Ok(ExternalPtr::new(automaton))
}

// Return whether an external pointer still points to a live Rust automaton.
#[extendr]
pub fn rust_ac_is_valid(ptr: ExternalPtr<AcAutomaton>) -> bool {
    ptr.try_addr().is_ok()
}

// Return automaton metadata.
#[extendr]
pub fn rust_ac_info(ptr: ExternalPtr<AcAutomaton>) -> Result<List> {
    let automaton = ptr.try_addr()?;

    let list = list!(
        patterns_len = automaton.patterns_len,
        min_pattern_len = automaton.min_pattern_len,
        max_pattern_len = automaton.max_pattern_len,
        match_kind = match_kind_name(automaton.match_kind),
        implementation = implementation_name(automaton.implementation),
        ascii_case_insensitive = automaton.ascii_case_insensitive,
        memory_usage = automaton.memory_usage
    );

    Ok(list)
}

extendr_module! {
    mod automaton;
    fn rust_ac_build;
    fn rust_ac_info;
    fn rust_ac_is_valid;
}
