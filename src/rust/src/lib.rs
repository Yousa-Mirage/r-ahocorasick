mod offsets;

use aho_corasick::{AhoCorasick, AhoCorasickBuilder, AhoCorasickKind, MatchKind};
use extendr_api::prelude::*;
use extendr_api::Result;

use offsets::Utf8OffsetMap;

// The Aho-Corasick automaton and its metadata
struct AcAutomaton {
    ac: AhoCorasick,
    patterns_len: usize,
    min_pattern_len: usize,
    max_pattern_len: usize,
    match_kind: MatchKind,
    implementation: AhoCorasickKind,
    ascii_case_insensitive: bool,
    memory_usage: usize,
}

// Keep raw byte offsets so locate can convert only the offsets it needs.
struct RawMatch {
    pattern_id: i32,
    start_byte: usize,
    end_byte: usize,
}

// Parse the R-facing match kind string into the Rust enum used by the builder.
fn parse_match_kind(match_kind: &str) -> MatchKind {
    match match_kind {
        "standard" => MatchKind::Standard,
        "leftmost_first" => MatchKind::LeftmostFirst,
        "leftmost_longest" => MatchKind::LeftmostLongest,
        _ => unreachable!("`match_kind` should have been validated by R"),
    }
}

// Parse the optional implementation selector into the Rust enum used by the builder.
fn parse_implementation(implementation: &str) -> Option<AhoCorasickKind> {
    match implementation {
        "auto" => None,
        "noncontiguous_nfa" => Some(AhoCorasickKind::NoncontiguousNFA),
        "contiguous_nfa" => Some(AhoCorasickKind::ContiguousNFA),
        "dfa" => Some(AhoCorasickKind::DFA),
        _ => unreachable!("`implementation` should have been validated by R"),
    }
}

// Convert the Rust match kind back into the string exposed to R.
fn match_kind_name(match_kind: MatchKind) -> &'static str {
    match match_kind {
        MatchKind::Standard => "standard",
        MatchKind::LeftmostFirst => "leftmost_first",
        MatchKind::LeftmostLongest => "leftmost_longest",
        _ => unreachable!("unsupported MatchKind"),
    }
}

// Convert the Rust implementation kind back into the string exposed to R.
fn implementation_name(implementation: AhoCorasickKind) -> &'static str {
    match implementation {
        AhoCorasickKind::NoncontiguousNFA => "noncontiguous_nfa",
        AhoCorasickKind::ContiguousNFA => "contiguous_nfa",
        AhoCorasickKind::DFA => "dfa",
        _ => unreachable!("unsupported AhoCorasickKind"),
    }
}

// Build an Aho-Corasick automaton.
#[extendr]
fn rust_ac_build(
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

// Return matches with R-style character offsets for UTF-8 strings.
#[extendr]
fn rust_ac_locate(
    ptr: ExternalPtr<AcAutomaton>,
    doc: Vec<String>,
    doc_ids: Vec<i32>,
    overlapping: bool,
) -> Result<List> {
    let automaton = ptr.try_addr()?;

    let mut out_doc_id = Vec::new();
    let mut out_pattern_id = Vec::new();
    let mut out_start = Vec::new();
    let mut out_end = Vec::new();

    // Search each haystack independently.
    for (haystack, doc_id) in doc.iter().zip(doc_ids.iter()) {
        let mut raw_matches = Vec::new();

        if overlapping {
            let matches = automaton
                .ac
                .try_find_overlapping_iter(haystack.as_bytes())
                .map_err(|err| Error::Other(err.to_string()))?;

            for mat in matches {
                // Store byte offsets first so conversion can be batched for this haystack.
                raw_matches.push(RawMatch {
                    pattern_id: (mat.pattern().as_usize() + 1) as i32,
                    start_byte: mat.start(),
                    end_byte: mat.end(),
                });
            }
        } else {
            let matches = automaton
                .ac
                .try_find_iter(haystack.as_bytes())
                .map_err(|err| Error::Other(err.to_string()))?;

            for mat in matches {
                // Store byte offsets first so conversion can be batched for this haystack.
                raw_matches.push(RawMatch {
                    pattern_id: (mat.pattern().as_usize() + 1) as i32,
                    start_byte: mat.start(),
                    end_byte: mat.end(),
                });
            }
        }

        // Build a compact lookup table for only the offsets returned by this search.
        let offset_map = Utf8OffsetMap::for_offsets(
            haystack,
            raw_matches
                .iter()
                .flat_map(|raw_match| [raw_match.start_byte, raw_match.end_byte]),
        );

        for raw_match in raw_matches {
            // Convert byte offsets into the 1-based inclusive character range expected by R.
            let range = offset_map
                .r_char_range(raw_match.start_byte, raw_match.end_byte)
                .ok_or_else(|| Error::Other("match offsets are not UTF-8 boundaries".into()))?;
            out_doc_id.push(*doc_id);
            out_pattern_id.push(raw_match.pattern_id);
            out_start.push(range.start);
            out_end.push(range.end);
        }
    }

    let list = list!(
        doc_id = out_doc_id,
        pattern_id = out_pattern_id,
        start = out_start,
        end = out_end
    );

    Ok(list)
}

// Return whether each haystack has at least one match.
#[extendr]
fn rust_ac_detect(ptr: ExternalPtr<AcAutomaton>, doc: Vec<String>) -> Result<Vec<bool>> {
    let automaton = ptr.try_addr()?;
    let mut out = Vec::with_capacity(doc.len());

    // Stop after the first match in each haystack.
    for haystack in &doc {
        let detected = automaton
            .ac
            .try_find(haystack.as_bytes())
            .map_err(|err| Error::Other(err.to_string()))?
            .is_some();
        out.push(detected);
    }

    Ok(out)
}

// Return the number of matches in each haystack.
#[extendr]
fn rust_ac_count(
    ptr: ExternalPtr<AcAutomaton>,
    doc: Vec<String>,
    overlapping: bool,
) -> Result<Vec<i32>> {
    let automaton = ptr.try_addr()?;
    let mut out = Vec::with_capacity(doc.len());

    for haystack in &doc {
        let count = if overlapping {
            let matches = automaton
                .ac
                .try_find_overlapping_iter(haystack.as_bytes())
                .map_err(|err| Error::Other(err.to_string()))?;
            matches.count()
        } else {
            let matches = automaton
                .ac
                .try_find_iter(haystack.as_bytes())
                .map_err(|err| Error::Other(err.to_string()))?;
            matches.count()
        };

        out.push(count as i32);
    }

    Ok(out)
}

// Return automaton metadata.
#[extendr]
fn rust_ac_info(ptr: ExternalPtr<AcAutomaton>) -> Result<List> {
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
    mod ahocorasick;
    fn rust_ac_build;
    fn rust_ac_locate;
    fn rust_ac_detect;
    fn rust_ac_count;
    fn rust_ac_info;
}
