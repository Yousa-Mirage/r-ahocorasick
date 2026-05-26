use std::fs;

use extendr_api::prelude::*;
use extendr_api::Result;

use crate::automaton::AcAutomaton;
use crate::collect_raw_matches;
use crate::offsets::Utf8OffsetMap;

// Return matches with R-style character offsets for UTF-8 strings.
#[extendr]
pub fn rust_ac_locate(
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

    for (haystack, doc_id) in doc.iter().zip(doc_ids.iter()) {
        let raw_matches = collect_raw_matches(&automaton.ac, haystack, overlapping)?;
        let offset_map = Utf8OffsetMap::for_offsets(
            haystack,
            raw_matches
                .iter()
                .flat_map(|raw_match| [raw_match.start_byte, raw_match.end_byte]),
        );

        for raw_match in raw_matches {
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

// Return matches with Rust byte offsets for UTF-8 strings.
#[extendr]
pub fn rust_ac_locate_bytes(
    ptr: ExternalPtr<AcAutomaton>,
    doc: Vec<String>,
    doc_ids: Vec<i32>,
    overlapping: bool,
) -> Result<List> {
    let automaton = ptr.try_addr()?;

    let mut out_doc_id = Vec::new();
    let mut out_pattern_id = Vec::new();
    let mut out_byte_start = Vec::new();
    let mut out_byte_end = Vec::new();

    for (haystack, doc_id) in doc.iter().zip(doc_ids.iter()) {
        let raw_matches = collect_raw_matches(&automaton.ac, haystack, overlapping)?;

        for raw_match in raw_matches {
            out_doc_id.push(*doc_id);
            out_pattern_id.push(raw_match.pattern_id);
            out_byte_start.push(raw_match.start_byte);
            out_byte_end.push(raw_match.end_byte);
        }
    }

    let list = list!(
        doc_id = out_doc_id,
        pattern_id = out_pattern_id,
        byte_start = out_byte_start,
        byte_end = out_byte_end
    );

    Ok(list)
}

// Return matches with R-style character offsets for UTF-8 files.
#[extendr]
pub fn rust_ac_locate_file(ptr: ExternalPtr<AcAutomaton>, path: Vec<String>) -> Result<List> {
    let automaton = ptr.try_addr()?;

    let mut out_file_id = Vec::new();
    let mut out_pattern_id = Vec::new();
    let mut out_start = Vec::new();
    let mut out_end = Vec::new();

    for (file_index, file_path) in path.iter().enumerate() {
        let file_id = file_index + 1;
        let haystack = fs::read_to_string(file_path)
            .map_err(|err| Error::Other(format!("failed to read `{file_path}`: {err}")))?;

        let raw_matches = collect_raw_matches(&automaton.ac, &haystack, false)?;
        let offset_map = Utf8OffsetMap::for_offsets(
            &haystack,
            raw_matches
                .iter()
                .flat_map(|raw_match| [raw_match.start_byte, raw_match.end_byte]),
        );

        for raw_match in raw_matches {
            let range = offset_map
                .r_char_range(raw_match.start_byte, raw_match.end_byte)
                .ok_or_else(|| Error::Other("match offsets are not UTF-8 boundaries".into()))?;
            out_file_id.push(file_id);
            out_pattern_id.push(raw_match.pattern_id);
            out_start.push(range.start);
            out_end.push(range.end);
        }
    }

    let list = list!(
        file_id = out_file_id,
        pattern_id = out_pattern_id,
        start = out_start,
        end = out_end
    );

    Ok(list)
}

extendr_module! {
    mod locate;
    fn rust_ac_locate;
    fn rust_ac_locate_bytes;
    fn rust_ac_locate_file;
}
