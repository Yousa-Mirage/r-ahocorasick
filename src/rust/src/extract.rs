use std::fs::{self, File};
use std::io;

use extendr_api::prelude::*;
use extendr_api::Result;

use crate::automaton::AcAutomaton;
use crate::collect_raw_matches;

// Return matched text and pattern ids for each haystack.
#[extendr]
pub fn rust_ac_extract(
    ptr: ExternalPtr<AcAutomaton>,
    doc: Vec<String>,
    doc_ids: Vec<i32>,
    overlapping: bool,
) -> Result<List> {
    let automaton = ptr.try_addr()?;

    let mut out_doc_id = Vec::new();
    let mut out_pattern_id = Vec::new();
    let mut out_matches = Vec::new();

    for (haystack, doc_id) in doc.iter().zip(doc_ids.iter()) {
        let raw_matches = collect_raw_matches(&automaton.ac, haystack, overlapping)?;

        for raw_match in raw_matches {
            let matched = haystack
                .get(raw_match.start_byte..raw_match.end_byte)
                .ok_or_else(|| Error::Other("match offsets are not UTF-8 boundaries".into()))?;
            out_doc_id.push(*doc_id);
            out_pattern_id.push(raw_match.pattern_id);
            out_matches.push(matched.to_string());
        }
    }

    let list = list!(
        doc_id = out_doc_id,
        pattern_id = out_pattern_id,
        matches = out_matches
    );

    Ok(list)
}

// Return matched text in each file.
#[extendr]
pub fn rust_ac_extract_file(
    ptr: ExternalPtr<AcAutomaton>,
    path: Vec<String>,
    overlapping: bool,
) -> Result<List> {
    let automaton = ptr.try_addr()?;

    let mut out_file_id = Vec::new();
    let mut out_pattern_id = Vec::new();
    let mut out_matches = Vec::new();

    for (file_index, file_path) in path.iter().enumerate() {
        let file_id = (file_index + 1) as i32;
        let haystack = fs::read_to_string(file_path)
            .map_err(|err| Error::Other(format!("failed to read `{file_path}`: {err}")))?;

        let raw_matches = collect_raw_matches(&automaton.ac, &haystack, overlapping)?;

        for raw_match in raw_matches {
            let matched = haystack
                .get(raw_match.start_byte..raw_match.end_byte)
                .ok_or_else(|| Error::Other("match offsets are not UTF-8 boundaries".into()))?
                .to_owned();

            out_file_id.push(file_id);
            out_pattern_id.push(raw_match.pattern_id);
            out_matches.push(matched);
        }
    }

    let list = list!(
        file_id = out_file_id,
        pattern_id = out_pattern_id,
        matches = out_matches
    );

    Ok(list)
}

// Return matched text in each file (stream).
#[extendr]
pub fn rust_ac_extract_file_stream(
    ptr: ExternalPtr<AcAutomaton>,
    path: Vec<String>,
) -> Result<List> {
    let automaton = ptr.try_addr()?;

    let mut out_file_id = Vec::new();
    let mut out_pattern_id = Vec::new();
    let mut out_matches = Vec::new();

    for (file_index, file_path) in path.iter().enumerate() {
        let file_id = (file_index + 1) as i32;
        let file = File::open(file_path)
            .map_err(|err| Error::Other(format!("failed to open `{file_path}`: {err}")))?;

        automaton
            .ac
            .try_stream_replace_all_with(file, io::sink(), |mat, matched_bytes, _wtr| {
                let pattern_id = mat.pattern().as_i32() + 1;
                let matched = std::str::from_utf8(matched_bytes)
                    .map_err(|err| {
                        io::Error::new(
                            io::ErrorKind::InvalidData,
                            format!("matched bytes are not UTF-8: {err}"),
                        )
                    })?
                    .to_owned();

                out_file_id.push(file_id);
                out_pattern_id.push(pattern_id);
                out_matches.push(matched);

                Ok(())
            })
            .map_err(|err| {
                Error::Other(format!("failed to stream extract `{file_path}`: {err}"))
            })?;
    }

    let list = list!(
        file_id = out_file_id,
        pattern_id = out_pattern_id,
        matches = out_matches
    );

    Ok(list)
}

extendr_module! {
    mod extract;
    fn rust_ac_extract;
    fn rust_ac_extract_file;
    fn rust_ac_extract_file_stream;
}
