//! Extract matched text and pattern ids from documents and files, with
//! whole-file and streaming search.

use std::fs::{self, File};
use std::io;

use extendr_api::Result;
use extendr_api::prelude::*;

use crate::automaton::AcAutomaton;
use crate::{for_each_match, r_strs};

// With `with_text = FALSE` the functions below return an empty `matches`
// vector. Creating one R string per match is the most expensive part of
// extraction, and for a case-sensitive automaton the matched text always equals
// the pattern, so the R layer can take it from the stored patterns instead.
// Case-insensitive matches can differ in case and need `with_text = TRUE`.

/// Return matched text and pattern ids for each haystack.
/// @noRd
#[extendr]
pub fn rust_ac_extract(
    ptr: ExternalPtr<AcAutomaton>,
    doc: Strings,
    doc_ids: Vec<i32>,
    overlapping: bool,
    with_text: bool,
) -> Result<List> {
    let automaton = ptr.try_addr()?;

    let mut out_doc_id = Vec::new();
    let mut out_pattern_id = Vec::new();
    let mut out_matches = Vec::new();

    for (haystack, doc_id) in r_strs(&doc).zip(doc_ids.iter()) {
        for_each_match(&automaton.ac, haystack.as_bytes(), overlapping, |mat| {
            if with_text {
                let matched = haystack
                    .get(mat.range())
                    .ok_or_else(|| Error::Other("match offsets are not UTF-8 boundaries".into()))?;
                out_matches.push(matched.to_string());
            }
            out_doc_id.push(*doc_id);
            out_pattern_id.push(mat.pattern().as_i32() + 1);
            Ok(())
        })?;
    }

    let list = list!(
        doc_id = out_doc_id,
        pattern_id = out_pattern_id,
        matches = out_matches
    );

    Ok(list)
}

/// Return matched text in each file.
/// @noRd
#[extendr]
pub fn rust_ac_extract_file(
    ptr: ExternalPtr<AcAutomaton>,
    path: Strings,
    overlapping: bool,
    with_text: bool,
) -> Result<List> {
    let automaton = ptr.try_addr()?;

    let mut out_file_id = Vec::new();
    let mut out_pattern_id = Vec::new();
    let mut out_matches = Vec::new();

    for (file_index, file_path) in r_strs(&path).enumerate() {
        let file_id = (file_index + 1) as i32;

        let haystack = fs::read(file_path)
            .map_err(|err| Error::Other(format!("failed to read `{file_path}`: {err}")))?;

        for_each_match(&automaton.ac, &haystack, overlapping, |mat| {
            out_file_id.push(file_id);
            out_pattern_id.push(mat.pattern().as_i32() + 1);
            if !with_text {
                return Ok(());
            }

            let matched_bytes = haystack
                .get(mat.range())
                .ok_or_else(|| Error::Other("match offsets are out of bounds".into()))?;
            let matched = std::str::from_utf8(matched_bytes)
                .map_err(|err| {
                    Error::Other(format!(
                        "failed to extract `{file_path}`: matched bytes are not UTF-8: {err}"
                    ))
                })?
                .to_owned();
            out_matches.push(matched);
            Ok(())
        })?;
    }

    let list = list!(
        file_id = out_file_id,
        pattern_id = out_pattern_id,
        matches = out_matches
    );

    Ok(list)
}

/// Return matched text in each file (stream).
/// @noRd
#[extendr]
pub fn rust_ac_extract_file_stream(
    ptr: ExternalPtr<AcAutomaton>,
    path: Strings,
    with_text: bool,
) -> Result<List> {
    let automaton = ptr.try_addr()?;

    let mut out_file_id = Vec::new();
    let mut out_pattern_id = Vec::new();
    let mut out_matches = Vec::new();

    for (file_index, file_path) in r_strs(&path).enumerate() {
        let file_id = (file_index + 1) as i32;
        let file = File::open(file_path)
            .map_err(|err| Error::Other(format!("failed to open `{file_path}`: {err}")))?;

        automaton
            .ac
            .try_stream_replace_all_with(file, io::sink(), |mat, matched_bytes, _wtr| {
                out_file_id.push(file_id);
                out_pattern_id.push(mat.pattern().as_i32() + 1);
                if !with_text {
                    return Ok(());
                }

                let matched = std::str::from_utf8(matched_bytes)
                    .map_err(|err| {
                        io::Error::new(
                            io::ErrorKind::InvalidData,
                            format!("matched bytes are not UTF-8: {err}"),
                        )
                    })?
                    .to_owned();
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
