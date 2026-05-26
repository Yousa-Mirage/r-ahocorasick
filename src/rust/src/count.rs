use std::fs::{self, File};

use extendr_api::prelude::*;
use extendr_api::Result;

use crate::automaton::AcAutomaton;

// Return the number of matches in each haystack.
#[extendr]
pub fn rust_ac_count(
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

// Return the number of non-overlapping matches in each file.
#[extendr]
pub fn rust_ac_count_file(ptr: ExternalPtr<AcAutomaton>, path: Vec<String>) -> Result<Vec<i32>> {
    let automaton = ptr.try_addr()?;
    let mut out = Vec::with_capacity(path.len());

    for file_path in &path {
        let haystack = fs::read(file_path)
            .map_err(|err| Error::Other(format!("failed to read `{file_path}`: {err}")))?;
        let matches = automaton
            .ac
            .try_find_iter(haystack.as_slice())
            .map_err(|err| Error::Other(err.to_string()))?;

        out.push(matches.count() as i32);
    }

    Ok(out)
}

// Return the number of non-overlapping matches in each file (stream).
#[extendr]
pub fn rust_ac_count_file_stream(
    ptr: ExternalPtr<AcAutomaton>,
    path: Vec<String>,
) -> Result<Vec<i32>> {
    let automaton = ptr.try_addr()?;
    let mut out = Vec::with_capacity(path.len());

    for file_path in &path {
        let file = File::open(file_path)
            .map_err(|err| Error::Other(format!("failed to open `{file_path}`: {err}")))?;
        let mut matches = automaton
            .ac
            .try_stream_find_iter(file)
            .map_err(|err| Error::Other(err.to_string()))?;

        let count = matches.try_fold(0usize, |count, mat| {
            mat.map(|_| count + 1)
                .map_err(|err| Error::Other(err.to_string()))
        })?;

        out.push(count as i32);
    }

    Ok(out)
}

extendr_module! {
    mod count;
    fn rust_ac_count;
    fn rust_ac_count_file;
    fn rust_ac_count_file_stream;
}
