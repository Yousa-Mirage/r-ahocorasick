use std::fs::{self, File};

use extendr_api::prelude::*;
use extendr_api::Result;

use crate::automaton::AcAutomaton;

// Return whether each haystack has at least one match.
#[extendr]
pub fn rust_ac_detect(ptr: ExternalPtr<AcAutomaton>, doc: Vec<String>) -> Result<Vec<bool>> {
    let automaton = ptr.try_addr()?;
    let mut out = Vec::with_capacity(doc.len());

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

// Return whether each file has at least one match.
#[extendr]
pub fn rust_ac_detect_file(ptr: ExternalPtr<AcAutomaton>, path: Vec<String>) -> Result<Vec<bool>> {
    let automaton = ptr.try_addr()?;
    let mut out = Vec::with_capacity(path.len());

    for file_path in &path {
        let haystack = fs::read(file_path)
            .map_err(|err| Error::Other(format!("failed to read `{file_path}`: {err}")))?;
        let detected = automaton
            .ac
            .try_find(haystack.as_slice())
            .map_err(|err| Error::Other(err.to_string()))?
            .is_some();
        out.push(detected);
    }

    Ok(out)
}

// Return whether each file has at least one match (stream).
#[extendr]
pub fn rust_ac_detect_file_stream(
    ptr: ExternalPtr<AcAutomaton>,
    path: Vec<String>,
) -> Result<Vec<bool>> {
    let automaton = ptr.try_addr()?;
    let mut out = Vec::with_capacity(path.len());

    for file_path in &path {
        let file = File::open(file_path)
            .map_err(|err| Error::Other(format!("failed to open `{file_path}`: {err}")))?;
        let mut matches = automaton
            .ac
            .try_stream_find_iter(file)
            .map_err(|err| Error::Other(err.to_string()))?;

        let detected = match matches.next() {
            Some(Ok(_)) => true,
            Some(Err(err)) => return Err(Error::Other(err.to_string())),
            None => false,
        };
        out.push(detected);
    }

    Ok(out)
}

extendr_module! {
    mod detect;
    fn rust_ac_detect;
    fn rust_ac_detect_file;
    fn rust_ac_detect_file_stream;
}
