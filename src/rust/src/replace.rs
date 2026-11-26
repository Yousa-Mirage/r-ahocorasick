use std::fs::{self, File};
use std::io::{BufWriter, Write};

use extendr_api::Result;
use extendr_api::prelude::*;

use crate::automaton::AcAutomaton;
use crate::r_strs;

// Replace non-overlapping matches in each haystack.
#[extendr]
pub fn rust_ac_replace(
    ptr: ExternalPtr<AcAutomaton>,
    doc: Strings,
    replace_with: Strings,
) -> Result<Vec<String>> {
    let automaton = ptr.try_addr()?;
    let replace_with = r_strs(&replace_with).collect::<Vec<_>>();

    let mut out = Vec::with_capacity(doc.len());
    for haystack in r_strs(&doc) {
        let replaced = automaton
            .ac
            .try_replace_all(haystack, &replace_with)
            .map_err(|err| Error::Other(err.to_string()))?;
        out.push(replaced);
    }

    Ok(out)
}

// Replace non-overlapping matches and write output files.
#[extendr]
pub fn rust_ac_replace_file(
    ptr: ExternalPtr<AcAutomaton>,
    path: Strings,
    output: Strings,
    replace_with: Strings,
) -> Result<Vec<String>> {
    let automaton = ptr.try_addr()?;
    let replace_with = r_strs(&replace_with).collect::<Vec<_>>();

    for (input_path, output_path) in r_strs(&path).zip(r_strs(&output)) {
        let haystack = fs::read(input_path)
            .map_err(|err| Error::Other(format!("failed to read `{input_path}`: {err}")))?;
        let replaced = automaton
            .ac
            .try_replace_all_bytes(haystack.as_slice(), &replace_with)
            .map_err(|err| Error::Other(err.to_string()))?;
        fs::write(output_path, replaced)
            .map_err(|err| Error::Other(format!("failed to write `{output_path}`: {err}")))?;
    }

    Ok(r_strs(&output).map(str::to_owned).collect())
}

// Replace non-overlapping matches and write output files (stream).
#[extendr]
pub fn rust_ac_replace_file_stream(
    ptr: ExternalPtr<AcAutomaton>,
    path: Strings,
    output: Strings,
    replace_with: Strings,
) -> Result<Vec<String>> {
    let automaton = ptr.try_addr()?;
    let replace_with = r_strs(&replace_with).collect::<Vec<_>>();

    for (input_path, output_path) in r_strs(&path).zip(r_strs(&output)) {
        let input = File::open(input_path)
            .map_err(|err| Error::Other(format!("failed to open `{input_path}`: {err}")))?;
        let output_file = File::create(output_path)
            .map_err(|err| Error::Other(format!("failed to create `{output_path}`: {err}")))?;
        let mut writer = BufWriter::new(output_file);

        automaton
            .ac
            .try_stream_replace_all(input, &mut writer, &replace_with)
            .map_err(|err| Error::Other(err.to_string()))?;
        writer
            .flush()
            .map_err(|err| Error::Other(format!("failed to flush `{output_path}`: {err}")))?;
    }

    Ok(r_strs(&output).map(str::to_owned).collect())
}

extendr_module! {
    mod replace;
    fn rust_ac_replace;
    fn rust_ac_replace_file;
    fn rust_ac_replace_file_stream;
}
