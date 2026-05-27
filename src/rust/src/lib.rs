mod automaton;
mod count;
mod detect;
mod extract;
mod locate;
mod offsets;
mod replace;

use aho_corasick::AhoCorasick;
use extendr_api::prelude::*;
use extendr_api::Result;

// Keep raw byte offsets so locate/extract can reuse one search pass.
pub(crate) struct RawMatch {
    pub(crate) pattern_id: i32,
    pub(crate) start_byte: usize,
    pub(crate) end_byte: usize,
}

pub(crate) fn collect_raw_matches(
    automaton: &AhoCorasick,
    haystack: &str,
    overlapping: bool,
) -> Result<Vec<RawMatch>> {
    let mut raw_matches = Vec::new();

    if overlapping {
        let matches = automaton
            .try_find_overlapping_iter(haystack.as_bytes())
            .map_err(|err| Error::Other(err.to_string()))?;

        for mat in matches {
            raw_matches.push(RawMatch {
                pattern_id: (mat.pattern().as_usize() + 1) as i32,
                start_byte: mat.start(),
                end_byte: mat.end(),
            });
        }
    } else {
        let matches = automaton
            .try_find_iter(haystack.as_bytes())
            .map_err(|err| Error::Other(err.to_string()))?;

        for mat in matches {
            raw_matches.push(RawMatch {
                pattern_id: (mat.pattern().as_usize() + 1) as i32,
                start_byte: mat.start(),
                end_byte: mat.end(),
            });
        }
    }

    Ok(raw_matches)
}

extendr_module! {
    mod ahocorasick;

    use automaton;
    use count;
    use detect;
    use extract;
    use locate;
    use replace;
}
