//! Entry point of the Rust library behind the R package: declares the modules,
//! registers the R-callable functions with extendr, and collects raw matches
//! for the search functions to share.

mod automaton;
mod count;
mod detect;
mod extract;
mod helper;
mod locate;
mod offsets;
mod replace;

use aho_corasick::{AhoCorasick, Match};
use extendr_api::Result;
use extendr_api::prelude::*;

pub(crate) use crate::helper::*;

/// Call `f` on each match in `haystack` as it is found.
pub(crate) fn for_each_match(
    automaton: &AhoCorasick,
    haystack: &[u8],
    overlapping: bool,
    mut f: impl FnMut(Match) -> Result<()>,
) -> Result<()> {
    if overlapping {
        let matches = automaton
            .try_find_overlapping_iter(haystack)
            .map_err(|err| Error::Other(err.to_string()))?;
        for mat in matches {
            f(mat)?;
        }
    } else {
        let matches = automaton
            .try_find_iter(haystack)
            .map_err(|err| Error::Other(err.to_string()))?;
        for mat in matches {
            f(mat)?;
        }
    }

    Ok(())
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
