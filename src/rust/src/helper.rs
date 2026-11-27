//! Small helpers shared by the modules:
//!   - borrowing R strings
//!   - checking if Strings are valid UTF-8
//!   - converting match counts to R integers.

use extendr_api::Result;
use extendr_api::prelude::*;

// Borrow the elements of an R character vector as `&str` without copying them.
pub(crate) fn r_strs(x: &Strings) -> impl Iterator<Item = &str> {
    x.iter().map(|s| &**s)
}

/// Return the 1-based positions of the strings in `x` that are not valid UTF-8,
/// skipping missing values.
/// @noRd
#[extendr]
pub fn rust_invalid_utf8(x: Strings) -> Vec<i32> {
    x.iter()
        .enumerate()
        .filter(|(_, s)| !s.is_na() && simdutf8::basic::from_utf8(s.as_bytes()).is_err())
        .map(|(i, _)| (i + 1) as i32)
        .collect()
}

// Convert a match count to an R integer, failing instead of wrapping around
// when it does not fit.
pub(crate) fn count_to_r_int(count: usize, source: &str) -> Result<i32> {
    i32::try_from(count).map_err(|_| {
        Error::Other(format!(
            "{source} has {count} matches, more than the largest R integer ({})",
            i32::MAX
        ))
    })
}

extendr_module! {
    mod helper;
    fn rust_invalid_utf8;
}

#[cfg(test)]
mod tests {
    use super::count_to_r_int;

    #[test]
    fn counts_up_to_the_largest_r_integer_are_kept() {
        assert_eq!(count_to_r_int(0, "x").ok(), Some(0));
        assert_eq!(count_to_r_int(i32::MAX as usize, "x").ok(), Some(i32::MAX));
    }

    #[test]
    fn larger_counts_are_errors_instead_of_wrapping() {
        assert!(count_to_r_int(i32::MAX as usize + 1, "x").is_err());
    }
}
