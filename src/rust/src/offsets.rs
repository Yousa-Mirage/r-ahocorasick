// Represent an R-style character span with 1-based inclusive bounds.
pub(crate) struct RCharRange {
    pub(crate) start: i32,
    pub(crate) end: i32,
}

// Cache only the requested UTF-8 byte offsets for a single haystack.
pub(crate) struct Utf8OffsetMap {
    offsets: Vec<usize>,
    char_indices: Vec<usize>,
}

impl Utf8OffsetMap {
    // Resolve a set of byte offsets by scanning UTF-8 character boundaries once.
    pub(crate) fn for_offsets<I>(haystack: &str, offsets: I) -> Self
    where
        I: IntoIterator<Item = usize>,
    {
        let mut offsets = offsets.into_iter().collect::<Vec<_>>();
        offsets.sort_unstable();
        offsets.dedup();

        let mut char_indices = vec![usize::MAX; offsets.len()];
        let mut next_needed = 0usize;
        let mut terminal_char_index = 0usize;

        // Record every valid character boundary in the haystack once.
        for (char_index, (byte_index, _)) in haystack.char_indices().enumerate() {
            terminal_char_index = char_index + 1;
            next_needed = Self::record_offset(
                &offsets,
                &mut char_indices,
                next_needed,
                byte_index,
                char_index,
            );

            if next_needed >= offsets.len() {
                break;
            }
        }

        // Mark the terminal boundary so exclusive matches at the end still resolve cleanly.
        Self::record_offset(
            &offsets,
            &mut char_indices,
            next_needed,
            haystack.len(),
            terminal_char_index,
        );

        Self {
            offsets,
            char_indices,
        }
    }

    // Convert a byte-range match into the character-range convention used by R.
    pub(crate) fn r_char_range(&self, start_byte: usize, end_byte: usize) -> Option<RCharRange> {
        let start_char = self.char_index(start_byte)?;
        let end_char = self.char_index(end_byte)?;
        if end_char < start_char {
            return None;
        }

        let start = i32::try_from(start_char.checked_add(1)?).ok()?;
        let end = i32::try_from(end_char).ok()?;

        Some(RCharRange { start, end })
    }

    // Record a requested offset when the current byte boundary matches it.
    fn record_offset(
        offsets: &[usize],
        char_indices: &mut [usize],
        mut next_needed: usize,
        byte_index: usize,
        char_index: usize,
    ) -> usize {
        while next_needed < offsets.len() && offsets[next_needed] < byte_index {
            next_needed += 1;
        }

        if next_needed < offsets.len() && offsets[next_needed] == byte_index {
            char_indices[next_needed] = char_index;
            next_needed += 1;
        }

        next_needed
    }

    // Reject any offset that does not land exactly on a UTF-8 character boundary.
    fn char_index(&self, byte_offset: usize) -> Option<usize> {
        let index = self.offsets.binary_search(&byte_offset).ok()?;

        match self.char_indices.get(index).copied() {
            Some(index) if index != usize::MAX => Some(index),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Utf8OffsetMap;

    #[test]
    fn maps_ascii_byte_offsets_to_r_character_offsets() {
        // ASCII offsets should map directly to the same visible character positions.
        let offsets = Utf8OffsetMap::for_offsets("hello world", [6, 11]);

        let range = offsets.r_char_range(6, 11);

        assert_eq!(range.map(|x| (x.start, x.end)), Some((7, 11)));
    }

    #[test]
    fn maps_utf8_byte_offsets_to_r_character_offsets() {
        // Multibyte prefixes should still produce the expected character positions.
        let offsets = Utf8OffsetMap::for_offsets("你好hello世界", [6, 11]);

        let range = offsets.r_char_range(6, 11);

        assert_eq!(range.map(|x| (x.start, x.end)), Some((3, 7)));
    }

    #[test]
    fn handles_match_at_end_of_haystack() {
        // Exclusive byte ends at the end of the haystack should still resolve correctly.
        let offsets = Utf8OffsetMap::for_offsets("你好hello", [6, 11]);

        let range = offsets.r_char_range(6, 11);

        assert_eq!(range.map(|x| (x.start, x.end)), Some((3, 7)));
    }

    #[test]
    fn rejects_offsets_that_are_not_utf8_boundaries() {
        // Interior byte offsets inside a multibyte code point must be rejected.
        let offsets = Utf8OffsetMap::for_offsets("你好", [0, 1, 3, 4]);

        assert!(offsets.r_char_range(1, 3).is_none());
        assert!(offsets.r_char_range(0, 4).is_none());
    }

    #[test]
    fn rejects_offsets_that_were_not_requested() {
        // The map should only answer for offsets requested by current matches.
        let offsets = Utf8OffsetMap::for_offsets("hello", [1, 3]);

        assert!(offsets.r_char_range(0, 5).is_none());
    }
}
