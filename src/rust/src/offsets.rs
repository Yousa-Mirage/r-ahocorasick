// Represent an R-style character span with 1-based inclusive bounds.
pub(crate) struct RCharRange {
    pub(crate) start: i32,
    pub(crate) end: i32,
}

// Cache the UTF-8 byte-to-character mapping for a single haystack.
pub(crate) struct Utf8OffsetMap {
    byte_to_char: Vec<usize>,
}

impl Utf8OffsetMap {
    // Build a lookup table from UTF-8 byte boundaries to character indices.
    pub(crate) fn new(haystack: &str) -> Self {
        let mut byte_to_char = vec![usize::MAX; haystack.len() + 1];
        let mut next_char_index = 0usize;

        // Record every valid character boundary in the haystack once.
        for (char_index, (byte_index, _)) in haystack.char_indices().enumerate() {
            byte_to_char[byte_index] = char_index;
            next_char_index = char_index + 1;
        }
        // Mark the terminal boundary so exclusive matches at the end still resolve cleanly.
        byte_to_char[haystack.len()] = next_char_index;

        Self { byte_to_char }
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

    // Reject any offset that does not land exactly on a UTF-8 character boundary.
    fn char_index(&self, byte_offset: usize) -> Option<usize> {
        match self.byte_to_char.get(byte_offset).copied() {
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
        let offsets = Utf8OffsetMap::new("hello world");

        let range = offsets.r_char_range(6, 11);

        assert_eq!(range.map(|x| (x.start, x.end)), Some((7, 11)));
    }

    #[test]
    fn maps_utf8_byte_offsets_to_r_character_offsets() {
        // Multibyte prefixes should still produce the expected character positions.
        let offsets = Utf8OffsetMap::new("你好hello世界");

        let range = offsets.r_char_range(6, 11);

        assert_eq!(range.map(|x| (x.start, x.end)), Some((3, 7)));
    }

    #[test]
    fn handles_match_at_end_of_haystack() {
        // Exclusive byte ends at the end of the haystack should still resolve correctly.
        let offsets = Utf8OffsetMap::new("你好hello");

        let range = offsets.r_char_range(6, 11);

        assert_eq!(range.map(|x| (x.start, x.end)), Some((3, 7)));
    }

    #[test]
    fn rejects_offsets_that_are_not_utf8_boundaries() {
        // Interior byte offsets inside a multibyte code point must be rejected.
        let offsets = Utf8OffsetMap::new("你好");

        assert!(offsets.r_char_range(1, 3).is_none());
        assert!(offsets.r_char_range(0, 4).is_none());
    }
}
