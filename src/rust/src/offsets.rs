//! Convert UTF-8 byte offsets of matches into R's 1-based character positions.

/// Represent an R-style character span with 1-based inclusive bounds.
pub(crate) struct RCharRange {
    pub(crate) start: i32,
    pub(crate) end: i32,
}

/// Convert byte offsets in one haystack to character indices by counting the
/// characters between the previous offset and the next one.
pub(crate) struct CharCursor<'a> {
    haystack: &'a str,
    byte: usize,
    chars: usize,
}

impl<'a> CharCursor<'a> {
    pub(crate) fn new(haystack: &'a str) -> Self {
        Self {
            haystack,
            byte: 0,
            chars: 0,
        }
    }

    /// Convert a byte-range match into the character-range convention used by R.
    pub(crate) fn r_char_range(
        &mut self,
        start_byte: usize,
        end_byte: usize,
    ) -> Option<RCharRange> {
        let start_char = self.char_index(start_byte)?;
        let end_char = self.char_index(end_byte)?;
        if end_char < start_char {
            return None;
        }

        let start = i32::try_from(start_char.checked_add(1)?).ok()?;
        let end = i32::try_from(end_char).ok()?;

        Some(RCharRange { start, end })
    }

    /// Return the number of characters before `byte`, or `None` if `byte` is not
    /// a character boundary of the haystack.
    fn char_index(&mut self, byte: usize) -> Option<usize> {
        if !self.haystack.is_char_boundary(byte) {
            return None;
        }

        let bytes = self.haystack.as_bytes();
        if byte >= self.byte {
            self.chars += count_chars(&bytes[self.byte..byte]);
        } else {
            self.chars -= count_chars(&bytes[byte..self.byte]);
        }
        self.byte = byte;

        Some(self.chars)
    }
}

/// Count the characters in valid UTF-8 bytes: every byte that is not a
/// continuation byte (`0b10xx_xxxx`) starts a character.
fn count_chars(bytes: &[u8]) -> usize {
    bytes.iter().filter(|&&b| (b as i8) >= -0x40).count()
}

#[cfg(test)]
mod tests {
    use super::CharCursor;

    fn range(cursor: &mut CharCursor, start: usize, end: usize) -> Option<(i32, i32)> {
        cursor.r_char_range(start, end).map(|x| (x.start, x.end))
    }

    #[test]
    fn maps_ascii_byte_offsets_to_r_character_offsets() {
        // ASCII offsets should map directly to the same visible character positions.
        let mut cursor = CharCursor::new("hello world");

        assert_eq!(range(&mut cursor, 6, 11), Some((7, 11)));
    }

    #[test]
    fn maps_utf8_byte_offsets_to_r_character_offsets() {
        // Multibyte prefixes should still produce the expected character positions.
        let mut cursor = CharCursor::new("你好hello世界");

        assert_eq!(range(&mut cursor, 6, 11), Some((3, 7)));
        assert_eq!(range(&mut cursor, 11, 17), Some((8, 9)));
    }

    #[test]
    fn handles_match_at_end_of_haystack() {
        // Exclusive byte ends at the end of the haystack should still resolve correctly.
        let mut cursor = CharCursor::new("你好hello");

        assert_eq!(range(&mut cursor, 6, 11), Some((3, 7)));
    }

    #[test]
    fn moves_back_for_overlapping_matches() {
        // Overlapping matches can start before the previous match ended.
        let mut cursor = CharCursor::new("好😀好😀");

        assert_eq!(range(&mut cursor, 0, 7), Some((1, 2)));
        assert_eq!(range(&mut cursor, 3, 10), Some((2, 3)));
        assert_eq!(range(&mut cursor, 0, 14), Some((1, 4)));
    }

    #[test]
    fn rejects_offsets_that_are_not_utf8_boundaries() {
        // Interior byte offsets inside a multibyte code point must be rejected.
        let mut cursor = CharCursor::new("你好");

        assert!(cursor.r_char_range(1, 3).is_none());
        assert!(cursor.r_char_range(0, 4).is_none());
        assert!(cursor.r_char_range(0, 7).is_none());
        // A rejected offset leaves the cursor usable.
        assert_eq!(range(&mut cursor, 3, 6), Some((2, 2)));
    }
}
