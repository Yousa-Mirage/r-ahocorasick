# invalid UTF-8 errors name the argument and positions

    Code
      ac_locate(ac, c("hello", invalid_utf8(), NA_character_))
    Condition
      Error in `ac_locate()`:
      ! `doc` must contain valid UTF-8 strings.
      x Invalid UTF-8 found at position 2.
      i Re-encode the text with `iconv()`, or read it with the correct `encoding`.
    Code
      ac_count(ac, rep(invalid_utf8(), 7))
    Condition
      Error in `ac_count()`:
      ! `doc` must contain valid UTF-8 strings.
      x Invalid UTF-8 found at positions 1, 2, 3, ..., 6, and 7.
      i Re-encode the text with `iconv()`, or read it with the correct `encoding`.
    Code
      ac_build(c("ok", invalid_utf8()))
    Condition
      Error in `ac_build()`:
      ! `patterns` must contain valid UTF-8 strings.
      x Invalid UTF-8 found at position 2.
      i Re-encode the text with `iconv()`, or read it with the correct `encoding`.
    Code
      ac_replace(ac, "hello", c("X", invalid_utf8()))
    Condition
      Error in `ac_replace()`:
      ! `replace_with` must contain valid UTF-8 strings.
      x Invalid UTF-8 found at position 2.
      i Re-encode the text with `iconv()`, or read it with the correct `encoding`.

# file functions reject paths with invalid UTF-8

    Code
      ac_count_file(ac, invalid_utf8())
    Condition
      Error in `ac_count_file()`:
      ! `path` must contain valid UTF-8 strings.
      x Invalid UTF-8 found at position 1.
      i Re-encode the text with `iconv()`, or read it with the correct `encoding`.

