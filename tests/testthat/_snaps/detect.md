# ac_detect_file errors when stream search is incompatible with match_kind

    Code
      ac_detect_file(ac, path, stream = TRUE)
    Condition
      Error in `ac_detect_file()`:
      ! File stream search requires `match_kind = "standard"`.
      i Rebuild the automaton with `match_kind = "standard"` to search files.

# ac_detect_file errors on missing paths

    Code
      ac_detect_file(ac, c("file.txt", NA_character_))
    Condition
      Error in `ac_detect_file()`:
      ! `path` must be a character vector with no missing values.

# ac_detect errors when missing documents are disallowed

    Code
      ac_detect(ac, c("hello", NA_character_), na = "error")
    Condition
      Error in `ac_detect()`:
      x `doc` must not contain missing values because `na = "error"`.
      i Use `na = "keep"` to keep missing values as `NA`.

