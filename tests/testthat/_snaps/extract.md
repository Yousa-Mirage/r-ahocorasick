# ac_extract_file errors when stream search is incompatible with match_kind

    Code
      ac_extract_file(ac, path, stream = TRUE)
    Condition
      Error in `ac_extract_file()`:
      ! File stream search requires `match_kind = "standard"`.
      i Rebuild the automaton with `match_kind = "standard"` to search files.

# ac_extract errors when overlapping search is incompatible with match_kind

    Code
      ac_extract(ac, "hello", overlapping = TRUE)
    Condition
      Error in `ac_extract()`:
      ! `overlapping = TRUE` requires `match_kind = "standard"`.
      i Rebuild the automaton with `match_kind = "standard"` to enable overlapping search.

---

    Code
      ac_extract_df(ac, "hello", overlapping = TRUE)
    Condition
      Error in `ac_extract_df()`:
      ! `overlapping = TRUE` requires `match_kind = "standard"`.
      i Rebuild the automaton with `match_kind = "standard"` to enable overlapping search.

# ac_extract errors when missing documents are disallowed

    Code
      ac_extract(ac, c("hello", NA_character_), na = "error")
    Condition
      Error in `ac_extract()`:
      x `doc` must not contain missing values because `na = "error"`.
      i Use `na = "keep"` to keep missing values as `NA`.

---

    Code
      ac_extract_df(ac, c("hello", NA_character_), na = "error")
    Condition
      Error in `ac_extract_df()`:
      x `doc` must not contain missing values because `na = "error"`.
      i Use `na = "omit"` to skip missing values.

