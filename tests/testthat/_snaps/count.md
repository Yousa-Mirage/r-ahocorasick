# ac_count_file validates overlapping file search

    Code
      ac_count_file(ac_standard, path, stream = TRUE, overlapping = TRUE)
    Condition
      Error in `ac_count_file()`:
      ! `overlapping = TRUE` is only supported when `stream = FALSE`.
      i Use the default non-streaming file search to enable overlapping search.

---

    Code
      ac_count_file(ac_leftmost, path, overlapping = TRUE)
    Condition
      Error in `ac_count_file()`:
      ! `overlapping = TRUE` requires `match_kind = "standard"`.
      i Rebuild the automaton with `match_kind = "standard"` to enable overlapping file search.

# ac_count_file errors when stream search is incompatible with match_kind

    Code
      ac_count_file(ac, path, stream = TRUE)
    Condition
      Error in `ac_count_file()`:
      ! File stream search requires `match_kind = "standard"`.
      i Rebuild the automaton with `match_kind = "standard"` to search files.

# ac_count_file errors on missing paths

    Code
      ac_count_file(ac, c("file.txt", NA_character_))
    Condition
      Error in `ac_count_file()`:
      ! `path` must be a character vector with no missing values.

# ac_count errors when overlapping search is incompatible with match_kind

    Code
      ac_count(ac, "hello", overlapping = TRUE)
    Condition
      Error in `ac_count()`:
      ! `overlapping = TRUE` requires `match_kind = "standard"`.
      i Rebuild the automaton with `match_kind = "standard"` to enable overlapping search.

# ac_count errors when missing documents are disallowed

    Code
      ac_count(ac, c("hello", NA_character_), na = "error")
    Condition
      Error in `ac_count()`:
      x `doc` must not contain missing values because `na = "error"`.
      i Use `na = "keep"` to keep missing values as `NA`.

# ac_count requires optional arguments to be named

    Code
      ac_count(ac, "hello", TRUE, "zero")
    Condition
      Error in `ac_count()`:
      ! `...` must be empty.
      x Problematic arguments:
      * ..1 = TRUE
      * ..2 = "zero"
      i Did you forget to name an argument?

