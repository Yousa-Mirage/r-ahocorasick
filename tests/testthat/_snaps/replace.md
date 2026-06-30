# ac_replace_file errors on invalid output paths

    Code
      ac_replace_file(ac, paths, "x", output = tempfile())
    Condition
      Error in `ac_replace_file()`:
      ! `output` must have the same length as `path`.

# ac_replace_file errors when stream search is incompatible with match_kind

    Code
      ac_replace_file(ac, path, "x", stream = TRUE)
    Condition
      Error in `ac_replace_file()`:
      ! File stream search requires `match_kind = "standard"`.
      i Rebuild the automaton with `match_kind = "standard"` to search files.

# ac_replace errors on invalid replacements

    Code
      ac_replace(ac, "hello", c("x", "y", "z"))
    Condition
      Error in `ac_replace()`:
      x `replace_with` must have length 1 or the same length as `ac_patterns()`.
      i Use one replacement for all patterns, or one replacement per pattern.

---

    Code
      ac_replace(ac, "hello", c("x", NA_character_))
    Condition
      Error in `ac_replace()`:
      ! `replace_with` must be a character vector with no missing values.

# ac_replace errors when missing documents are disallowed

    Code
      ac_replace(ac, c("hello", NA_character_), "x", na = "error")
    Condition
      Error in `ac_replace()`:
      x `doc` must not contain missing values because `na = "error"`.
      i Use `na = "keep"` to keep missing values as `NA`.

# ac_replace_file requires optional arguments to be named

    Code
      ac_replace_file(ac, path, "x", output)
    Condition
      Error in `ac_replace_file()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = output
      i Did you forget to name an argument?

