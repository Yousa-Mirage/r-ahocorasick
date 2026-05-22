# ac_locate errors when overlapping search is incompatible with match_kind

    Code
      ac_locate(ac, "hello", overlapping = TRUE)
    Condition
      Error in `ac_locate()`:
      ! `overlapping = TRUE` requires `match_kind = "standard"`.
      i Rebuild the automaton with `match_kind = "standard"` to enable overlapping search.

# ac_locate errors when missing documents are disallowed

    Code
      ac_locate(ac, c("hello", NA_character_), na = "error")
    Condition
      Error in `ac_locate()`:
      x `doc` must not contain missing values because `na = "error"`.
      i Use `na = "omit"` to skip missing values.

# ac_locate warns when no patterns match

    Code
      hits <- ac_locate(ac, "world")
    Condition
      Warning:
      No matched patterns found.

