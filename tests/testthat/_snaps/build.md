# ac_build errors on duplicate patterns when requested

    Code
      ac_build(c("hello", "hello"), duplicate = "error")
    Condition
      Error in `ac_build()`:
      x `patterns` must not contain duplicates because `duplicate = "error"`.
      i Use `duplicate = "keep"` or `duplicate = "deduplicate"`.

# ac_build errors on invalid pattern vectors

    Code
      ac_build(c("hello", ""))
    Condition
      Error in `ac_build()`:
      ! `patterns` must be a character vector of non-empty strings with no missing values.

# ac_build requires optional arguments to be named

    Code
      ac_build(c("hello"), "leftmost_first")
    Condition
      Error in `ac_build()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = "leftmost_first"
      i Did you forget to name an argument?

# ac_automaton validation catches invalid external pointers

    Code
      ac_info(ac)
    Condition
      Error in `validate_ac_automaton()`:
      ! `ac` contains an invalid Rust external pointer.
      i Rebuild it with `ac_build()`. External pointers cannot be restored across R sessions.

