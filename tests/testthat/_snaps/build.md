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

# ac_automaton validation catches malformed objects

    Code
      ac_info("hello")
    Condition
      Error in `validate_ac_automaton()`:
      ! `ac` must be an <ac_automaton> object.
    Code
      ac_info(missing_field)
    Condition
      Error in `validate_ac_automaton()`:
      ! `ac` is not a valid <ac_automaton> object.
      i It must have the fields ptr, patterns, options, and info. Create automatons with `ac_build()`.
    Code
      ac_info(wrong_type)
    Condition
      Error in `validate_ac_automaton()`:
      ! `ac` is not a valid <ac_automaton> object.
      i It must have the fields ptr, patterns, options, and info. Create automatons with `ac_build()`.

# print shows a hint for automatons restored from disk

    Code
      print(ac)
    Output
      <ac_automaton>
      patterns: 2 
      match_kind: standard 
      implementation: dfa 
      case: sensitive 
    Code
      print(restored)
    Output
      <ac_automaton> (invalid pointer, rebuild with ac_build())
    Code
      print(structure(list(ptr = new("externalptr")), class = "ac_automaton"))
    Output
      <ac_automaton> (invalid pointer, rebuild with ac_build())
    Code
      print(structure(list(), class = "ac_automaton"))
    Output
      <ac_automaton> (invalid pointer, rebuild with ac_build())

