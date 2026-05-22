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

