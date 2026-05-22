# ac_detect errors when missing documents are disallowed

    Code
      ac_detect(ac, c("hello", NA_character_), na = "error")
    Condition
      Error in `ac_detect()`:
      x `doc` must not contain missing values because `na = "error"`.
      i Use `na = "keep"` to keep missing values as `NA`.

