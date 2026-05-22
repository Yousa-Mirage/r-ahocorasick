test_that("ac_detect reports whether each document has any match", {
  ac <- ac_build(c("hello", "world"))
  doc <- c(a = "hello there", b = "nothing", c = NA_character_, d = "world")

  expect_equal(
    ac_detect(ac, doc),
    c(a = TRUE, b = FALSE, c = NA, d = TRUE)
  )
})

test_that("ac_detect can treat missing documents as unmatched", {
  ac <- ac_build("hello")

  expect_equal(
    ac_detect(ac, c("hello", NA_character_, "world"), na = "false"),
    c(TRUE, FALSE, FALSE)
  )
})

test_that("ac_detect supports UTF-8 matching", {
  ac_utf8 <- ac_build(c("你a😀", "后缀"))

  expect_equal(ac_detect(ac_utf8, c("xx你a😀", "前缀x😀中后缀")), c(TRUE, TRUE))
})

test_that("ac_detect supports case-insensitive matching", {
  ac_case <- ac_build("abc", ascii_case_insensitive = TRUE)

  expect_equal(ac_detect(ac_case, c("ABC", "xyz", "aBc")), c(TRUE, FALSE, TRUE))
})

test_that("ac_detect errors when missing documents are disallowed", {
  ac <- ac_build("hello")

  expect_snapshot(
    error = TRUE,
    ac_detect(ac, c("hello", NA_character_), na = "error")
  )
})
