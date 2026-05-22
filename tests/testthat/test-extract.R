test_that("ac_extract returns matches and patterns per document", {
  ac <- ac_build(c(greeting = "hello", object = "world"))
  doc <- c(a = "hello world hello", b = "nothing", c = NA_character_)

  hits <- ac_extract(ac, doc)

  expect_named(hits, names(doc))
  expect_equal(
    hits[[1]],
    list(
      matches = c("hello", "world", "hello"),
      patterns = c("hello", "world", "hello")
    )
  )
  expect_equal(hits[[2]], list(matches = character(), patterns = character()))
  expect_equal(hits[[3]], list(matches = NA_character_, patterns = NA_character_))
})

test_that("ac_extract distinguishes matched text from pattern values", {
  ac <- ac_build("abc", ascii_case_insensitive = TRUE)

  hits <- ac_extract(ac, "ABC")

  expect_equal(hits[[1]], list(matches = "ABC", patterns = "abc"))
})

test_that("ac_extract supports overlapping and UTF-8 matches", {
  ac <- ac_build(c("aba", "bab", "你a😀"))
  doc <- c("ababa", "xx你a😀")

  hits <- ac_extract(ac, doc, overlapping = TRUE)

  expect_equal(
    hits[[1]],
    list(
      matches = c("aba", "bab", "aba"),
      patterns = c("aba", "bab", "aba")
    )
  )
  expect_equal(hits[[2]], list(matches = "你a😀", patterns = "你a😀"))
})

test_that("ac_extract can treat missing documents as empty matches", {
  ac <- ac_build("hello")

  hits <- ac_extract(ac, c("hello", NA_character_, "world"), na = "empty")

  expect_equal(hits[[1]], list(matches = "hello", patterns = "hello"))
  expect_equal(hits[[2]], list(matches = character(), patterns = character()))
  expect_equal(hits[[3]], list(matches = character(), patterns = character()))
})

test_that("ac_extract errors when overlapping search is incompatible with match_kind", {
  ac <- ac_build("hello", match_kind = "leftmost_first")

  expect_snapshot(
    error = TRUE,
    ac_extract(ac, "hello", overlapping = TRUE)
  )
})

test_that("ac_extract errors when missing documents are disallowed", {
  ac <- ac_build("hello")

  expect_snapshot(
    error = TRUE,
    ac_extract(ac, c("hello", NA_character_), na = "error")
  )
})
