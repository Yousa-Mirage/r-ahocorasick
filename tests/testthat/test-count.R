test_that("ac_count returns the number of matches per document", {
  ac <- ac_build(c("hello", "world"))
  doc <- c(a = "hello hello world", b = "nothing", c = NA_character_)

  expect_equal(
    ac_count(ac, doc),
    c(a = 3L, b = 0L, c = NA_integer_)
  )
})

test_that("ac_count can treat missing documents as zero matches", {
  ac <- ac_build("hello")

  expect_equal(
    ac_count(ac, c("hello", NA_character_, "world"), na = "zero"),
    c(1L, 0L, 0L)
  )
})

test_that("ac_count supports overlapping counts", {
  ac <- ac_build("aba")

  expect_equal(ac_count(ac, "ababa"), 1L)
  expect_equal(ac_count(ac, "ababa", overlapping = TRUE), 2L)
})

test_that("ac_count supports overlapping counts and UTF-8", {
  ac <- ac_build("把把")

  expect_equal(ac_count(ac, "一把把把把住了"), 2L)
  expect_equal(ac_count(ac, "一把把把把住了", overlapping = TRUE), 3L)
})

test_that("ac_count follows leftmost match semantics", {
  ac <- ac_build(
    c("disco", "disc", "discontent"),
    match_kind = "leftmost_longest"
  )

  expect_equal(ac_count(ac, "discontent"), 1L)
})

test_that("ac_count errors when overlapping search is incompatible with match_kind", {
  ac <- ac_build("hello", match_kind = "leftmost_first")

  expect_snapshot(
    error = TRUE,
    ac_count(ac, "hello", overlapping = TRUE)
  )
})

test_that("ac_count errors when missing documents are disallowed", {
  ac <- ac_build("hello")

  expect_snapshot(
    error = TRUE,
    ac_count(ac, c("hello", NA_character_), na = "error")
  )
})
