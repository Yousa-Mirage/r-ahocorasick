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

test_that("ac_count_file returns the number of matches per file", {
  ac <- ac_build(c("hello", "world"))
  paths <- c(
    a = tempfile(),
    b = tempfile(),
    c = tempfile()
  )
  on.exit(unlink(paths), add = TRUE)
  writeLines("hello hello world", paths[[1]])
  writeLines("nothing", paths[[2]])
  writeLines("world", paths[[3]])

  expect_equal(
    ac_count_file(ac, paths),
    c(a = 3L, b = 0L, c = 1L)
  )
})

test_that("ac_count_file supports UTF-8 matching", {
  ac <- ac_build("把把")
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)
  writeLines("一把把把把住了", path, useBytes = TRUE)

  expect_equal(ac_count_file(ac, path), 2L)
  expect_equal(ac_count_file(ac, path, stream = TRUE), 2L)
})

test_that("ac_count_file supports here paths", {
  testthat::skip_if_not_installed("here")
  ac <- ac_build("Package")

  expect_equal(ac_count_file(ac, here::here("DESCRIPTION")), 1L)
})

test_that("ac_count_file supports leftmost match kinds without streaming", {
  ac <- ac_build(
    c("append", "appendage", "app"),
    match_kind = "leftmost_longest"
  )
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)
  writeLines("appendage app", path)

  expect_equal(ac_count_file(ac, path), 2L)
})

test_that("ac_count_file errors when stream search is incompatible with match_kind", {
  ac <- ac_build("hello", match_kind = "leftmost_longest")
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)
  writeLines("hello", path)

  expect_snapshot(
    error = TRUE,
    ac_count_file(ac, path, stream = TRUE)
  )
})

test_that("ac_count_file errors on missing paths", {
  ac <- ac_build("hello")

  expect_snapshot(
    error = TRUE,
    ac_count_file(ac, c("file.txt", NA_character_))
  )
})

test_that("ac_count_file errors when a file does not exist", {
  ac <- ac_build("hello")

  expect_snapshot(
    error = TRUE,
    ac_count_file(ac, "definitely-missing-ahocorasick-file.txt")
  )
})

test_that("ac_count_file errors when path is not a file", {
  ac <- ac_build("hello")

  expect_snapshot(
    ac_count_file(ac, "."),
    error = TRUE,
    transform = function(x) {
      gsub(normalizePath(".", mustWork = FALSE), "<testthat-dir>", x, fixed = TRUE)
    }
  )
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
