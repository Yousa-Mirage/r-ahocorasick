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

test_that("ac_detect_file reports whether each file has any match", {
  ac <- ac_build(c("hello", "world"))
  paths <- c(
    a = tempfile(),
    b = tempfile(),
    c = tempfile()
  )
  on.exit(unlink(paths), add = TRUE)
  writeLines("hello there", paths[[1]])
  writeLines("nothing", paths[[2]])
  writeLines("world", paths[[3]])

  expect_equal(
    ac_detect_file(ac, paths),
    c(a = TRUE, b = FALSE, c = TRUE)
  )
})

test_that("ac_detect_file supports UTF-8 matching", {
  ac <- ac_build(c("你a😀", "后缀"))
  paths <- c(tempfile(), tempfile())
  on.exit(unlink(paths), add = TRUE)
  writeLines("xx你a😀", paths[[1]], useBytes = TRUE)
  writeLines("前缀x😀中后缀", paths[[2]], useBytes = TRUE)

  expect_equal(ac_detect_file(ac, paths), c(TRUE, TRUE))
  expect_equal(ac_detect_file(ac, paths, stream = TRUE), c(TRUE, TRUE))
})

test_that("ac_detect_file supports here paths", {
  testthat::skip_if_not_installed("here")
  ac <- ac_build("Package")

  expect_true(ac_detect_file(ac, here::here("DESCRIPTION")))
})

test_that("ac_detect_file supports leftmost match kinds without streaming", {
  ac <- ac_build("hello", match_kind = "leftmost_first")
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)
  writeLines("hello", path)

  expect_true(ac_detect_file(ac, path))
})

test_that("ac_detect_file errors when stream search is incompatible with match_kind", {
  ac <- ac_build("hello", match_kind = "leftmost_first")
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)
  writeLines("hello", path)

  expect_snapshot(
    error = TRUE,
    ac_detect_file(ac, path, stream = TRUE)
  )
})

test_that("ac_detect_file errors on missing paths", {
  ac <- ac_build("hello")

  expect_snapshot(
    error = TRUE,
    ac_detect_file(ac, c("file.txt", NA_character_))
  )
})

test_that("ac_detect_file errors when a file does not exist", {
  ac <- ac_build("hello")

  expect_snapshot(
    error = TRUE,
    ac_detect_file(ac, "definitely-missing-ahocorasick-file.txt")
  )
})

test_that("ac_detect_file errors when path is not a file", {
  ac <- ac_build("hello")

  expect_snapshot(
    error = TRUE,
    transform = function(x) {
      gsub(normalizePath(".", mustWork = FALSE), "<testthat-dir>", x, fixed = TRUE)
    },
    ac_detect_file(ac, ".")
  )
})

test_that("ac_detect errors when missing documents are disallowed", {
  ac <- ac_build("hello")

  expect_snapshot(
    error = TRUE,
    ac_detect(ac, c("hello", NA_character_), na = "error")
  )
})
