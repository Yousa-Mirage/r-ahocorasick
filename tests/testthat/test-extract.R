test_that("ac_extract returns matches and patterns per document", {
  ac <- ac_build(c(greeting = "hello", object = "world"))
  doc <- c(a = "hello world hello", b = "nothing", c = NA_character_)

  hits <- ac_extract(ac, doc)

  expect_named(hits, names(doc))
  expect_equal(
    hits[[1]],
    data.frame(
      matches = c("hello", "world", "hello"),
      patterns = c("hello", "world", "hello")
    )
  )
  expect_equal(hits[[2]], data.frame(matches = character(), patterns = character()))
  expect_equal(hits[[3]], data.frame(matches = NA_character_, patterns = NA_character_))
})

test_that("ac_extract_df returns one row per match", {
  ac <- ac_build(c("hello", "world"))
  doc <- c("hello world", NA_character_, "nothing", "world")

  hits <- ac_extract_df(ac, doc)

  expect_equal(
    hits,
    data.frame(
      doc_id = c(1L, 1L, 4L),
      matches = c("hello", "world", "world"),
      patterns = c("hello", "world", "world")
    )
  )
})

test_that("ac_extract_df keeps actual matches and pattern values", {
  ac <- ac_build(c("abc", "你a😀"), ascii_case_insensitive = TRUE)
  doc <- c("ABC 你a😀", NA_character_)

  hits <- ac_extract_df(ac, doc, na = "keep")

  expect_equal(
    hits,
    data.frame(
      doc_id = c(1L, 1L, 2L),
      matches = c("ABC", "你a😀", NA_character_),
      patterns = c("abc", "你a😀", NA_character_)
    )
  )
})

test_that("ac_extract_df returns an empty data frame when no patterns match", {
  ac <- ac_build("hello")

  hits <- ac_extract_df(ac, c("world", NA_character_))

  expect_equal(
    hits,
    data.frame(
      doc_id = integer(),
      matches = character(),
      patterns = character()
    )
  )
})

test_that("ac_extract distinguishes matched text from pattern values", {
  ac <- ac_build("abc", ascii_case_insensitive = TRUE)

  hits <- ac_extract(ac, "ABC")

  expect_equal(hits[[1]], data.frame(matches = "ABC", patterns = "abc"))
})

test_that("ac_extract supports overlapping and UTF-8 matches", {
  ac <- ac_build(c("aba", "bab", "你a😀"))
  doc <- c("ababa", "xx你a😀")

  hits <- ac_extract(ac, doc, overlapping = TRUE)

  expect_equal(
    hits[[1]],
    data.frame(
      matches = c("aba", "bab", "aba"),
      patterns = c("aba", "bab", "aba")
    )
  )
  expect_equal(hits[[2]], data.frame(matches = "你a😀", patterns = "你a😀"))
})

test_that("ac_extract can treat missing documents as empty matches", {
  ac <- ac_build("hello")

  hits <- ac_extract(ac, c("hello", NA_character_, "world"), na = "empty")

  expect_equal(hits[[1]], data.frame(matches = "hello", patterns = "hello"))
  expect_equal(hits[[2]], data.frame(matches = character(), patterns = character()))
  expect_equal(hits[[3]], data.frame(matches = character(), patterns = character()))
})

test_that("ac_extract_file returns matches and patterns per file", {
  ac <- ac_build(c(greeting = "hello", object = "world"))
  paths <- c(
    a = tempfile(),
    b = tempfile()
  )
  on.exit(unlink(paths), add = TRUE)
  writeLines("hello world hello", paths[[1]])
  writeLines("nothing", paths[[2]])

  hits <- ac_extract_file(ac, paths)

  expect_named(hits, names(paths))
  expect_equal(
    hits[[1]],
    data.frame(
      matches = c("hello", "world", "hello"),
      patterns = c("hello", "world", "hello")
    )
  )
  expect_equal(hits[[2]], data.frame(matches = character(), patterns = character()))
})

test_that("ac_extract_file keeps actual matched text", {
  ac <- ac_build(c("abc", "你a😀"), ascii_case_insensitive = TRUE)
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)
  writeLines("ABC 你a😀", path, useBytes = TRUE)

  hits <- ac_extract_file(ac, path)
  stream_hits <- ac_extract_file(ac, path, stream = TRUE)

  expect_equal(
    hits[[1]],
    data.frame(
      matches = c("ABC", "你a😀"),
      patterns = c("abc", "你a😀")
    )
  )
  expect_equal(stream_hits, hits)
})

test_that("ac_extract_file supports overlapping matches without streaming", {
  ac <- ac_build("aba")
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)
  writeLines("ababa", path)

  expect_equal(
    ac_extract_file(ac, path)[[1]],
    data.frame(matches = "aba", patterns = "aba")
  )
  expect_equal(
    ac_extract_file(ac, path, overlapping = TRUE)[[1]],
    data.frame(matches = c("aba", "aba"), patterns = c("aba", "aba"))
  )

  expect_snapshot(
    error = TRUE,
    ac_extract_file(ac, path, stream = TRUE, overlapping = TRUE)
  )
})

test_that("ac_extract_file supports leftmost match kinds without streaming", {
  ac <- ac_build(
    c("append", "appendage", "app"),
    match_kind = "leftmost_longest"
  )
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)
  writeLines("appendage app", path)

  hits <- ac_extract_file(ac, path)

  expect_equal(
    hits[[1]],
    data.frame(
      matches = c("appendage", "app"),
      patterns = c("appendage", "app")
    )
  )
})

test_that("ac_extract_file errors when stream search is incompatible with match_kind", {
  ac <- ac_build("hello", match_kind = "leftmost_first")
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)
  writeLines("hello", path)

  expect_snapshot(
    error = TRUE,
    ac_extract_file(ac, path, stream = TRUE)
  )

  expect_snapshot(
    error = TRUE,
    ac_extract_file(ac, path, overlapping = TRUE)
  )
})

test_that("ac_extract errors when overlapping search is incompatible with match_kind", {
  ac <- ac_build("hello", match_kind = "leftmost_first")

  expect_snapshot(
    error = TRUE,
    ac_extract(ac, "hello", overlapping = TRUE)
  )

  expect_snapshot(
    error = TRUE,
    ac_extract_df(ac, "hello", overlapping = TRUE)
  )
})

test_that("ac_extract errors when missing documents are disallowed", {
  ac <- ac_build("hello")

  expect_snapshot(
    error = TRUE,
    ac_extract(ac, c("hello", NA_character_), na = "error")
  )

  expect_snapshot(
    error = TRUE,
    ac_extract_df(ac, c("hello", NA_character_), na = "error")
  )
})
