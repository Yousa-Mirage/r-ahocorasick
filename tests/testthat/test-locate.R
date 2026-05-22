test_that("ac_locate returns doc ids, pattern ids and character offsets", {
  ac <- ac_build(c("hello", "world"))
  doc <- c(doc1 = "hello world", doc2 = "world hello", doc3 = "nothing")

  hits <- ac_locate(ac, doc)

  expect_equal(
    hits,
    data.frame(
      doc_id = c(1L, 1L, 2L, 2L),
      pattern_id = c(1L, 2L, 2L, 1L),
      start = c(1L, 7L, 1L, 7L),
      end = c(5L, 11L, 5L, 11L)
    )
  )
})

test_that("ac_locate maps UTF-8 offsets and preserves original doc ids", {
  ac <- ac_build(c("hello", "world"))
  doc <- c("你好hello世界", NA_character_, "world")

  hits <- ac_locate(ac, doc)

  expect_equal(
    hits,
    data.frame(
      doc_id = c(1L, 3L),
      pattern_id = c(1L, 2L),
      start = c(3L, 1L),
      end = c(7L, 5L)
    )
  )
})

test_that("ac_locate respects overlap", {
  ac_standard <- ac_build(c("aba", "bab"))

  expect_equal(
    ac_locate(ac_standard, "ababa", overlapping = TRUE),
    data.frame(
      doc_id = c(1L, 1L, 1L),
      pattern_id = c(1L, 2L, 1L),
      start = c(1L, 2L, 3L),
      end = c(3L, 4L, 5L)
    )
  )
})

test_that("ac_locate handles mixed Chinese English and emoji patterns", {
  ac <- ac_build(c("你a😀", "a😀b", "前缀x", "后缀"))
  doc <- c("你a😀b文😀", "前缀x😀中后缀")

  hits <- ac_locate(ac, doc, overlapping = TRUE)

  expect_equal(
    hits,
    data.frame(
      doc_id = c(1L, 1L, 2L, 2L),
      pattern_id = c(1L, 2L, 3L, 4L),
      start = c(1L, 2L, 1L, 6L),
      end = c(3L, 4L, 3L, 7L)
    )
  )
})

test_that("ac_locate respects leftmost match semantics", {
  ac_leftmost <- ac_build(
    c("disco", "disc", "discontent"),
    match_kind = "leftmost_longest"
  )

  expect_equal(
    ac_locate(ac_leftmost, "discontent"),
    data.frame(
      doc_id = 1L,
      pattern_id = 3L,
      start = 1L,
      end = 10L
    )
  )
})

test_that("ac_locate honors case-insensitive matching", {
  ac_case <- ac_build("abc", ascii_case_insensitive = TRUE)

  expect_equal(
    ac_locate(ac_case, "ABC"),
    data.frame(
      doc_id = 1L,
      pattern_id = 1L,
      start = 1L,
      end = 3L
    )
  )
})

test_that("ac_locate matches pattern indexes correctly", {
  ac_dedup <- ac_build(c("hello", "hello", "world"), duplicate = "deduplicate")
  ac_keep <- ac_build(c("hello", "hello", "world"), duplicate = "keep")

  expect_equal(ac_locate(ac_dedup, "hello world")$pattern_id, c(1L, 2L))
  expect_equal(ac_locate(ac_keep, "hello world")$pattern_id, c(1L, 3L))
})

test_that("ac_locate errors when overlapping search is incompatible with match_kind", {
  ac <- ac_build("hello", match_kind = "leftmost_first")

  expect_snapshot(
    error = TRUE,
    ac_locate(ac, "hello", overlapping = TRUE)
  )
})

test_that("ac_locate errors when missing documents are disallowed", {
  ac <- ac_build("hello")

  expect_snapshot(
    error = TRUE,
    ac_locate(ac, c("hello", NA_character_), na = "error")
  )
})

test_that("ac_locate warns when no patterns match", {
  ac <- ac_build("hello")

  expect_snapshot(
    hits <- ac_locate(ac, "world")
  )

  expect_equal(
    hits,
    data.frame(
      doc_id = integer(),
      pattern_id = integer(),
      start = integer(),
      end = integer()
    )
  )
})
