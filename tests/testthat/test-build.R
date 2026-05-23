test_that("ac_build returns an automaton with stored patterns and metadata", {
  ac <- ac_build(
    c(greeting = "hello", object = "world!"),
    ascii_case_insensitive = TRUE
  )
  info <- ac_info(ac)

  expect_s3_class(ac, "ac_automaton")
  expect_equal(length(ac), 2L)
  expect_equal(ac_patterns(ac), c(greeting = "hello", object = "world!"))
  expect_equal(info$patterns_len, 2L)
  expect_equal(info$min_pattern_len, 5L)
  expect_equal(info$max_pattern_len, 6L)
  expect_equal(info$match_kind, "standard")
  expect_type(info$implementation, "character")
  expect_true(info$ascii_case_insensitive)
  expect_true(info$memory_usage > 0)
})

test_that("ac_build applies duplicate handling before compilation", {
  ac_keep <- ac_build(c("hello", "hello", "world"), duplicate = "keep")
  ac_dedup <- ac_build(c("hello", "hello", "world"), duplicate = "deduplicate")

  expect_equal(length(ac_keep), 3L)
  expect_equal(length(ac_dedup), 2L)
})

test_that("ac_build errors on duplicate patterns when requested", {
  expect_snapshot(
    error = TRUE,
    ac_build(c("hello", "hello"), duplicate = "error")
  )
})

test_that("ac_build errors on invalid pattern vectors", {
  expect_snapshot(
    error = TRUE,
    ac_build(c("hello", ""))
  )
})

test_that("ac_automaton validation catches invalid external pointers", {
  ac <- structure(
    list(ptr = new("externalptr")),
    class = "ac_automaton"
  )

  expect_snapshot(
    error = TRUE,
    ac_info(ac)
  )
})
