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

test_that("ac_build requires optional arguments to be named", {
  expect_snapshot(
    error = TRUE,
    ac_build(c("hello"), "leftmost_first")
  )
})

test_that("ac_build returns an automaton with all of its fields", {
  ac <- ac_build("hello", match_kind = "leftmost_first")

  expect_s3_class(ac, "ac_automaton")
  expect_named(ac, c("ptr", "patterns", "options", "info"))
  expect_type(ac$ptr, "externalptr")
  expect_equal(ac$options$match_kind, "leftmost_first")
  expect_true(has_ac_fields(ac))
  expect_identical(validate_ac_automaton(ac), ac)
})

test_that("ac_automaton validation catches invalid external pointers", {
  ac <- ac_build("hello")
  ac$ptr <- new("externalptr")

  expect_snapshot(ac_info(ac), error = TRUE)
})

test_that("ac_automaton validation catches malformed objects", {
  ac <- ac_build("hello")
  missing_field <- structure(
    unclass(ac)[c("ptr", "patterns")],
    class = "ac_automaton"
  )
  wrong_type <- ac
  wrong_type$patterns <- 1L

  expect_snapshot(error = TRUE, {
    ac_info("hello")
    ac_info(missing_field)
    ac_info(wrong_type)
  })
})

test_that("print shows a hint for automatons restored from disk", {
  ac <- ac_build(c("hello", "world"))
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(ac, path)
  restored <- readRDS(path)

  expect_snapshot({
    print(ac)
    print(restored)
    print(structure(list(ptr = new("externalptr")), class = "ac_automaton"))
    print(structure(list(), class = "ac_automaton"))
  })
  expect_identical(print(restored), restored)
})

test_that("length works for automatons restored from disk", {
  ac <- ac_build(c("hello", "world", "hello"), duplicate = "deduplicate")
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(ac, path)

  expect_identical(length(ac), 2L)
  expect_identical(length(readRDS(path)), 2L)
  expect_identical(length(structure(list(), class = "ac_automaton")), 0L)
})
