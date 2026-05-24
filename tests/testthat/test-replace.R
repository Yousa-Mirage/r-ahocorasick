test_that("ac_replace replaces matches with per-pattern replacements", {
  ac <- ac_build(c("fox", "brown", "quick"))
  doc <- c(a = "The quick brown fox.", b = "nothing")

  expect_equal(
    ac_replace(ac, doc, c("sloth", "grey", "slow")),
    c(a = "The slow grey sloth.", b = "nothing")
  )
})

test_that("ac_replace can use one replacement for all patterns", {
  ac <- ac_build(c("hello", "world"))

  expect_equal(
    ac_replace(ac, "hello world", ""),
    " "
  )
})

test_that("ac_replace supports UTF-8 matching", {
  ac_utf8 <- ac_build(c("世界", "😀"))

  expect_equal(ac_replace(ac_utf8, "你好世界😀", c("world", "smile")), "你好worldsmile")
})

test_that("ac_replace supports ASCII case-insensitive matching", {
  ac_case <- ac_build("abc", ascii_case_insensitive = TRUE)

  expect_equal(ac_replace(ac_case, "ABC abc", "x"), "x x")
})

test_that("ac_replace follows automaton match semantics", {
  doc <- "append the app to the appendage"
  replace_with <- c("x", "y", "z")

  ac_standard <- ac_build(c("append", "appendage", "app"))
  ac_leftmost_first <- ac_build(
    c("append", "appendage", "app"),
    match_kind = "leftmost_first"
  )
  ac_leftmost_longest <- ac_build(
    c("append", "appendage", "app"),
    match_kind = "leftmost_longest"
  )

  expect_equal(
    ac_replace(ac_standard, doc, replace_with),
    "zend the z to the zendage"
  )
  expect_equal(
    ac_replace(ac_leftmost_first, doc, replace_with),
    "x the z to the xage"
  )
  expect_equal(
    ac_replace(ac_leftmost_longest, doc, replace_with),
    "x the z to the y"
  )
})

test_that("ac_replace handles missing documents", {
  ac <- ac_build("hello")
  doc <- c(a = "hello", b = NA_character_, c = "world")

  expect_equal(
    ac_replace(ac, doc, "x"),
    c(a = "x", b = NA, c = "world")
  )
  expect_equal(
    ac_replace(ac, doc, "x", na = "empty"),
    c(a = "x", b = "", c = "world")
  )
})

test_that("ac_replace matches duplicate pattern indexes", {
  ac <- ac_build(c("hello", "hello", "world"), duplicate = "keep")

  expect_equal(
    ac_replace(ac, "hello world", c("x", "y", "z")),
    "x z"
  )
})

test_that("ac_replace errors on invalid replacements", {
  ac <- ac_build(c("hello", "world"))

  expect_snapshot(
    error = TRUE,
    ac_replace(ac, "hello", c("x", "y", "z"))
  )
  expect_snapshot(
    error = TRUE,
    ac_replace(ac, "hello", c("x", NA_character_))
  )
})

test_that("ac_replace errors when missing documents are disallowed", {
  ac <- ac_build("hello")

  expect_snapshot(
    error = TRUE,
    ac_replace(ac, c("hello", NA_character_), "x", na = "error")
  )
})
