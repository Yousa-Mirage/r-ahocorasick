# "he" followed by a byte that can never appear in UTF-8.
invalid_utf8 <- function() {
  rawToChar(as.raw(c(0x68, 0x65, 0xff)))
}

test_that("document functions reject invalid UTF-8", {
  ac <- ac_build(c("he", "llo"))
  doc <- c("hello", invalid_utf8(), NA_character_)

  expect_error(ac_count(ac, doc), class = "rlang_error")
  expect_error(ac_detect(ac, doc), class = "rlang_error")
  expect_error(ac_extract(ac, doc), class = "rlang_error")
  expect_error(ac_extract_df(ac, doc), class = "rlang_error")
  expect_error(ac_locate(ac, doc), class = "rlang_error")
  expect_error(ac_locate_bytes(ac, doc), class = "rlang_error")
  expect_error(ac_locate_df(ac, doc), class = "rlang_error")
  expect_error(ac_replace(ac, doc, "X"), class = "rlang_error")
})

test_that("invalid UTF-8 errors name the argument and positions", {
  ac <- ac_build(c("he", "llo"))

  expect_snapshot(error = TRUE, {
    ac_locate(ac, c("hello", invalid_utf8(), NA_character_))
    ac_count(ac, rep(invalid_utf8(), 7))
    ac_build(c("ok", invalid_utf8()))
    ac_replace(ac, "hello", c("X", invalid_utf8()))
  })
})

test_that("file functions reject paths with invalid UTF-8", {
  ac <- ac_build("hello")
  path <- tempfile()
  writeLines("hello", path)
  on.exit(unlink(path), add = TRUE)

  expect_snapshot(error = TRUE, {
    ac_count_file(ac, invalid_utf8())
  })
  expect_error(
    ac_replace_file(ac, path, "X", output = invalid_utf8()),
    class = "rlang_error"
  )
})

test_that("latin1 strings are converted to UTF-8 before matching", {
  cafe <- rawToChar(as.raw(c(0x63, 0x61, 0x66, 0xe9)))
  Encoding(cafe) <- "latin1"

  ac <- ac_build(cafe)
  doc <- c(paste("un", cafe), NA_character_)

  expect_equal(ac_patterns(ac), "café")
  expect_equal(ac_detect(ac, doc), c(TRUE, NA))
  expect_equal(ac_locate(ac, doc)[[1]]$start, 4L)
  expect_equal(ac_locate(ac, doc)[[1]]$end, 7L)
  expect_equal(ac_replace(ac, doc, "tea"), c("un tea", NA_character_))
})
