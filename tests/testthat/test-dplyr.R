test_that("search functions work naturally in dplyr::mutate()", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tibble")

  ac <- ac_build(c("hello", "world"))
  docs <- tibble::tibble(
    doc = c("hello world", "nothing", NA_character_, "world hello")
  )

  out <- dplyr::mutate(
    docs,
    detected = ac_detect(ac, doc),
    count = ac_count(ac, doc),
    locations = ac_locate(ac, doc),
    extracted = ac_extract(ac, doc)
  )

  expect_equal(out$detected, c(TRUE, FALSE, NA, TRUE))
  expect_equal(out$count, c(2L, 0L, NA_integer_, 2L))
  expect_equal(
    out$locations[[1]],
    list(
      pattern_id = c(1L, 2L),
      start = c(1L, 7L),
      end = c(5L, 11L)
    )
  )
  expect_equal(
    out$locations[[3]],
    list(
      pattern_id = NA_integer_,
      start = NA_integer_,
      end = NA_integer_
    )
  )
  expect_equal(
    out$extracted[[4]],
    list(
      matches = c("world", "hello"),
      patterns = c("world", "hello")
    )
  )
})

test_that("missing documents can be made mutate-friendly defaults", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tibble")

  ac <- ac_build("hello")
  docs <- tibble::tibble(doc = c("hello", NA_character_, "world"))

  out <- dplyr::mutate(
    docs,
    detected = ac_detect(ac, doc, na = "false"),
    count = ac_count(ac, doc, na = "zero"),
    locations = ac_locate(ac, doc, na = "empty"),
    extracted = ac_extract(ac, doc, na = "empty")
  )

  expect_equal(out$detected, c(TRUE, FALSE, FALSE))
  expect_equal(out$count, c(1L, 0L, 0L))
  expect_equal(
    out$locations[[2]],
    list(pattern_id = integer(), start = integer(), end = integer())
  )
  expect_equal(
    out$extracted[[2]],
    list(matches = character(), patterns = character())
  )
})
