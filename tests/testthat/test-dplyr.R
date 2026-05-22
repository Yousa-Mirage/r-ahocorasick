test_that("search functions work naturally in dplyr::mutate()", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tibble")
  skip_if_not_installed("tidyr")

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
    data.frame(
      pattern_id = c(1L, 2L),
      start = c(1L, 7L),
      end = c(5L, 11L)
    )
  )
  expect_equal(
    out$locations[[3]],
    data.frame(
      pattern_id = NA_integer_,
      start = NA_integer_,
      end = NA_integer_
    )
  )
  expect_equal(
    out$extracted[[4]],
    data.frame(
      matches = c("world", "hello"),
      patterns = c("world", "hello")
    )
  )

  unnested_locations <- tidyr::unnest(out, locations)
  expect_equal(unnested_locations$pattern_id, c(1L, 2L, NA_integer_, 2L, 1L))
  expect_equal(unnested_locations$start, c(1L, 7L, NA_integer_, 1L, 7L))
  expect_equal(unnested_locations$end, c(5L, 11L, NA_integer_, 5L, 11L))

  unnested_extract <- tidyr::unnest(out, extracted)
  expect_equal(
    unnested_extract$matches,
    c("hello", "world", NA_character_, "world", "hello")
  )
  expect_equal(
    unnested_extract$patterns,
    c("hello", "world", NA_character_, "world", "hello")
  )
})

test_that("missing documents can be made mutate-friendly defaults", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tibble")
  skip_if_not_installed("tidyr")

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
    data.frame(pattern_id = integer(), start = integer(), end = integer())
  )
  expect_equal(
    out$extracted[[2]],
    data.frame(matches = character(), patterns = character())
  )

  unnested_locations <- tidyr::unnest(out, locations)
  expect_equal(unnested_locations$doc, "hello")
  expect_equal(unnested_locations$pattern_id, 1L)

  unnested_extract <- tidyr::unnest(out, extracted)
  expect_equal(unnested_extract$doc, "hello")
  expect_equal(unnested_extract$matches, "hello")
})
