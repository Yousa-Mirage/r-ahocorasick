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

test_that("ac_replace_file writes default output paths", {
  ac <- ac_build(c("fox", "brown", "quick"))
  path <- tempfile(fileext = ".txt")
  writeLines("The quick brown fox.", path)

  output <- ac_replace_file(ac, path, c("sloth", "grey", "slow"))
  on.exit(unlink(c(path, output)), add = TRUE)

  expect_equal(fs::path_real(fs::path_dir(output)), fs::path_real(fs::path_dir(path)))
  expect_equal(
    fs::path_file(output),
    paste0(tools::file_path_sans_ext(fs::path_file(path)), "_replaced.txt")
  )
  expect_equal(readLines(output), "The slow grey sloth.")
})

test_that("ac_replace_file writes explicit output paths", {
  ac <- ac_build(c("hello", "world"))
  paths <- c(
    a = tempfile(),
    b = tempfile()
  )
  outputs <- c(
    a = tempfile(),
    b = tempfile()
  )
  on.exit(unlink(c(paths, outputs)), add = TRUE)
  writeLines("hello world", paths[[1]])
  writeLines("nothing", paths[[2]])

  result <- ac_replace_file(ac, paths, "x", output = outputs)

  expect_named(result, names(paths))
  expect_equal(fs::path_real(fs::path_dir(unname(result))), fs::path_real(fs::path_dir(outputs)))
  expect_equal(fs::path_file(unname(result)), fs::path_file(outputs))
  expect_equal(readLines(outputs[[1]]), "x x")
  expect_equal(readLines(outputs[[2]]), "nothing")
})

test_that("ac_replace_file supports UTF-8 matching", {
  ac <- ac_build(c("世界", "😀"))
  path <- tempfile(fileext = ".txt")
  output <- tempfile(fileext = ".txt")
  stream_output <- tempfile(fileext = ".txt")
  on.exit(unlink(c(path, output, stream_output)), add = TRUE)
  writeLines("你好世界😀", path, useBytes = TRUE)

  ac_replace_file(ac, path, c("world", "smile"), output = output)
  ac_replace_file(ac, path, c("world", "smile"), output = stream_output, stream = TRUE)

  expect_equal(readLines(output), "你好worldsmile")
  expect_equal(readLines(stream_output), "你好worldsmile")
})

test_that("ac_replace_file supports leftmost match kinds without streaming", {
  ac <- ac_build(
    c("append", "appendage", "app"),
    match_kind = "leftmost_longest"
  )
  path <- tempfile()
  output <- tempfile()
  on.exit(unlink(c(path, output)), add = TRUE)
  writeLines("append the app to the appendage", path)

  ac_replace_file(ac, path, c("x", "y", "z"), output = output)

  expect_equal(readLines(output), "x the z to the y")
})

test_that("ac_replace_file errors when output is the input file", {
  ac <- ac_build("hello")
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)
  writeLines("hello", path)

  expect_error(
    ac_replace_file(ac, path, "x", output = path),
    "`output` must not be the same file as `path`",
    fixed = TRUE
  )
})

test_that("ac_replace_file errors on invalid output paths", {
  ac <- ac_build("hello")
  paths <- c(tempfile(), tempfile())
  on.exit(unlink(paths), add = TRUE)
  writeLines("hello", paths[[1]])
  writeLines("world", paths[[2]])

  expect_snapshot(
    error = TRUE,
    ac_replace_file(ac, paths, "x", output = tempfile())
  )
})

test_that("ac_replace_file errors when stream search is incompatible with match_kind", {
  ac <- ac_build("hello", match_kind = "leftmost_longest")
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)
  writeLines("hello", path)

  expect_snapshot(
    error = TRUE,
    ac_replace_file(ac, path, "x", stream = TRUE)
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

test_that("ac_replace_file requires optional arguments to be named", {
  ac <- ac_build("hello")
  path <- tempfile()
  output <- tempfile()
  on.exit(unlink(c(path, output)), add = TRUE)
  writeLines("hello", path)

  expect_snapshot(
    error = TRUE,
    ac_replace_file(ac, path, "x", output)
  )
})
