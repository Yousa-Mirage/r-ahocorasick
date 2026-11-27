# Tests on a large document: 鲁迅全集, about 600,000 characters.

luxun_path <- function() {
  test_path("fixtures", "luxun.txt")
}

skip_if_no_luxun <- function() {
  skip_if_not(
    file.exists(luxun_path()),
    "The Lu Xun fixture is only available in a source checkout."
  )
}

# The whole file as one string, line endings included.
read_luxun <- function() {
  path <- luxun_path()
  text <- rawToChar(readBin(path, "raw", file.size(path)))
  Encoding(text) <- "UTF-8"
  text
}

# Names, places and common words. "Q" overlaps "阿Q", and "中国", "中国人" and
# "国人" overlap each other, so overlapping search and match kinds matter.
luxun_patterns <- c(
  "阿Q",
  "Q",
  "孔乙己",
  "闰土",
  "祥林嫂",
  "狂人",
  "鲁镇",
  "未庄",
  "先生",
  "中国",
  "中国人",
  "国人",
  "革命",
  "呐喊",
  "彷徨",
  "吃人",
  "的",
  "，",
  "。"
)

# Sort located matches the same way.
sort_matches <- function(df) {
  df <- df[order(df$start, df$pattern_id), c("pattern_id", "start", "end")]
  row.names(df) <- NULL
  df
}

test_that("overlapping locate on a large document matches gregexpr()", {
  skip_if_no_luxun()

  text <- read_luxun()
  ac <- ac_build(luxun_patterns)

  expected <- do.call(
    rbind,
    lapply(seq_along(luxun_patterns), function(i) {
      starts <- gregexpr(luxun_patterns[[i]], text, fixed = TRUE)[[1]]
      starts <- as.integer(starts[starts > 0L])
      data.frame(
        pattern_id = rep(i, length(starts)),
        start = starts,
        end = starts + nchar(luxun_patterns[[i]]) - 1L
      )
    })
  )

  located <- ac_locate(ac, text, overlapping = TRUE)[[1]]

  expect_equal(
    sort_matches(located),
    sort_matches(expected)
  )
})

test_that("search functions agree with each other on a large document", {
  skip_if_no_luxun()

  text <- read_luxun()
  lines <- readLines(luxun_path(), encoding = "UTF-8")
  bytes <- readBin(luxun_path(), "raw", file.size(luxun_path()))

  # Byte offset before each character, and after the last one. `substring()`
  # would be much slower: it walks the text from the start for every match.
  char_bytes <- c(0, cumsum(nchar(strsplit(text, "")[[1]], type = "bytes")))

  for (match_kind in c("standard", "leftmost_first", "leftmost_longest")) {
    ac <- ac_build(luxun_patterns, match_kind = match_kind)
    overlapping <- if (match_kind == "standard") c(FALSE, TRUE) else FALSE

    for (ov in overlapping) {
      located <- ac_locate(ac, text, overlapping = ov)[[1]]
      located_bytes <- ac_locate_bytes(ac, text, overlapping = ov)
      extracted <- ac_extract(ac, text, overlapping = ov)[[1]]
      n <- ac_count(ac, text, overlapping = ov)

      expect_gt(n, 20000L)
      expect_equal(nrow(located), n)
      expect_equal(nrow(located_bytes), n)
      expect_equal(nrow(extracted), n)
      expect_true(ac_detect(ac, text))

      # Character positions, byte offsets and extracted text all describe the
      # same matches.
      expect_equal(located_bytes$pattern_id, located$pattern_id)
      expect_equal(extracted$patterns, luxun_patterns[located$pattern_id])
      expect_equal(extracted$matches, extracted$patterns)
      expect_equal(located_bytes$byte_start, char_bytes[located$start])
      expect_equal(located_bytes$byte_end, char_bytes[located$end + 1L])

      matched_bytes <- mapply(
        function(start, end) rawToChar(bytes[(start + 1):end]),
        located_bytes$byte_start,
        located_bytes$byte_end,
        USE.NAMES = FALSE
      )
      Encoding(matched_bytes) <- "UTF-8"
      expect_equal(matched_bytes, extracted$matches)

      # No pattern contains a line break, so searching line by line finds the
      # same matches as searching the whole text.
      line_counts <- ac_count(ac, lines, overlapping = ov)

      expect_equal(
        sum(line_counts),
        n
      )
      expect_equal(
        ac_detect(ac, lines),
        line_counts > 0L
      )
      expect_equal(
        lengths(lapply(ac_locate(ac, lines, overlapping = ov), `[[`, "start")),
        line_counts
      )
      expect_equal(
        ac_extract_df(ac, lines, overlapping = ov)$matches,
        extracted$matches
      )
    }
  }
})

test_that("file functions agree with document functions on a large document", {
  skip_if_no_luxun()

  text <- read_luxun()
  path <- luxun_path()
  ac <- ac_build(luxun_patterns)

  for (ov in c(FALSE, TRUE)) {
    expect_equal(
      ac_count_file(ac, path, overlapping = ov),
      ac_count(ac, text, overlapping = ov)
    )
    expect_equal(
      ac_locate_file(ac, path, overlapping = ov),
      ac_locate(ac, text, overlapping = ov)
    )
    expect_equal(
      ac_extract_file(ac, path, overlapping = ov),
      ac_extract(ac, text, overlapping = ov)
    )
  }

  expect_equal(
    ac_count_file(ac, path, stream = TRUE),
    ac_count(ac, text)
  )
  expect_equal(
    ac_extract_file(ac, path, stream = TRUE),
    ac_extract(ac, text)
  )
})

test_that("replacing on a large document round-trips", {
  skip_if_no_luxun()

  text <- read_luxun()
  ac <- ac_build(luxun_patterns)
  tokens <- sprintf("<%02d>", seq_along(luxun_patterns))
  ac_tokens <- ac_build(tokens)

  expect_false(ac_detect(ac_tokens, text))

  replaced <- ac_replace(ac, text, tokens)

  expect_equal(
    ac_count(ac_tokens, replaced),
    ac_count(ac, text)
  )
  expect_equal(
    ac_extract(ac_tokens, replaced)[[1]]$matches,
    tokens[ac_locate(ac, text)[[1]]$pattern_id]
  )
  expect_identical(
    ac_replace(ac_tokens, replaced, luxun_patterns),
    text
  )

  # File replacement, with and without streaming, writes the same text.
  read_text <- function(path) {
    out <- rawToChar(readBin(path, "raw", file.size(path)))
    Encoding(out) <- "UTF-8"
    out
  }
  outputs <- c(tempfile(fileext = ".txt"), tempfile(fileext = ".txt"))
  on.exit(unlink(outputs), add = TRUE)

  ac_replace_file(ac, luxun_path(), tokens, output = outputs[[1]])
  ac_replace_file(
    ac,
    luxun_path(),
    tokens,
    output = outputs[[2]],
    stream = TRUE
  )

  expect_identical(read_text(outputs[[1]]), replaced)
  expect_identical(read_text(outputs[[2]]), replaced)
})

test_that("results on a large document are stable", {
  skip_if_no_luxun()

  text <- read_luxun()
  ac <- ac_build(luxun_patterns, match_kind = "leftmost_longest")
  located <- ac_locate_df(ac, text)

  expect_snapshot({
    nchar(text)
    ac_count(ac, text)
    data.frame(
      pattern = luxun_patterns,
      count = tabulate(located$pattern_id, length(luxun_patterns))
    )
    head(located[located$pattern_id <= 8L, ], 10)
    ac_extract_df(ac, text)[c(1, 1000, 10000, 20000), ]
  })
})
