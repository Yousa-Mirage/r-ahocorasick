# Generate the benchmark inputs from the novels in `bench/data/`.
#
# Run from the package root:
#
#   Rscript bench/01-generate-benchmark-data.R
#
# This writes `bench/output/benchmark-inputs.rds`, which
# `bench/02-run-benchmarks.R` reads. The inputs are deterministic: the same
# novels always give the same documents and patterns.

if (!file.exists("DESCRIPTION") || !dir.exists("bench/data")) {
  stop("Run this script from the package root.", call. = FALSE)
}

data_dir <- "bench/data"
output_dir <- "bench/output"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Document shapes, in characters per document.
medium_docs_n <- 500L
medium_chars <- 5000L
short_docs_n <- 20000L
short_chars <- c(en = 200L, zh = 100L)

english_books <- c(
  "Crime and Punishment.txt",
  "Pride and Prejudice.txt",
  "The Count of Monte Cristo.txt"
)
chinese_books <- c(
  "三体1-3.txt",
  "射雕英雄传.txt",
  "红楼梦.txt",
  "鲁迅全集.txt"
)

# Text -------------------------------------------------------------------------

read_book <- function(file) {
  path <- file.path(data_dir, file)
  text <- rawToChar(readBin(path, "raw", file.size(path)))
  Encoding(text) <- "UTF-8"
  if (!validUTF8(text)) {
    stop("`", file, "` is not valid UTF-8.", call. = FALSE)
  }
  # Collapse line breaks and runs of white space into single spaces.
  gsub("[[:space:]　]+", " ", text, perl = TRUE)
}

# English text is lowercased and transliterated to ASCII, so that ASCII-only
# methods can search it.
normalize_english <- function(text) {
  text <- iconv(
    tolower(text),
    from = "UTF-8",
    to = "ASCII//TRANSLIT",
    sub = " "
  )
  text <- gsub("[^ -~]+", " ", text, perl = TRUE)
  gsub(" +", " ", text, perl = TRUE)
}

# Split `text` into consecutive chunks of `size` characters, dropping a short
# final chunk. Working on single characters avoids `substring()`, which is
# slow on long UTF-8 strings.
chunk_text <- function(text, size) {
  chars <- strsplit(text, "", fixed = TRUE)[[1]]
  n_chunks <- length(chars) %/% size
  chars <- chars[seq_len(n_chunks * size)]
  groups <- rep(seq_len(n_chunks), each = size)
  vapply(
    split(chars, groups),
    paste,
    character(1),
    collapse = "",
    USE.NAMES = FALSE
  )
}

# Take `n` chunks spread evenly over every book, so that each corpus covers all
# of its books.
sample_chunks <- function(books, size, n) {
  chunks <- unlist(lapply(books, chunk_text, size = size), use.names = FALSE)
  if (length(chunks) < n) {
    stop(
      "Not enough text for ",
      n,
      " chunks of ",
      size,
      " characters.",
      call. = FALSE
    )
  }
  chunks[unique(round(seq.int(1L, length(chunks), length.out = n)))]
}

en_books <- vapply(
  english_books,
  function(f) normalize_english(read_book(f)),
  ""
)
zh_books <- vapply(chinese_books, read_book, "")

en_medium <- sample_chunks(en_books, medium_chars, medium_docs_n)
zh_medium <- sample_chunks(zh_books, medium_chars, medium_docs_n)
half <- medium_chars %/% 2L
mix_medium <- paste(substr(en_medium, 1L, half), substr(zh_medium, 1L, half))

corpora <- list(
  en_medium = en_medium,
  zh_medium = zh_medium,
  mix_medium = mix_medium,
  en_short = sample_chunks(en_books, short_chars[["en"]], short_docs_n),
  zh_short = sample_chunks(zh_books, short_chars[["zh"]], short_docs_n),
  # One document per book.
  en_long = unname(en_books),
  zh_long = unname(zh_books)
)

# Patterns ---------------------------------------------------------------------

# Patterns whose prefix equals their suffix ("that", "哈哈") can overlap
# themselves. The base R and stringr baselines search one pattern at a time and
# never report such overlaps, so their counts would differ from overlapping
# Aho-Corasick counts. These patterns are left out of every pattern set.
has_self_overlap <- function(pattern) {
  chars <- strsplit(pattern, "", fixed = TRUE)[[1]]
  n <- length(chars)
  n > 1L &&
    any(vapply(
      seq_len(n - 1L),
      function(k) identical(chars[seq_len(k)], chars[(n - k + 1L):n]),
      logical(1)
    ))
}

# Return the first `n` candidates that do not overlap themselves and are not in
# `exclude`.
take_patterns <- function(candidates, n, exclude = character()) {
  candidates <- setdiff(candidates, exclude)
  keep <- !vapply(candidates, has_self_overlap, logical(1))
  selected <- candidates[keep][seq_len(n)]
  if (anyNA(selected)) {
    stop("Not enough pattern candidates.", call. = FALSE)
  }
  selected
}

# Sort the names of a frequency table by decreasing frequency, breaking ties by
# name so the order is deterministic.
by_frequency <- function(freq, decreasing = TRUE) {
  ord <- order(if (decreasing) -freq else freq, names(freq))
  freq[ord]
}

en_words <- regmatches(en_medium, gregexpr("\\b[a-z]{3,14}\\b", en_medium))
en_freq <- by_frequency(table(unlist(en_words)))

en_common <- take_patterns(names(en_freq), 32L)
en_rare_pool <- names(by_frequency(
  en_freq[nchar(names(en_freq)) >= 7L & en_freq >= 2L],
  decreasing = FALSE
))
en_rare <- take_patterns(en_rare_pool, 256L, exclude = en_common)
en_dict <- take_patterns(names(en_freq), 2000L)

# Chinese words are not separated by spaces, so the candidates are the most
# frequent runs of 2 to 4 Han characters.
han_ngrams <- function(doc, sizes = 2:4) {
  chars <- strsplit(doc, "", fixed = TRUE)[[1]]
  unlist(lapply(sizes, function(size) {
    n <- length(chars) - size + 1L
    if (n < 1L) {
      return(character())
    }
    do.call(
      paste0,
      lapply(seq_len(size) - 1L, function(k) chars[k + seq_len(n)])
    )
  }))
}
zh_freq <- table(unlist(lapply(
  zh_medium[seq(1L, length(zh_medium), by = 3L)],
  han_ngrams
)))
# `\\p{Han}` would also match punctuation such as "。", which Unicode lists as
# used with Han, so test the main CJK block directly.
is_han <- grepl("^[\\x{4E00}-\\x{9FFF}]+$", names(zh_freq), perl = TRUE)
zh_freq <- by_frequency(zh_freq[is_han])

zh_common <- take_patterns(names(zh_freq), 32L)
zh_dict <- take_patterns(names(zh_freq), 1000L)

pattern_sets <- list(
  en_common_32 = en_common,
  en_rare_256 = en_rare,
  en_dict_2000 = en_dict,
  zh_common_32 = zh_common,
  zh_dict_1000 = zh_dict,
  mix_common_64 = c(en_common, zh_common)
)

# Cases ------------------------------------------------------------------------

cases <- data.frame(
  case_id = c(
    "en_medium_common",
    "en_medium_rare",
    "en_medium_dict",
    "zh_medium_common",
    "zh_medium_dict",
    "mix_medium_common",
    "en_short_common",
    "zh_short_common",
    "en_long_common",
    "zh_long_common"
  ),
  corpus_id = c(
    "en_medium",
    "en_medium",
    "en_medium",
    "zh_medium",
    "zh_medium",
    "mix_medium",
    "en_short",
    "zh_short",
    "en_long",
    "zh_long"
  ),
  pattern_set_id = c(
    "en_common_32",
    "en_rare_256",
    "en_dict_2000",
    "zh_common_32",
    "zh_dict_1000",
    "mix_common_64",
    "en_common_32",
    "zh_common_32",
    "en_common_32",
    "zh_common_32"
  )
)
cases$docs_n <- lengths(corpora[cases$corpus_id], use.names = FALSE)
cases$mean_doc_chars <- vapply(
  corpora[cases$corpus_id],
  function(docs) mean(nchar(docs)),
  numeric(1),
  USE.NAMES = FALSE
)
cases$input_mib <- vapply(
  corpora[cases$corpus_id],
  function(docs) sum(nchar(docs, type = "bytes")) / 2^20,
  numeric(1),
  USE.NAMES = FALSE
)
cases$patterns_n <- lengths(
  pattern_sets[cases$pattern_set_id],
  use.names = FALSE
)

saveRDS(
  list(corpora = corpora, pattern_sets = pattern_sets, cases = cases),
  file.path(output_dir, "benchmark-inputs.rds")
)

print(cases, digits = 3)
message("Wrote ", file.path(output_dir, "benchmark-inputs.rds"))
