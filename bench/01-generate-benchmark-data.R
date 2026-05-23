args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) > 0L) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath("bench/01-generate-benchmark-data.R", mustWork = FALSE)
}

bench_dir <- dirname(script_path)
root_dir <- normalizePath(file.path(bench_dir, ".."), mustWork = TRUE)
data_dir <- file.path(bench_dir, "data")
output_dir <- file.path(bench_dir, "output")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

docs_n <- as.integer(Sys.getenv("BENCH_DOCS_N", "200"))
chunk_chars <- as.integer(Sys.getenv("BENCH_CHUNK_CHARS", "5000"))
pattern_docs_n <- as.integer(Sys.getenv("BENCH_PATTERN_DOCS_N", "80"))
seed <- as.integer(Sys.getenv("BENCH_SEED", "20260524"))

set.seed(seed)

read_text <- function(path) {
  raw <- readBin(path, "raw", n = file.info(path)$size)
  text <- rawToChar(raw)
  for (encoding in c("UTF-8", "GB18030", "GBK")) {
    converted <- iconv(text, from = encoding, to = "UTF-8", sub = NA)
    if (!is.na(converted)) {
      return(converted)
    }
  }
  stop("Could not decode text file: ", basename(path), call. = FALSE)
}

normalize_space <- function(x) {
  x <- gsub("\r\n|\r", "\n", x, perl = TRUE)
  gsub("[[:space:]]+", " ", x, perl = TRUE)
}

normalize_english <- function(x) {
  x <- normalize_space(tolower(x))
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = " ")
  x <- gsub("[^ -~]+", " ", x, perl = TRUE)
  gsub("[[:space:]]+", " ", x, perl = TRUE)
}

normalize_utf8 <- function(x) {
  normalize_space(enc2utf8(x))
}

chunk_text <- function(text, size) {
  n <- nchar(text, type = "chars", allowNA = FALSE, keepNA = FALSE)
  starts <- seq.int(1L, n, by = size)
  chunks <- substring(text, starts, pmin(starts + size - 1L, n))
  chunks[nchar(chunks, type = "chars") > size * 0.8]
}

select_evenly <- function(x, n) {
  if (length(x) <= n) {
    return(x)
  }
  x[unique(round(seq.int(1L, length(x), length.out = n)))]
}

make_corpus <- function(files, corpus_id, transform, chunk_size, n) {
  rows <- lapply(files, function(path) {
    text <- transform(read_text(path))
    chunks <- chunk_text(text, chunk_size)
    data.frame(
      corpus_id = corpus_id,
      source = basename(path),
      chunk_id = seq_along(chunks),
      doc = chunks,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  out <- out[seq_len(nrow(out)) %in% select_evenly(seq_len(nrow(out)), n), , drop = FALSE]
  row.names(out) <- NULL
  out$doc_id <- sprintf("%s_%04d", corpus_id, seq_len(nrow(out)))
  out[, c("corpus_id", "doc_id", "source", "chunk_id", "doc")]
}

extract_regex <- function(text, pattern) {
  matches <- gregexpr(pattern, text, perl = TRUE)
  unlist(regmatches(text, matches), use.names = FALSE)
}

has_self_overlap <- function(pattern) {
  chars <- strsplit(pattern, "", useBytes = FALSE)[[1]]
  n <- length(chars)
  if (n < 2L) {
    return(FALSE)
  }
  any(vapply(
    seq_len(n - 1L),
    function(k) {
      identical(chars[seq_len(k)], tail(chars, k))
    },
    logical(1)
  ))
}

is_han_string <- function(pattern) {
  codepoints <- utf8ToInt(pattern)
  all(codepoints >= 0x4E00 & codepoints <= 0x9FFF)
}

select_patterns <- function(candidates, n, exclude = character()) {
  selected <- character()
  for (candidate in candidates) {
    if (candidate %in% exclude || candidate %in% selected) {
      next
    }
    if (has_self_overlap(candidate)) {
      next
    }
    selected <- c(selected, candidate)
    if (length(selected) >= n) {
      break
    }
  }
  if (length(selected) < n) {
    stop("Not enough pattern candidates were generated.", call. = FALSE)
  }
  selected
}

make_han_ngrams <- function(text, sizes = 2:4) {
  runs <- extract_regex(text, "\\p{Han}+")
  pieces <- vector("list", length(runs) * length(sizes))
  index <- 0L

  for (run in runs) {
    len <- nchar(run, type = "chars", allowNA = FALSE, keepNA = FALSE)
    for (size in sizes) {
      if (len < size) {
        next
      }
      starts <- seq_len(len - size + 1L)
      index <- index + 1L
      pieces[[index]] <- substring(run, starts, starts + size - 1L)
    }
  }

  unlist(pieces[seq_len(index)], use.names = FALSE)
}

source_files <- list.files(data_dir, pattern = "[.]txt$", full.names = TRUE)
english_files <- source_files[grepl(
  "Crime and Punishment|Pride and Prejudice|The Count of Monte Cristo",
  basename(source_files)
)]
chinese_files <- setdiff(source_files, english_files)

if (length(english_files) != 3L || length(chinese_files) == 0L) {
  stop("Unexpected benchmark data layout under bench/data/.", call. = FALSE)
}

en_docs <- make_corpus(english_files, "en_docs", normalize_english, chunk_chars, docs_n)
zh_docs <- make_corpus(chinese_files, "zh_docs", normalize_utf8, chunk_chars, docs_n)

mix_n <- min(nrow(en_docs), nrow(zh_docs), docs_n)
mix_docs <- data.frame(
  corpus_id = "mix_docs",
  doc_id = sprintf("mix_docs_%04d", seq_len(mix_n)),
  source = paste(en_docs$source[seq_len(mix_n)], zh_docs$source[seq_len(mix_n)], sep = " + "),
  chunk_id = seq_len(mix_n),
  doc = paste(
    substring(en_docs$doc[seq_len(mix_n)], 1L, floor(chunk_chars / 2L)),
    substring(zh_docs$doc[seq_len(mix_n)], 1L, floor(chunk_chars / 2L))
  ),
  stringsAsFactors = FALSE
)

documents <- rbind(en_docs, zh_docs, mix_docs)
row.names(documents) <- NULL

english_words <- extract_regex(paste(en_docs$doc, collapse = " "), "\\b[a-z]{3,14}\\b")
english_freq <- sort(table(english_words), decreasing = TRUE)

en_small_common <- select_patterns(names(english_freq), 32L)
rare_pool <- names(sort(english_freq[nchar(names(english_freq)) >= 7L & english_freq >= 2L]))
en_large_rare <- select_patterns(rare_pool, 256L, exclude = en_small_common)

zh_pattern_docs <- zh_docs$doc[seq_len(min(nrow(zh_docs), pattern_docs_n))]
zh_ngrams <- make_han_ngrams(paste(zh_pattern_docs, collapse = ""))
zh_freq <- sort(table(zh_ngrams), decreasing = TRUE)
zh_candidates <- names(zh_freq)[vapply(names(zh_freq), is_han_string, logical(1))]
zh_small_common <- select_patterns(zh_candidates, 32L)

pattern_sets <- list(
  en_small_common = en_small_common,
  en_large_rare = en_large_rare,
  zh_small_common = zh_small_common,
  mix_medium = c(en_small_common, zh_small_common),
  overlap_stress = c("disc", "disco", "discontent", "content")
)

pattern_meta <- do.call(
  rbind,
  lapply(names(pattern_sets), function(set_id) {
    patterns <- pattern_sets[[set_id]]
    data.frame(
      pattern_set_id = set_id,
      pattern_index = seq_along(patterns),
      pattern = patterns,
      pattern_bytes = nchar(patterns, type = "bytes"),
      stringsAsFactors = FALSE
    )
  })
)
row.names(pattern_meta) <- NULL

cases <- data.frame(
  case_id = c("en_small_common", "en_large_rare", "zh_small_common", "mix_medium"),
  corpus_id = c("en_docs", "en_docs", "zh_docs", "mix_docs"),
  pattern_set_id = c("en_small_common", "en_large_rare", "zh_small_common", "mix_medium"),
  stringsAsFactors = FALSE
)
cases$docs_n <- vapply(
  cases$corpus_id,
  function(id) {
    sum(documents$corpus_id == id)
  },
  integer(1)
)
cases$patterns_n <- vapply(
  cases$pattern_set_id,
  function(id) {
    sum(pattern_meta$pattern_set_id == id)
  },
  integer(1)
)
cases$input_bytes <- vapply(
  cases$corpus_id,
  function(id) {
    sum(nchar(documents$doc[documents$corpus_id == id], type = "bytes"))
  },
  numeric(1)
)

inputs <- list(
  generated_at = Sys.time(),
  root_dir = root_dir,
  docs_n = docs_n,
  chunk_chars = chunk_chars,
  pattern_docs_n = pattern_docs_n,
  seed = seed,
  documents = documents,
  pattern_meta = pattern_meta,
  cases = cases
)

saveRDS(inputs, file.path(output_dir, "benchmark-inputs.rds"))
write.csv(cases, file.path(output_dir, "cases.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(
  pattern_meta,
  file.path(output_dir, "pattern-sets.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

message("Wrote benchmark inputs to ", output_dir)
