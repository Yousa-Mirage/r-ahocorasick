# Run the benchmarks on the inputs from `bench/01-generate-benchmark-data.R`.
#
# Run from the package root:
#
#   Rscript bench/01-generate-benchmark-data.R
#   Rscript bench/02-run-benchmarks.R
#
# Settings, through environment variables:
#
# - `BENCH_MIN_TIME`: seconds to spend timing each expression (default 1).
# - `BENCH_MIN_ITERATIONS`: minimum timed iterations per expression (default 3).
# - `BENCH_CASES`: comma-separated case ids to run (default: all cases).
# - `BENCH_RESULTS_DIR`: where to write the results (default `bench/results`).
#
# The results are CSV files that `vignettes/articles/benchmarks.Rmd` reads.

if (!file.exists("DESCRIPTION") || !dir.exists("bench")) {
  stop("Run this script from the package root.", call. = FALSE)
}

input_path <- "bench/output/benchmark-inputs.rds"
if (!file.exists(input_path)) {
  stop("Run bench/01-generate-benchmark-data.R first.", call. = FALSE)
}

min_time <- as.numeric(Sys.getenv("BENCH_MIN_TIME", "1"))
min_iterations <- as.integer(Sys.getenv("BENCH_MIN_ITERATIONS", "3"))
results_dir <- Sys.getenv("BENCH_RESULTS_DIR", "bench/results")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

compared_packages <- c("polars", "AhoCorasickTrie", "stringr")
missing <- compared_packages[
  !vapply(compared_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing) > 0L || !requireNamespace("bench", quietly = TRUE)) {
  stop(
    "Install the benchmark packages first: ",
    paste(c("bench", missing), collapse = ", "),
    call. = FALSE
  )
}

# Benchmark the package source in this checkout, as an optimized build.
Sys.unsetenv("DEBUG")
suppressPackageStartupMessages(pkgload::load_all(
  ".",
  quiet = TRUE,
  debug = FALSE,
  compile = TRUE
))
pl <- polars::pl

inputs <- readRDS(input_path)
corpora <- inputs$corpora
pattern_sets <- inputs$pattern_sets
cases <- inputs$cases
case_filter <- Sys.getenv("BENCH_CASES", "")
if (nzchar(case_filter)) {
  cases <- cases[cases$case_id %in% strsplit(case_filter, ",")[[1]], ]
}

# Methods ----------------------------------------------------------------------
#
# Every method answers three questions about each document, with overlapping
# matches:
#
# - detect: does it contain any pattern? A logical vector.
# - count: how many matches does it contain? An integer vector.
# - extract: which matches does it contain? A data frame with one row per match
#   and columns `doc_id` and `match` (ahocorasick also adds `patterns`).
#
# The timed functions return these R objects, so each timing includes turning
# the result into something R code can use. The exception is `polars_native`,
# which keeps both input and output inside polars, as a polars workflow would.

base_hits <- function(docs, pattern) {
  # The patterns are valid UTF-8, so byte matches are character matches.
  matches <- gregexpr(pattern, docs, fixed = TRUE, useBytes = TRUE)
  positions <- unlist(matches, use.names = FALSE)
  doc_ids <- rep.int(seq_along(matches), lengths(matches))
  doc_ids[positions > 0L]
}

# Search one pattern at a time, and stop searching a document once it matched.
detect_one_by_one <- function(docs, patterns, detect) {
  out <- logical(length(docs))
  todo <- seq_along(docs)
  for (pattern in patterns) {
    hit <- detect(docs[todo], pattern)
    out[todo[hit]] <- TRUE
    todo <- todo[!hit]
    if (length(todo) == 0L) {
      break
    }
  }
  out
}

make_methods <- function(docs, patterns) {
  ac <- ac_build(patterns)
  docs_df <- pl$DataFrame(doc = docs)
  patterns_list <- list(patterns)

  polars_detect <- function(df) {
    df$select(detected = pl$col("doc")$str$contains_any(patterns_list))
  }
  polars_count <- function(df) {
    df$select(
      n = pl$col("doc")$str$find_many(
        patterns_list,
        overlapping = TRUE
      )$list$len()
    )
  }
  polars_extract <- function(df) {
    df$with_row_index("doc_id", offset = 1L)$select(
      pl$col("doc_id"),
      match = pl$col("doc")$str$extract_many(patterns_list, overlapping = TRUE)
    )$explode("match")$drop_nulls()
  }
  polars_to_r <- list(
    detect = function(x) x$get_column("detected")$to_r_vector(),
    count = function(x) as.integer(x$get_column("n")$to_r_vector()),
    extract = function(x) {
      out <- as.data.frame(x)
      out$doc_id <- as.integer(out$doc_id)
      out
    }
  )

  trie_search <- function() {
    AhoCorasickTrie::AhoCorasickSearch(patterns, docs, groupByKeyword = FALSE)
  }

  list(
    ahocorasick = list(
      detect = function() ac_detect(ac, docs),
      count = function() ac_count(ac, docs, overlapping = TRUE),
      extract = function() ac_extract_df(ac, docs, overlapping = TRUE),
      to_extract = function(x) data.frame(doc_id = x$doc_id, match = x$matches)
    ),
    polars = list(
      detect = function() {
        polars_to_r$detect(polars_detect(pl$DataFrame(doc = docs)))
      },
      count = function() {
        polars_to_r$count(polars_count(pl$DataFrame(doc = docs)))
      },
      extract = function() {
        polars_to_r$extract(polars_extract(pl$DataFrame(doc = docs)))
      }
    ),
    polars_native = list(
      detect = function() polars_detect(docs_df),
      count = function() polars_count(docs_df),
      extract = function() polars_extract(docs_df),
      to_detect = polars_to_r$detect,
      to_count = polars_to_r$count,
      to_extract = polars_to_r$extract
    ),
    # AhoCorasickTrie has no separate build step: every search builds a trie.
    # It only supports ASCII.
    AhoCorasickTrie = list(
      supported = !any(grepl("[^ -~]", c(docs, patterns))),
      detect = function() lengths(trie_search()) > 0L,
      count = function() lengths(trie_search()),
      extract = function() {
        res <- trie_search()
        # Each match flattens to its keyword followed by its offset.
        flat <- unlist(res, use.names = FALSE)
        data.frame(
          doc_id = rep.int(seq_along(res), lengths(res)),
          match = flat[c(TRUE, FALSE)]
        )
      }
    ),
    base = list(
      detect = function() {
        detect_one_by_one(docs, patterns, function(x, pattern) {
          grepl(pattern, x, fixed = TRUE, useBytes = TRUE)
        })
      },
      count = function() {
        hits <- unlist(lapply(patterns, base_hits, docs = docs))
        tabulate(hits, nbins = length(docs))
      },
      # A fixed pattern always matches its own text, so the match is the
      # pattern repeated, which is much faster than `regmatches()`.
      extract = function() {
        hits <- lapply(patterns, base_hits, docs = docs)
        data.frame(
          doc_id = unlist(hits, use.names = FALSE),
          match = rep.int(patterns, lengths(hits))
        )
      }
    ),
    stringr = list(
      detect = function() {
        detect_one_by_one(docs, patterns, function(x, pattern) {
          stringr::str_detect(x, stringr::fixed(pattern))
        })
      },
      count = function() {
        Reduce(
          `+`,
          lapply(patterns, function(p) {
            stringr::str_count(docs, stringr::fixed(p))
          })
        )
      },
      extract = function() {
        hits <- lapply(patterns, function(p) {
          stringr::str_extract_all(docs, stringr::fixed(p))
        })
        data.frame(
          doc_id = unlist(lapply(hits, function(h) {
            rep.int(seq_along(h), lengths(h))
          })),
          match = unlist(hits, use.names = FALSE)
        )
      }
    )
  )
}

tasks <- c("detect", "count", "extract")

# Convert a method's result to the shape used to compare methods.
normalize <- function(method, task, value) {
  convert <- method[[paste0("to_", task)]]
  if (!is.null(convert)) {
    value <- convert(value)
  }
  switch(
    task,
    detect = as.logical(value),
    count = as.integer(value),
    extract = {
      out <- data.frame(
        doc_id = as.integer(value$doc_id),
        match = enc2utf8(as.character(value$match))
      )
      out <- out[order(out$doc_id, out$match, method = "radix"), ]
      row.names(out) <- NULL
      out
    }
  )
}

# Timing -----------------------------------------------------------------------

# Time `fun` without discarding iterations that ran the garbage collector:
# collecting garbage is part of the cost of producing many R objects.
time_it <- function(fun) {
  res <- bench::mark(
    fun(),
    min_time = min_time,
    min_iterations = min_iterations,
    max_iterations = 10000L,
    check = FALSE,
    filter_gc = FALSE,
    memory = FALSE,
    time_unit = "s"
  )
  times <- as.numeric(res$time[[1]]) * 1000
  data.frame(
    median_ms = stats::median(times),
    q1_ms = unname(stats::quantile(times, 0.25)),
    q3_ms = unname(stats::quantile(times, 0.75)),
    iterations = length(times),
    gc_iterations = sum(
      res$gc[[1]]$level0 + res$gc[[1]]$level1 + res$gc[[1]]$level2 > 0L
    )
  )
}

log_step <- function(...) {
  message(format(Sys.time(), "%H:%M:%S "), ...)
}

# Library comparison -----------------------------------------------------------

comparison_rows <- list()
correctness_rows <- list()
build_rows <- list()

for (i in seq_len(nrow(cases))) {
  case <- cases[i, ]
  docs <- corpora[[case$corpus_id]]
  patterns <- pattern_sets[[case$pattern_set_id]]
  methods <- make_methods(docs, patterns)

  log_step(case$case_id, ": ac_build()")
  build_rows[[case$case_id]] <- cbind(
    case[c("case_id", "pattern_set_id", "patterns_n")],
    time_it(function() ac_build(patterns))
  )

  # Base R is the reference: it is independent of every Aho-Corasick
  # implementation and simple enough to trust.
  reference <- lapply(
    stats::setNames(tasks, tasks),
    function(task) normalize(methods$base, task, methods$base[[task]]())
  )

  for (method_id in names(methods)) {
    method <- methods[[method_id]]
    supported <- !isFALSE(method$supported)

    for (task in tasks) {
      if (!supported) {
        correctness_rows[[length(correctness_rows) + 1L]] <- data.frame(
          case_id = case$case_id,
          method = method_id,
          task = task,
          status = "unsupported"
        )
        next
      }

      log_step(case$case_id, ": ", method_id, " ", task)
      # This first call also warms up the method before it is timed.
      value <- normalize(method, task, method[[task]]())
      same <- identical(value, reference[[task]])
      correctness_rows[[length(correctness_rows) + 1L]] <- data.frame(
        case_id = case$case_id,
        method = method_id,
        task = task,
        status = if (same) "same" else "different"
      )
      if (!same) {
        stop(
          method_id,
          " gives a different ",
          task,
          " result than base R for ",
          case$case_id,
          call. = FALSE
        )
      }

      comparison_rows[[length(comparison_rows) + 1L]] <- cbind(
        data.frame(case_id = case$case_id, method = method_id, task = task),
        time_it(method[[task]])
      )
    }
  }
}

comparison <- do.call(rbind, comparison_rows)
correctness <- do.call(rbind, correctness_rows)
build <- do.call(rbind, build_rows)

# ahocorasick functions --------------------------------------------------------
#
# Time each search function on a few document shapes. File functions search the
# same documents written to one file each; they are skipped for short
# documents, which would need 20,000 files.

api_cases <- intersect(
  c("en_medium_common", "zh_short_common", "en_long_common", "zh_long_common"),
  cases$case_id
)
api_rows <- list()

for (case_id in api_cases) {
  case <- cases[cases$case_id == case_id, ]
  docs <- corpora[[case$corpus_id]]
  patterns <- pattern_sets[[case$pattern_set_id]]
  ac <- ac_build(patterns)
  ac_ci <- ac_build(patterns, ascii_case_insensitive = TRUE)
  replacements <- rep("*", length(patterns))

  calls <- list(
    "ac_detect()" = function() ac_detect(ac, docs),
    "ac_count()" = function() ac_count(ac, docs),
    "ac_count(overlapping = TRUE)" = function() {
      ac_count(ac, docs, overlapping = TRUE)
    },
    "ac_locate()" = function() ac_locate(ac, docs),
    "ac_locate_df()" = function() ac_locate_df(ac, docs),
    "ac_locate_bytes()" = function() ac_locate_bytes(ac, docs),
    "ac_extract()" = function() ac_extract(ac, docs),
    "ac_extract_df()" = function() ac_extract_df(ac, docs),
    "ac_extract_df(), case-insensitive" = function() ac_extract_df(ac_ci, docs),
    "ac_replace()" = function() ac_replace(ac, docs, replacements)
  )

  if (!grepl("_short_", case_id)) {
    file_dir <- tempfile("ac-bench-")
    dir.create(file_dir)
    paths <- file.path(file_dir, sprintf("doc-%05d.txt", seq_along(docs)))
    for (j in seq_along(docs)) {
      writeBin(charToRaw(docs[[j]]), paths[[j]])
    }
    outputs <- file.path(file_dir, sprintf("out-%05d.txt", seq_along(docs)))
    calls <- c(
      calls,
      list(
        "ac_count_file()" = function() ac_count_file(ac, paths),
        "ac_count_file(stream = TRUE)" = function() {
          ac_count_file(ac, paths, stream = TRUE)
        },
        "ac_locate_file()" = function() ac_locate_file(ac, paths),
        "ac_extract_file()" = function() ac_extract_file(ac, paths),
        "ac_replace_file()" = function() {
          ac_replace_file(ac, paths, replacements, output = outputs)
        },
        "ac_replace_file(stream = TRUE)" = function() {
          ac_replace_file(
            ac,
            paths,
            replacements,
            output = outputs,
            stream = TRUE
          )
        }
      )
    )
  }

  for (call in names(calls)) {
    log_step(case_id, ": ", call)
    calls[[call]]()
    api_rows[[length(api_rows) + 1L]] <- cbind(
      data.frame(case_id = case_id, call = call),
      time_it(calls[[call]])
    )
  }
}

api <- do.call(rbind, api_rows)

# Environment ------------------------------------------------------------------

cpu <- tryCatch(
  switch(
    Sys.info()[["sysname"]],
    Darwin = system("sysctl -n machdep.cpu.brand_string", intern = TRUE),
    Linux = sub(
      ".*: ",
      "",
      grep("model name", readLines("/proc/cpuinfo"), value = TRUE)[[1]]
    ),
    Windows = Sys.getenv("PROCESSOR_IDENTIFIER"),
    NA_character_
  ),
  error = function(cnd) NA_character_
)

environment <- data.frame(
  key = c(
    "date",
    "r_version",
    "platform",
    "cpu",
    "min_time_seconds",
    "min_iterations",
    paste0("package_", c("ahocorasick", compared_packages, "bench"))
  ),
  value = c(
    format(Sys.Date()),
    R.version.string,
    R.version$platform,
    cpu,
    min_time,
    min_iterations,
    vapply(
      c("ahocorasick", compared_packages, "bench"),
      function(pkg) as.character(utils::packageVersion(pkg)),
      character(1)
    )
  )
)

# Results ----------------------------------------------------------------------

write_result <- function(df, name) {
  utils::write.csv(
    df,
    file.path(results_dir, paste0(name, ".csv")),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
}

write_result(cases, "cases")
write_result(build, "build")
write_result(correctness, "correctness")
write_result(comparison, "comparison")
write_result(api, "api")
write_result(environment, "environment")

log_step("Wrote results to ", results_dir)
