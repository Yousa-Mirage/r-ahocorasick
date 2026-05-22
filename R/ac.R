#' Build an Aho-Corasick automaton
#'
#' `ac_build()` compiles a character vector of patterns into a reusable
#' automaton backed by the Rust `aho-corasick` crate.
#'
#' @param patterns A character vector of non-empty patterns.
#' @param match_kind Matching semantics:
#'   * `"standard"` supports overlapping search (Default).
#'   * `"leftmost_first"` returns leftmost non-overlapping matches, breaking
#'     ties by pattern order.
#'   * `"leftmost_longest"` returns leftmost non-overlapping matches, breaking
#'     ties by longest match.
#' @param implementation Rust automaton implementation. `"auto"` lets the
#'   crate choose.
#' @param ascii_case_insensitive Use ASCII-only case-insensitive matching. Default is `FALSE`.
#' @param duplicate How duplicate patterns are handled.
#'
#' @return An immutable `<ac_automaton>` object.
#' @seealso [ac_locate()], [ac_detect()], [ac_count()], [ac_extract()],
#'   [ac_patterns()], [ac_info()].
#'
#' @examples
#' ac <- ac_build(c("hello", "world"))
#' length(ac)
#' ac_info(ac)
#' @export
ac_build <- function(
  patterns,
  match_kind = c("standard", "leftmost_first", "leftmost_longest"),
  implementation = c("auto", "noncontiguous_nfa", "contiguous_nfa", "dfa"),
  ascii_case_insensitive = FALSE,
  duplicate = c("keep", "error", "deduplicate")
) {
  match_kind <- rlang::arg_match(match_kind)
  implementation <- rlang::arg_match(implementation)
  duplicate <- rlang::arg_match(duplicate)

  if (!checkmate::test_character(patterns, any.missing = FALSE, min.len = 1L, min.chars = 1L)) {
    cli::cli_abort(
      "{.arg patterns} must be a character vector of non-empty strings with no missing values."
    )
  }
  if (!checkmate::test_flag(ascii_case_insensitive)) {
    cli::cli_abort("{.arg ascii_case_insensitive} must be a logical value.")
  }

  if (duplicate == "error" && anyDuplicated(patterns) > 0L) {
    cli::cli_abort(c(
      "x" = "{.arg patterns} must not contain duplicates because {.arg duplicate = \"error\"}.",
      "i" = "Use {.code duplicate = \"keep\"} or {.code duplicate = \"deduplicate\"}."
    ))
  }
  if (duplicate == "deduplicate") {
    keep <- !duplicated(patterns)
    patterns <- patterns[keep]
  }

  patterns_utf8 <- enc2utf8(patterns)
  ptr <- rust_ac_build(
    patterns_utf8,
    match_kind,
    implementation,
    ascii_case_insensitive
  )
  info <- rust_ac_info(ptr)

  structure(
    list(
      ptr = ptr,
      patterns = patterns_utf8,
      options = list(
        match_kind = match_kind,
        implementation = implementation,
        ascii_case_insensitive = ascii_case_insensitive,
        duplicate = duplicate
      ),
      info = info
    ),
    class = "ac_automaton"
  )
}

#' Locate pattern matches in strings
#'
#' `ac_locate()` searches a character vector with a compiled automaton and
#' returns one list element per document. Character offsets are 1-based and
#' inclusive, so they can be used directly with `substr()`.
#'
#' @param ac An `<ac_automaton>` object created by `ac_build()`.
#' @param doc A character vector of documents to search.
#' @param overlapping Default is `FALSE`. If `TRUE`, report overlapping
#'   matches. This is only supported when `ac` was built with `match_kind = "standard"`.
#' @param na How to handle `NA` documents. `"keep"` returns `NA_integer_`
#'   in `pattern_id`, `start`, and `end` (default); `"empty"` treats missing
#'   documents as no matches; `"error"` fails.
#'
#' @return A list with the same length as `doc`. Each element is a list with
#'   three integer vectors:
#'  * `pattern_id`: Index of the matched pattern in `ac_patterns(ac)`.
#'  * `start`: 1-based index of the first character in each match.
#'  * `end`: 1-based index of the last character in each match.
#' @seealso [ac_locate_df()], [ac_extract()], [ac_detect()], [ac_count()].
#'
#' @examples
#' if (requireNamespace("dplyr", quietly = TRUE)) {
#'   ac <- ac_build(c("hello", "world"))
#'   docs <- data.frame(doc = c("hello world", "nothing", "world"))
#'   dplyr::mutate(docs, hits = ac_locate(ac, doc))
#' }
#' @export
ac_locate <- function(
  ac,
  doc,
  overlapping = FALSE,
  na = c("keep", "empty", "error")
) {
  ac <- validate_ac_automaton(ac)

  if (!checkmate::test_character(doc)) {
    cli::cli_abort("{.arg doc} must be a character vector.")
  }
  if (!checkmate::test_flag(overlapping)) {
    cli::cli_abort("{.arg overlapping} must be a logical value.")
  }
  na <- rlang::arg_match(na)

  if (overlapping && ac$info$match_kind != "standard") {
    cli::cli_abort(c(
      "{.code overlapping = TRUE} requires {.code match_kind = \"standard\"}.",
      "i" = "Rebuild the automaton with {.code match_kind = \"standard\"} to enable overlapping search."
    ))
  }

  if (na == "error" && anyNA(doc)) {
    cli::cli_abort(c(
      "x" = "{.arg doc} must not contain missing values because {.arg na = \"error\"}.",
      "i" = "Use {.code na = \"keep\"} to keep missing values as {.code NA}."
    ))
  }

  doc <- enc2utf8(doc)
  out <- lapply(doc, \(...) {
    list(
      pattern_id = integer(),
      start = integer(),
      end = integer()
    )
  })

  missing <- is.na(doc)
  if (na == "keep" && any(missing)) {
    out[missing] <- lapply(
      out[missing],
      \(...) {
        list(
          pattern_id = NA_integer_,
          start = NA_integer_,
          end = NA_integer_
        )
      }
    )
  }

  keep <- !missing
  if (!any(keep)) {
    return(out)
  }

  doc_ids <- as.integer(which(keep))
  raw <- rust_ac_locate(ac$ptr, doc[keep], doc_ids, overlapping)

  if (length(raw$doc_id) == 0L) {
    return(out)
  }

  row_ids <- split(seq_along(raw$doc_id), raw$doc_id)
  res_list <- lapply(row_ids, \(rows) {
    list(
      pattern_id = raw$pattern_id[rows],
      start = raw$start[rows],
      end = raw$end[rows]
    )
  })
  out[as.integer(names(row_ids))] <- res_list

  out
}

#' Locate pattern matches as a data frame
#'
#' `ac_locate_df()` is the data-frame form of [ac_locate()]. It is useful when
#' you want one row per match instead of one list element per document.
#'
#' @param ac An `<ac_automaton>` object created by `ac_build()`.
#' @param doc A character vector of documents to search.
#' @param overlapping Default is `FALSE`. If `TRUE`, report overlapping
#'   matches. This is only supported when `ac` was built with
#'   `match_kind = "standard"`.
#' @param na How to handle `NA` documents. `"omit"` drops missing documents
#'   (default); `"keep"` returns one row with missing result columns for each
#'   missing document; `"error"` fails.
#'
#' @return A data frame with one row per match and four columns:
#'   `doc_id`, `pattern_id`, `start`, and `end`.
#' @seealso [ac_locate()], [ac_extract_df()].
#'
#' @examples
#' ac <- ac_build(c("hello", "world"))
#' doc <- c("hello world", "nothing", "world hello")
#' ac_locate_df(ac, doc)
#' @export
ac_locate_df <- function(
  ac,
  doc,
  overlapping = FALSE,
  na = c("omit", "keep", "error")
) {
  na <- rlang::arg_match(na)
  if (!checkmate::test_character(doc)) {
    cli::cli_abort("{.arg doc} must be a character vector.")
  }
  if (na == "error" && anyNA(doc)) {
    cli::cli_abort(c(
      "x" = "{.arg doc} must not contain missing values because {.arg na = \"error\"}.",
      "i" = "Use {.code na = \"omit\"} to skip missing values."
    ))
  }

  hits <- ac_locate(
    ac,
    doc,
    overlapping = overlapping,
    na = ifelse(na == "omit", "empty", na)
  )
  locate_list_to_df(hits)
}

#' Detect pattern matches in documents
#'
#' `ac_detect()` returns whether each document has at least one pattern match.
#'
#' @param ac An `<ac_automaton>` object created by `ac_build()`.
#' @param doc A character vector of documents to search.
#' @param na How to handle `NA` documents. `"keep"` returns `NA`
#'   (default); `"false"` treats missing documents as not matched; `"error"`
#'   fails.
#'
#' @return A logical vector with the same length as `doc`.
#' @seealso [ac_count()], [ac_locate()], [ac_extract()].
#'
#' @examples
#' if (requireNamespace("dplyr", quietly = TRUE)) {
#'   ac <- ac_build(c("hello", "world"))
#'   docs <- data.frame(doc = c("hello world", "nothing", "world"))
#'   dplyr::mutate(docs, matched = ac_detect(ac, doc))
#' }
#' @export
ac_detect <- function(
  ac,
  doc,
  na = c("keep", "false", "error")
) {
  ac <- validate_ac_automaton(ac)

  if (!checkmate::test_character(doc)) {
    cli::cli_abort("{.arg doc} must be a character vector.")
  }
  na <- rlang::arg_match(na)

  if (na == "error" && anyNA(doc)) {
    cli::cli_abort(c(
      "x" = "{.arg doc} must not contain missing values because {.arg na = \"error\"}.",
      "i" = "Use {.code na = \"keep\"} to keep missing values as {.code NA}."
    ))
  }

  out <- rep(ifelse(na == "false", FALSE, NA), length(doc))
  names(out) <- names(doc)

  doc <- enc2utf8(doc)
  keep <- !is.na(doc)
  if (any(keep)) {
    out[keep] <- rust_ac_detect(ac$ptr, doc[keep])
  }

  out
}

#' Count pattern matches in documents
#'
#' `ac_count()` returns the number of pattern matches in each document.
#'
#' @param ac An `<ac_automaton>` object created by `ac_build()`.
#' @param doc A character vector of documents to search.
#' @param overlapping Default is `FALSE`. If `TRUE`, count overlapping
#'   matches. This is only supported when `ac` was built with
#'   `match_kind = "standard"`.
#' @param na How to handle `NA` documents. `"keep"` returns `NA_integer_`
#'   (default); `"zero"` treats missing documents as zero matches; `"error"`
#'   fails.
#'
#' @return An integer vector with the same length as `doc`.
#' @seealso [ac_detect()], [ac_locate()], [ac_extract()].
#'
#' @examples
#' if (requireNamespace("dplyr", quietly = TRUE)) {
#'   ac <- ac_build(c("hello", "world"))
#'   docs <- data.frame(doc = c("hello world", "nothing", "world"))
#'   dplyr::mutate(docs, n_matches = ac_count(ac, doc))
#' }
#' @export
ac_count <- function(
  ac,
  doc,
  overlapping = FALSE,
  na = c("keep", "zero", "error")
) {
  ac <- validate_ac_automaton(ac)

  if (!checkmate::test_character(doc)) {
    cli::cli_abort("{.arg doc} must be a character vector.")
  }
  if (!checkmate::test_flag(overlapping)) {
    cli::cli_abort("{.arg overlapping} must be a logical value.")
  }
  na <- rlang::arg_match(na)

  if (overlapping && ac$info$match_kind != "standard") {
    cli::cli_abort(c(
      "{.code overlapping = TRUE} requires {.code match_kind = \"standard\"}.",
      "i" = "Rebuild the automaton with {.code match_kind = \"standard\"} to enable overlapping search."
    ))
  }

  if (na == "error" && anyNA(doc)) {
    cli::cli_abort(c(
      "x" = "{.arg doc} must not contain missing values because {.arg na = \"error\"}.",
      "i" = "Use {.code na = \"keep\"} to keep missing values as {.code NA}."
    ))
  }

  out <- rep(ifelse(na == "zero", 0L, NA_integer_), length(doc))
  names(out) <- names(doc)

  doc <- enc2utf8(doc)
  keep <- !is.na(doc)
  if (any(keep)) {
    out[keep] <- rust_ac_count(ac$ptr, doc[keep], overlapping)
  }

  out
}

#' Extract pattern matches from documents
#'
#' `ac_extract()` returns one list element per document. Each element contains
#' the matched text and the corresponding pattern values.
#'
#' @param ac An `<ac_automaton>` object created by `ac_build()`.
#' @param doc A character vector of documents to search.
#' @param overlapping Default is `FALSE`. If `TRUE`, extract overlapping
#'   matches. This is only supported when `ac` was built with
#'   `match_kind = "standard"`.
#' @param na How to handle `NA` documents. `"keep"` returns `NA_character_`
#'   in both `matches` and `patterns` (default); `"empty"` treats missing
#'   documents as no matches; `"error"` fails.
#'
#' @return A list with the same length as `doc`. Each element is a list with
#'   two character vectors:
#'  * `matches`: Text matched in the document.
#'  * `patterns`: Pattern values corresponding to each match.
#' @seealso [ac_extract_df()], [ac_locate()], [ac_detect()], [ac_count()].
#'
#' @examples
#' if (requireNamespace("dplyr", quietly = TRUE)) {
#'   ac <- ac_build(c("hello", "world"))
#'   docs <- data.frame(doc = c("hello world", "nothing", "world"))
#'   dplyr::mutate(docs, matches = ac_extract(ac, doc))
#' }
#' @export
ac_extract <- function(
  ac,
  doc,
  overlapping = FALSE,
  na = c("keep", "empty", "error")
) {
  # Validate inputs
  ac <- validate_ac_automaton(ac)

  if (!checkmate::test_character(doc)) {
    cli::cli_abort("{.arg doc} must be a character vector.")
  }
  if (!checkmate::test_flag(overlapping)) {
    cli::cli_abort("{.arg overlapping} must be a logical value.")
  }
  na <- rlang::arg_match(na)

  if (overlapping && ac$info$match_kind != "standard") {
    cli::cli_abort(c(
      "{.code overlapping = TRUE} requires {.code match_kind = \"standard\"}.",
      "i" = "Rebuild the automaton with {.code match_kind = \"standard\"} to enable overlapping search."
    ))
  }

  if (na == "error" && anyNA(doc)) {
    cli::cli_abort(c(
      "x" = "{.arg doc} must not contain missing values because {.arg na = \"error\"}.",
      "i" = "Use {.code na = \"keep\"} to keep missing values as {.code NA}."
    ))
  }

  # Initialize output with empty matches/patterns
  doc <- enc2utf8(doc)
  out <- lapply(doc, \(...) list(matches = character(), patterns = character()))

  missing <- is.na(doc)
  if (na == "keep" && any(missing)) {
    out[missing] <- lapply(
      out[missing],
      \(...) list(matches = NA_character_, patterns = NA_character_)
    )
  }

  keep <- !missing
  if (!any(keep)) {
    return(out)
  }

  # Extract matches by Rust
  doc_ids <- as.integer(which(keep))
  raw <- rust_ac_extract(ac$ptr, doc[keep], doc_ids, overlapping)

  if (length(raw$doc_id) == 0L) {
    return(out)
  }

  # Group matches by document and map pattern IDs to values
  row_ids <- split(seq_along(raw$doc_id), raw$doc_id)
  res_list <- lapply(row_ids, \(rows) {
    list(
      matches = raw$matches[rows],
      patterns = unname(ac$patterns[raw$pattern_id[rows]])
    )
  })
  indices <- as.integer(names(row_ids))
  out[indices] <- res_list

  out
}

#' Extract pattern matches as a data frame
#'
#' `ac_extract_df()` is the data-frame form of [ac_extract()]. It is useful when
#' you want one row per match instead of one list element per document.
#'
#' @param ac An `<ac_automaton>` object created by `ac_build()`.
#' @param doc A character vector of documents to search.
#' @param overlapping Default is `FALSE`. If `TRUE`, extract overlapping
#'   matches. This is only supported when `ac` was built with
#'   `match_kind = "standard"`.
#' @param na How to handle `NA` documents. `"omit"` drops missing documents
#'   (default); `"keep"` returns one row with missing result columns for each
#'   missing document; `"error"` fails.
#'
#' @return A data frame with one row per match and three columns:
#'   `doc_id`, `matches`, and `patterns`.
#' @seealso [ac_extract()], [ac_locate_df()].
#'
#' @examples
#' ac <- ac_build(c("hello", "world"))
#' doc <- c("hello world", "nothing", "world hello")
#' ac_extract_df(ac, doc)
#' @export
ac_extract_df <- function(
  ac,
  doc,
  overlapping = FALSE,
  na = c("omit", "keep", "error")
) {
  na <- rlang::arg_match(na)
  if (!checkmate::test_character(doc)) {
    cli::cli_abort("{.arg doc} must be a character vector.")
  }
  if (na == "error" && anyNA(doc)) {
    cli::cli_abort(c(
      "x" = "{.arg doc} must not contain missing values because {.arg na = \"error\"}.",
      "i" = "Use {.code na = \"omit\"} to skip missing values."
    ))
  }

  hits <- ac_extract(
    ac,
    doc,
    overlapping = overlapping,
    na = ifelse(na == "omit", "empty", na)
  )
  extract_list_to_df(hits)
}


#' Return patterns stored in an automaton
#'
#' @param ac An `<ac_automaton>` object created by `ac_build()`.
#'
#' @return A character vector of stored patterns.
#' @seealso [ac_build()], [ac_info()].
#'
#' @examples
#' ac <- ac_build(c("hello", "world"))
#' ac_patterns(ac)
#' @export
ac_patterns <- function(ac) {
  ac <- validate_ac_automaton(ac)
  ac$patterns
}

#' Return automaton metadata
#'
#' @param ac An `<ac_automaton>` object created by `ac_build()`.
#'
#' @return A list of automaton metadata.
#' @seealso [ac_build()], [ac_patterns()].
#'
#' @examples
#' ac <- ac_build(c("hello", "world"))
#' ac_info(ac)
#' @export
ac_info <- function(ac) {
  ac <- validate_ac_automaton(ac)
  ac$info
}

#' @export
print.ac_automaton <- function(x, ...) {
  info <- ac_info(x)
  cat("<ac_automaton>\n")
  cat("patterns:", info$patterns_len, "\n")
  cat("match_kind:", info$match_kind, "\n")
  cat("implementation:", info$implementation, "\n")
  cat(
    "case:",
    if (isTRUE(info$ascii_case_insensitive)) "ASCII-insensitive" else "sensitive",
    "\n"
  )
  invisible(x)
}

#' @export
length.ac_automaton <- function(ac) {
  ac <- validate_ac_automaton(ac)
  ac$info$patterns_len
}

# Validate that an object is a properly constructed <ac_automaton>.
validate_ac_automaton <- function(ac) {
  if (!inherits(ac, "ac_automaton")) {
    cli::cli_abort("{.arg ac} must be an <ac_automaton> object.")
  }
  if (is.null(ac$ptr)) {
    cli::cli_abort("{.arg ac} must contain a non-null Rust external pointer.")
  }
  ac
}

locate_list_to_df <- function(hits) {
  rows <- lapply(seq_along(hits), \(doc_id) {
    hit <- hits[[doc_id]]
    n <- length(hit$pattern_id)
    if (n == 0L) {
      return(NULL)
    }

    data.frame(
      doc_id = rep.int(doc_id, n),
      pattern_id = hit$pattern_id,
      start = hit$start,
      end = hit$end
    )
  })

  compact_rows(rows, empty_locate_df)
}

extract_list_to_df <- function(hits) {
  rows <- lapply(seq_along(hits), \(doc_id) {
    hit <- hits[[doc_id]]
    n <- length(hit$matches)
    if (n == 0L) {
      return(NULL)
    }

    data.frame(
      doc_id = rep.int(doc_id, n),
      matches = hit$matches,
      patterns = hit$patterns
    )
  })

  compact_rows(rows, empty_extract_df)
}

compact_rows <- function(rows, empty) {
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0L) {
    return(empty())
  }

  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

empty_locate_df <- function() {
  data.frame(
    doc_id = integer(),
    pattern_id = integer(),
    start = integer(),
    end = integer()
  )
}

empty_extract_df <- function() {
  data.frame(
    doc_id = integer(),
    matches = character(),
    patterns = character()
  )
}
