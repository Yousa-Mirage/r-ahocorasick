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
#' returns one row per match. Character offsets are 1-based and inclusive, so
#' they can be used directly with `substr()`.
#'
#' @param ac An `<ac_automaton>` object created by `ac_build()`.
#' @param doc A character vector of documents to search.
#' @param overlapping Default is `FALSE`. If `TRUE`, report overlapping
#'   matches. This is only supported when `ac` was built with `match_kind = "standard"`.
#' @param na How to handle `NA` strings. `"omit"` skips them (default); `"error"` fails.
#'
#' @return A data frame with match metadata and character offsets. The columns are:
#'  * `doc_id`: Index of the input document in `doc`.
#'  * `pattern_id`: Index of the matched pattern in `ac_patterns(ac)`.
#'  * `start`: 1-based index of the first character in the match.
#'  * `end`: 1-based index of the last character in the match.
#' @export
ac_locate <- function(
  ac,
  doc,
  overlapping = FALSE,
  na = c("omit", "error")
) {
  ac <- validate_ac_automaton(ac)

  if (!checkmate::test_character(doc, min.len = 1L, all.missing = FALSE)) {
    cli::cli_abort("{.arg doc} must be a non-empty character vector.")
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
      "i" = "Use {.code na = \"omit\"} to skip missing values."
    ))
  }

  doc <- enc2utf8(doc)

  keep <- !is.na(doc)
  doc <- doc[keep]
  doc_ids <- as.integer(which(keep))

  if (length(doc) == 0L) {
    cli::cli_warn("No non-missing documents to search.")
    return(empty_locate_df())
  }

  raw <- rust_ac_locate(ac$ptr, doc, doc_ids, overlapping)

  if (length(raw$doc_id) == 0L) {
    cli::cli_warn("No matched patterns found.")
    return(empty_locate_df())
  }

  data.frame(raw)
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


#' Return patterns stored in an automaton
#'
#' @param ac An `<ac_automaton>` object created by `ac_build()`.
#'
#' @return A character vector of stored patterns.
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

# Create an empty data frame with the same structure as the output of `ac_locate()`.
empty_locate_df <- function() {
  data.frame(
    doc_id = integer(),
    pattern_id = integer(),
    start = integer(),
    end = integer()
  )
}
