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
