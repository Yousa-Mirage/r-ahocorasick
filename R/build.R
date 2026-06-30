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
#' @param duplicate How duplicate patterns are handled:
#'   * `"keep"` preserves duplicates in their original order.
#'   * `"error"` fails if `patterns` contains duplicates.
#'   * `"deduplicate"` keeps the first occurrence of each pattern and drops
#'     later duplicates.
#'
#' @return An immutable `<ac_automaton>` object.
#' @seealso [ac_locate()], [ac_locate_df()], [ac_detect()], [ac_count()],
#'   [ac_extract()], [ac_extract_df()], [ac_replace()], [ac_patterns()].
#'
#' @examples
#' ac <- ac_build(c("hello", "world"))
#' length(ac)
#' ac_info(ac)
#' @export
ac_build <- function(
  patterns,
  ...,
  match_kind = c("standard", "leftmost_first", "leftmost_longest"),
  implementation = c("auto", "noncontiguous_nfa", "contiguous_nfa", "dfa"),
  ascii_case_insensitive = FALSE,
  duplicate = c("keep", "error", "deduplicate")
) {
  rlang::check_dots_empty()

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
