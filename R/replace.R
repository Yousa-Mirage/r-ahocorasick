#' Replace pattern matches in documents
#'
#' `ac_replace()` replaces all non-overlapping matches in each document with
#' the corresponding replacement string.
#'
#' @param ac An `<ac_automaton>` object created by `ac_build()`.
#' @param doc A character vector of documents to search and replace.
#' @param replace_with A character vector of replacements. If length 1, the
#'   same replacement is used for every pattern. Otherwise, it **MUST** have the
#'   same length as `ac_patterns(ac)`, and replacements are matched to patterns
#'   by position.
#' @param na How to handle `NA` documents. `"keep"` returns `NA_character_`
#'   (default); `"empty"` treats missing documents as empty strings; `"error"`
#'   fails.
#'
#' @return A character vector with the same length and names as `doc`.
#' @seealso [ac_build()], [ac_detect()], [ac_count()], [ac_extract()],
#'   [ac_locate()].
#'
#' @examples
#' ac <- ac_build(c("fox", "brown", "quick"))
#' ac_replace(
#'   ac,
#'   "The quick brown fox.",
#'   c("sloth", "grey", "slow")
#' )
#'
#' ac <- ac_build(c("append", "appendage", "app"), match_kind = "leftmost_first")
#' ac_replace(ac, "append the app to the appendage", c("x", "y", "z"))
#' @export
ac_replace <- function(
  ac,
  doc,
  replace_with,
  na = c("keep", "empty", "error")
) {
  ac <- validate_ac_automaton(ac)

  if (!checkmate::test_character(doc)) {
    cli::cli_abort("{.arg doc} must be a character vector.")
  }
  if (!checkmate::test_character(replace_with, any.missing = FALSE)) {
    cli::cli_abort(
      "{.arg replace_with} must be a character vector with no missing values."
    )
  }
  na <- rlang::arg_match(na)

  if (!(length(replace_with) == 1L || length(replace_with) == length(ac))) {
    cli::cli_abort(c(
      "x" = "{.arg replace_with} must have length 1 or the same length as {.fn ac_patterns}.",
      "i" = "Use one replacement for all patterns, or one replacement per pattern."
    ))
  }
  if (na == "error" && anyNA(doc)) {
    cli::cli_abort(c(
      "x" = "{.arg doc} must not contain missing values because {.arg na = \"error\"}.",
      "i" = "Use {.code na = \"keep\"} to keep missing values as {.code NA}."
    ))
  }

  if (length(replace_with) == 1L) {
    replace_with <- rep(replace_with, length(ac))
  }

  out <- rep(ifelse(na == "empty", "", NA_character_), length(doc))
  names(out) <- names(doc)

  doc <- enc2utf8(doc)
  replace_with <- enc2utf8(replace_with)
  keep <- !is.na(doc)
  if (any(keep)) {
    out[keep] <- rust_ac_replace(ac$ptr, doc[keep], replace_with)
  }

  out
}
