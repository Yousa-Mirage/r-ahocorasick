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
#' @seealso [ac_count_file()], [ac_detect()], [ac_locate()], [ac_extract()].
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

#' Count pattern matches in files
#'
#' `ac_count_file()` returns the number of pattern matches in each file.
#'
#' @param ac An `<ac_automaton>` object created by `ac_build()`.
#' @param path A vector of file paths to search.
#' @param stream If `FALSE` (default), each file is read into memory before
#'   searching. If `TRUE`, files are searched as streams. Stream search requires
#'   an automaton built with `match_kind = "standard"`.
#' @param overlapping Default is `FALSE`. If `TRUE`, count overlapping matches.
#'   This is only supported when `stream = FALSE` and `ac` was built with
#'   `match_kind = "standard"`.
#'
#' @return An integer vector with the same length as `path`.
#' @seealso [ac_count()], [ac_detect_file()], [ac_locate_bytes()].
#'
#' @examples
#' ac <- ac_build(c("hello", "world"))
#' path <- tempfile()
#' writeLines("hello hello world", path)
#' ac_count_file(ac, path)
#' @export
ac_count_file <- function(ac, path, stream = FALSE, overlapping = FALSE) {
  ac <- validate_ac_automaton(ac)
  stream <- validate_file_stream(stream)
  overlapping <- validate_file_overlapping(ac, overlapping, stream)
  if (stream) {
    ac <- validate_match_kind_for_file(ac)
  }
  path <- validate_stream_file_path(path)

  out <- if (stream) {
    rust_ac_count_file_stream(ac$ptr, unname(path))
  } else {
    rust_ac_count_file(ac$ptr, unname(path), overlapping)
  }
  names(out) <- names(path)
  out
}
