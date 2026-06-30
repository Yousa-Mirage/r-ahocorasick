#' Detect pattern matches in documents
#'
#' `ac_detect()` returns whether each document has at least one pattern match.
#'
#' @param ac An `<ac_automaton>` object created by `ac_build()`.
#' @param doc A character vector of documents to search.
#' @param ... Must be empty. This is used to require optional arguments to be
#'   supplied by name.
#' @param na How to handle `NA` documents. `"keep"` returns `NA`
#'   (default); `"false"` treats missing documents as not matched; `"error"`
#'   fails.
#'
#' @return A logical vector with the same length as `doc`.
#' @seealso [ac_detect_file()], [ac_count()], [ac_locate()], [ac_extract()].
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
  ...,
  na = c("keep", "false", "error")
) {
  rlang::check_dots_empty()

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

#' Detect pattern matches in files
#'
#' `ac_detect_file()` returns whether each file has at least one pattern match.
#'
#' @param ac An `<ac_automaton>` object created by `ac_build()`.
#' @param path A vector of file paths to search.
#' @param ... Must be empty. This is used to require optional arguments to be
#'   supplied by name.
#' @param stream If `FALSE` (default), each file is read into memory before
#'   searching. If `TRUE`, files are searched as streams. Stream search requires
#'   an automaton built with `match_kind = "standard"`.
#'
#' @return A logical vector with the same length as `path`.
#' @seealso [ac_detect()], [ac_count_file()], [ac_locate_bytes()].
#'
#' @examples
#' ac <- ac_build(c("hello", "world"))
#' path <- tempfile()
#' writeLines("hello world", path)
#' ac_detect_file(ac, path)
#' @export
ac_detect_file <- function(ac, path, ..., stream = FALSE) {
  rlang::check_dots_empty()

  ac <- validate_ac_automaton(ac)
  stream <- validate_file_stream(stream)
  if (stream) {
    ac <- validate_match_kind_for_file(ac)
  }
  path <- validate_stream_file_path(path)

  out <- if (stream) {
    rust_ac_detect_file_stream(ac$ptr, unname(path))
  } else {
    rust_ac_detect_file(ac$ptr, unname(path))
  }
  names(out) <- names(path)
  out
}
