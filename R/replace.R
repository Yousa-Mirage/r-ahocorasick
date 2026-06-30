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
#' @param ... Must be empty. This is used to require optional arguments to be
#'   supplied by name.
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
  ...,
  na = c("keep", "empty", "error")
) {
  rlang::check_dots_empty()

  ac <- validate_ac_automaton(ac)

  if (!checkmate::test_character(doc)) {
    cli::cli_abort("{.arg doc} must be a character vector.")
  }
  na <- rlang::arg_match(na)
  replace_with <- validate_replace_with(ac, replace_with)

  if (na == "error" && anyNA(doc)) {
    cli::cli_abort(c(
      "x" = "{.arg doc} must not contain missing values because {.arg na = \"error\"}.",
      "i" = "Use {.code na = \"keep\"} to keep missing values as {.code NA}."
    ))
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

#' Replace pattern matches in files
#'
#' `ac_replace_file()` replaces all non-overlapping matches in input files and
#' writes the result to output files.
#'
#' @param ac An `<ac_automaton>` object created by `ac_build()`.
#' @param path A vector of input file paths to search and replace.
#' @param replace_with A character vector of replacements. If length 1, the
#'   same replacement is used for every pattern. Otherwise, it **MUST** have the
#'   same length as `ac_patterns(ac)`, and replacements are matched to patterns
#'   by position.
#' @param ... Must be empty. This is used to require optional arguments to be
#'   supplied by name.
#' @param output A vector of output file paths. It must have the same
#'   length as `path`. If `NULL`, output paths are created by adding
#'   `"_replaced"` suffix. Existing output files are overwritten.
#' @param stream If `FALSE` (default), each file is read into memory before
#'   replacement. If `TRUE`, files are searched and replaced as streams. Stream
#'   replacement requires an automaton built with `match_kind = "standard"`.
#'
#' @return A character vector of output file paths with the same length as
#'   `path`.
#' @seealso [ac_replace()], [ac_detect_file()], [ac_count_file()].
#'
#' @examples
#' ac <- ac_build(c("fox", "brown", "quick"))
#' path <- tempfile(fileext = ".txt")
#' writeLines("The quick brown fox.", path)
#' ac_replace_file(path = path, ac = ac, replace_with = c("sloth", "grey", "slow"))
#' @export
ac_replace_file <- function(
  ac,
  path,
  replace_with,
  ...,
  output = NULL,
  stream = FALSE
) {
  rlang::check_dots_empty()

  ac <- validate_ac_automaton(ac)
  stream <- validate_file_stream(stream)
  if (stream) {
    ac <- validate_match_kind_for_file(ac)
  }
  path <- validate_stream_file_path(path)
  replace_with <- validate_replace_with(ac, replace_with)
  output <- validate_replace_output_path(path, output)

  out <- if (stream) {
    rust_ac_replace_file_stream(ac$ptr, unname(path), unname(output), replace_with)
  } else {
    rust_ac_replace_file(ac$ptr, unname(path), unname(output), replace_with)
  }
  names(out) <- names(path)
  out
}

validate_replace_with <- function(ac, replace_with) {
  if (!checkmate::test_character(replace_with, any.missing = FALSE)) {
    cli::cli_abort(
      "{.arg replace_with} must be a character vector with no missing values.",
      call = rlang::caller_env()
    )
  }

  if (!(length(replace_with) == 1L || length(replace_with) == length(ac))) {
    cli::cli_abort(
      c(
        "x" = "{.arg replace_with} must have length 1 or the same length as {.fn ac_patterns}.",
        "i" = "Use one replacement for all patterns, or one replacement per pattern."
      ),
      call = rlang::caller_env()
    )
  }

  if (length(replace_with) == 1L) {
    replace_with <- rep(replace_with, length(ac))
  }

  enc2utf8(replace_with)
}

validate_replace_output_path <- function(path, output = NULL) {
  if (is.null(output)) {
    return(add_replaced_suffix(path))
  }

  if (!checkmate::test_character(output, any.missing = FALSE)) {
    cli::cli_abort(
      "{.arg output} must be a character vector with no missing values.",
      call = rlang::caller_env()
    )
  }

  if (length(output) != length(path)) {
    cli::cli_abort(
      "{.arg output} must have the same length as {.arg path}.",
      call = rlang::caller_env()
    )
  }

  for (out in output) {
    checkmate::assert_path_for_output(out, overwrite = TRUE)
  }

  output <- normalize_output_path(output)
  names(output) <- names(path)

  duplicated_output <- duplicated(output) | duplicated(output, fromLast = TRUE)
  if (any(duplicated_output)) {
    cli::cli_abort(
      c(
        "x" = "{.arg output} paths must be unique.",
        "{.path {output[which(duplicated_output)]}}"
      ),
      call = rlang::caller_env()
    )
  }

  input_output_overlap <- output %in% normalize_output_path(path)
  if (any(input_output_overlap)) {
    cli::cli_abort(
      c(
        "x" = "{.arg output} must not be the same file as {.arg path}.",
        "{.path {output[input_output_overlap]}}"
      ),
      call = rlang::caller_env()
    )
  }

  output
}

add_replaced_suffix <- function(path) {
  file <- fs::path_file(path)
  ext <- fs::path_ext(file)
  stem <- fs::path_ext_remove(file)

  replaced_file <- paste0(stem, "_replaced")

  replaced_file <- ifelse(
    nzchar(ext),
    fs::path_ext_set(replaced_file, ext),
    replaced_file
  )
  fs::path(fs::path_dir(path), replaced_file)
}

normalize_output_path <- function(path) {
  path <- enc2utf8(path)
  fs::path(
    fs::path_real(fs::path_dir(path)),
    fs::path_file(path)
  )
}
