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
#' @param na How to handle `NA` documents. `"keep"` returns one row with
#'   missing `matches` and `patterns` values (default); `"empty"` treats
#'   missing documents as no matches; `"error"` fails.
#'
#' @return A list with the same length as `doc`. Each element is a data frame
#'   with one row per match and two columns:
#'  * `matches`: Text matched in the document.
#'  * `patterns`: Pattern values corresponding to each match.
#' @seealso [ac_extract_df()], [ac_locate()], [ac_detect()], [ac_count()].
#'
#' @examples
#' if (
#'   requireNamespace("dplyr", quietly = TRUE) &&
#'     requireNamespace("tibble", quietly = TRUE) &&
#'     requireNamespace("tidyr", quietly = TRUE)
#' ) {
#'   ac <- ac_build(c("hello", "world"))
#'   tibble::tibble(doc = c("hello world", "nothing", "world")) |>
#'     dplyr::mutate(extracted = ac_extract(ac, doc)) |>
#'     tidyr::unnest(extracted)
#' }
#' @export
ac_extract <- function(
  ac,
  doc,
  ...,
  overlapping = FALSE,
  na = c("keep", "empty", "error")
) {
  rlang::check_dots_empty()

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

  doc <- enc2utf8(doc)
  out <- rep(list(new_extract_result()), length(doc))
  names(out) <- names(doc)

  missing <- is.na(doc)
  if (na == "keep" && any(missing)) {
    out[missing] <- rep(
      list(new_extract_result(
        matches = NA_character_,
        patterns = NA_character_
      )),
      sum(missing)
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

  patterns <- unname(ac$patterns[raw$pattern_id])
  runs <- rle(raw$doc_id)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1L

  for (i in seq_along(starts)) {
    rows <- seq.int(starts[[i]], ends[[i]])
    out[[runs$values[[i]]]] <- new_extract_result(
      raw$matches[rows],
      patterns[rows]
    )
  }

  out
}

#' Extract pattern matches from files
#'
#' `ac_extract_file()` returns one list element per file. Each element contains
#' the matched text and the corresponding pattern values.
#'
#' @param ac An `<ac_automaton>` object created by `ac_build()`.
#' @param path A vector of file paths to search.
#' @param stream If `FALSE` (default), each file is read into memory before
#'   searching. If `TRUE`, files are searched as streams. Stream search requires
#'   an automaton built with `match_kind = "standard"`.
#' @param overlapping Default is `FALSE`. If `TRUE`, extract overlapping
#'   matches. This is only supported when `stream = FALSE` and `ac` was built
#'   with `match_kind = "standard"`.
#'
#' @return A list with the same length as `path`. Each element is a data frame
#'   with one row per match and two columns:
#'  * `matches`: Text matched in the file.
#'  * `patterns`: Pattern values corresponding to each match.
#' @seealso [ac_extract()], [ac_detect_file()], [ac_count_file()].
#'
#' @examples
#' ac <- ac_build(c("hello", "world"))
#' path <- tempfile()
#' writeLines("hello world", path)
#' ac_extract_file(ac, path)
#' @export
ac_extract_file <- function(ac, path, ..., stream = FALSE, overlapping = FALSE) {
  rlang::check_dots_empty()

  ac <- validate_ac_automaton(ac)
  stream <- validate_file_stream(stream)
  overlapping <- validate_file_overlapping(ac, overlapping, stream)
  if (stream) {
    ac <- validate_match_kind_for_file(ac)
  }
  path <- validate_stream_file_path(path)

  out <- rep(list(new_extract_result()), length(path))
  names(out) <- names(path)

  raw <- if (stream) {
    rust_ac_extract_file_stream(ac$ptr, unname(path))
  } else {
    rust_ac_extract_file(ac$ptr, unname(path), overlapping)
  }

  if (length(raw$file_id) == 0L) {
    return(out)
  }

  patterns <- unname(ac$patterns[raw$pattern_id])
  runs <- rle(raw$file_id)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1L

  for (i in seq_along(starts)) {
    rows <- seq.int(starts[[i]], ends[[i]])
    out[[runs$values[[i]]]] <- new_extract_result(
      raw$matches[rows],
      patterns[rows]
    )
  }

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
  ...,
  overlapping = FALSE,
  na = c("omit", "keep", "error")
) {
  rlang::check_dots_empty()

  ac <- validate_ac_automaton(ac)

  na <- rlang::arg_match(na)
  if (!checkmate::test_character(doc)) {
    cli::cli_abort("{.arg doc} must be a character vector.")
  }
  if (!checkmate::test_flag(overlapping)) {
    cli::cli_abort("{.arg overlapping} must be a logical value.")
  }
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
  missing <- is.na(doc)
  keep <- !missing

  out <- if (any(keep)) {
    raw <- rust_ac_extract(
      ac$ptr,
      doc[keep],
      as.integer(which(keep)),
      overlapping
    )

    if (length(raw$doc_id) == 0L) {
      empty_extract_df()
    } else {
      data.frame(
        doc_id = raw$doc_id,
        matches = raw$matches,
        patterns = unname(ac$patterns[raw$pattern_id])
      )
    }
  } else {
    empty_extract_df()
  }

  if (na == "keep" && any(missing)) {
    missing_rows <- data.frame(
      doc_id = as.integer(which(missing)),
      matches = NA_character_,
      patterns = NA_character_
    )
    out <- compact_rows(
      list(out, missing_rows),
      empty_extract_df
    )
    out <- out[order(out$doc_id), , drop = FALSE]
    row.names(out) <- NULL
  }

  out
}

new_extract_result <- function(
  matches = character(),
  patterns = character()
) {
  structure(
    list(
      matches = matches,
      patterns = patterns
    ),
    class = "data.frame",
    row.names = .set_row_names(length(matches))
  )
}

empty_extract_df <- function() {
  data.frame(
    doc_id = integer(),
    matches = character(),
    patterns = character()
  )
}
