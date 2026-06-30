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
#' @param na How to handle `NA` documents. `"keep"` returns one row with
#'   missing `pattern_id`, `start`, and `end` values (default); `"empty"`
#'   treats missing documents as no matches; `"error"` fails.
#'
#' @return A list with the same length as `doc`. Each element is a data frame
#'   with one row per match and three columns:
#'  * `pattern_id`: Index of the matched pattern in `ac_patterns(ac)`.
#'  * `start`: 1-based index of the first character in each match.
#'  * `end`: 1-based index of the last character in each match.
#' @seealso [ac_locate_df()], [ac_locate_bytes()], [ac_extract()],
#'   [ac_detect()], [ac_count()].
#'
#' @examples
#' if (
#'   requireNamespace("dplyr", quietly = TRUE) &&
#'     requireNamespace("tibble", quietly = TRUE) &&
#'     requireNamespace("tidyr", quietly = TRUE)
#' ) {
#'   ac <- ac_build(c("hello", "world"))
#'   tibble::tibble(doc = c("hello world", "nothing", "world")) |>
#'     dplyr::mutate(hits = ac_locate(ac, doc)) |>
#'     tidyr::unnest(hits)
#' }
#' @export
ac_locate <- function(
  ac,
  doc,
  ...,
  overlapping = FALSE,
  na = c("keep", "empty", "error")
) {
  rlang::check_dots_empty()

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
  out <- rep(list(new_locate_result()), length(doc))
  names(out) <- names(doc)

  missing <- is.na(doc)
  if (na == "keep" && any(missing)) {
    out[missing] <- rep(
      list(new_locate_result(
        pattern_id = NA_integer_,
        start = NA_integer_,
        end = NA_integer_
      )),
      sum(missing)
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

  runs <- rle(raw$doc_id)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1L

  for (i in seq_along(starts)) {
    rows <- seq.int(starts[[i]], ends[[i]])
    out[[runs$values[[i]]]] <- new_locate_result(
      raw$pattern_id[rows],
      raw$start[rows],
      raw$end[rows]
    )
  }

  out
}

#' Locate pattern matches in files
#'
#' `ac_locate_file()` searches files with a compiled automaton and returns one
#' list element per file. Character offsets are 1-based and inclusive, so they
#' can be used directly with `substr()`.
#'
#' @details
#' File location search is always non-streaming. Converting byte offsets from a
#' streaming search into R-facing character offsets would require a second pass
#' over the same file to reconstruct UTF-8 character boundaries. Keeping
#' `ac_locate_file()` as a simple in-memory search is the clearest
#' implementation.
#'
#' @param ac An `<ac_automaton>` object created by `ac_build()`.
#' @param path A vector of file paths to search.
#' @param overlapping Default is `FALSE`. If `TRUE`, report overlapping
#'   matches. This is only supported when `ac` was built with
#'   `match_kind = "standard"`.
#'
#' @return A list with the same length as `path`. Each element is a data frame
#'   with one row per match and three columns:
#'  * `pattern_id`: Index of the matched pattern in `ac_patterns(ac)`.
#'  * `start`: 1-based index of the first character in each match.
#'  * `end`: 1-based index of the last character in each match.
#' @seealso [ac_locate()], [ac_detect_file()], [ac_count_file()],
#'   [ac_extract_file()].
#'
#' @examples
#' ac <- ac_build(c("hello", "world"))
#' path <- tempfile()
#' writeLines("hello world", path)
#' ac_locate_file(ac, path)
#' @export
ac_locate_file <- function(ac, path, ..., overlapping = FALSE) {
  rlang::check_dots_empty()

  ac <- validate_ac_automaton(ac)
  overlapping <- validate_file_overlapping(ac, overlapping)
  path <- validate_stream_file_path(path)

  out <- rep(list(new_locate_result()), length(path))
  names(out) <- names(path)

  raw <- rust_ac_locate_file(ac$ptr, unname(path), overlapping)

  if (length(raw$file_id) == 0L) {
    return(out)
  }

  runs <- rle(raw$file_id)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1L

  for (i in seq_along(starts)) {
    rows <- seq.int(starts[[i]], ends[[i]])
    out[[runs$values[[i]]]] <- new_locate_result(
      raw$pattern_id[rows],
      raw$start[rows],
      raw$end[rows]
    )
  }

  out
}

#' Locate pattern matches with byte offsets
#'
#' `ac_locate_bytes()` searches a character vector with a compiled automaton
#' and returns byte offsets from the Rust `aho-corasick` crate. Byte offsets are
#' 0-based, and `byte_end` is end-exclusive.
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
#'   `doc_id`, `pattern_id`, `byte_start`, and `byte_end`.
#' @seealso [ac_locate()], [ac_locate_df()].
#'
#' @examples
#' ac <- ac_build(c("hello", "world"))
#' doc <- c("hello world", "nothing", "world hello")
#' ac_locate_bytes(ac, doc)
#' @export
ac_locate_bytes <- function(
  ac,
  doc,
  ...,
  overlapping = FALSE,
  na = c("omit", "keep", "error")
) {
  rlang::check_dots_empty()

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
      "i" = "Use {.code na = \"omit\"} to skip missing values."
    ))
  }

  doc <- enc2utf8(doc)
  missing <- is.na(doc)
  keep <- !missing

  out <- empty_locate_bytes_df()
  if (any(keep)) {
    doc_ids <- as.integer(which(keep))
    raw <- rust_ac_locate_bytes(ac$ptr, doc[keep], doc_ids, overlapping)
    out <- data.frame(
      doc_id = as.integer(raw$doc_id),
      pattern_id = as.integer(raw$pattern_id),
      byte_start = as.numeric(raw$byte_start),
      byte_end = as.numeric(raw$byte_end)
    )
  }

  if (na == "keep" && any(missing)) {
    out <- rbind(
      out,
      data.frame(
        doc_id = as.integer(which(missing)),
        pattern_id = NA_integer_,
        byte_start = NA_real_,
        byte_end = NA_real_
      )
    )
    out <- out[order(out$doc_id), , drop = FALSE]
    row.names(out) <- NULL
  }

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
#' @seealso [ac_locate()], [ac_locate_bytes()], [ac_extract_df()].
#'
#' @examples
#' ac <- ac_build(c("hello", "world"))
#' doc <- c("hello world", "nothing", "world hello")
#' ac_locate_df(ac, doc)
#' @export
ac_locate_df <- function(
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
    raw <- rust_ac_locate(
      ac$ptr,
      doc[keep],
      as.integer(which(keep)),
      overlapping
    )

    if (length(raw$doc_id) == 0L) {
      empty_locate_df()
    } else {
      data.frame(
        doc_id = raw$doc_id,
        pattern_id = raw$pattern_id,
        start = raw$start,
        end = raw$end
      )
    }
  } else {
    empty_locate_df()
  }

  if (na == "keep" && any(missing)) {
    missing_rows <- data.frame(
      doc_id = as.integer(which(missing)),
      pattern_id = NA_integer_,
      start = NA_integer_,
      end = NA_integer_
    )
    out <- compact_rows(
      list(out, missing_rows),
      empty_locate_df
    )
    out <- out[order(out$doc_id), , drop = FALSE]
    row.names(out) <- NULL
  }

  out
}

new_locate_result <- function(
  pattern_id = integer(),
  start = integer(),
  end = integer()
) {
  structure(
    list(
      pattern_id = pattern_id,
      start = start,
      end = end
    ),
    class = "data.frame",
    row.names = .set_row_names(length(pattern_id))
  )
}

empty_locate_df <- function() {
  data.frame(
    doc_id = integer(),
    pattern_id = integer(),
    start = integer(),
    end = integer()
  )
}

empty_locate_bytes_df <- function() {
  data.frame(
    doc_id = integer(),
    pattern_id = integer(),
    byte_start = numeric(),
    byte_end = numeric()
  )
}
