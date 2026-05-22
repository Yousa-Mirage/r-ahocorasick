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
#' @seealso [ac_locate_df()], [ac_extract()], [ac_detect()], [ac_count()].
#'
#' @examples
#' if (requireNamespace("tidyverse", quietly = TRUE)) {
#'   ac <- ac_build(c("hello", "world"))
#'   tibble::tibble(doc = c("hello world", "nothing", "world")) |>
#'     dplyr::mutate(hits = ac_locate(ac, doc)) |>
#'     tidyr::unnest(hits)
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
  out <- lapply(doc, \(...) empty_locate_result())

  missing <- is.na(doc)
  if (na == "keep" && any(missing)) {
    out[missing] <- lapply(
      out[missing],
      \(...) missing_locate_result()
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
    data.frame(
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

locate_list_to_df <- function(hits) {
  rows <- lapply(seq_along(hits), \(doc_id) {
    hit <- hits[[doc_id]]
    n <- nrow(hit)
    if (n == 0L) {
      return(NULL)
    }

    data.frame(
      doc_id = rep.int(doc_id, n),
      hit
    )
  })

  compact_rows(rows, empty_locate_df)
}

empty_locate_result <- function() {
  data.frame(
    pattern_id = integer(),
    start = integer(),
    end = integer()
  )
}

missing_locate_result <- function() {
  data.frame(
    pattern_id = NA_integer_,
    start = NA_integer_,
    end = NA_integer_
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
