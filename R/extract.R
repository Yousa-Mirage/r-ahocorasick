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
  out <- lapply(doc, \(...) empty_extract_result())

  missing <- is.na(doc)
  if (na == "keep" && any(missing)) {
    out[missing] <- lapply(
      out[missing],
      \(...) missing_extract_result()
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
    data.frame(
      matches = raw$matches[rows],
      patterns = unname(ac$patterns[raw$pattern_id[rows]])
    )
  })
  indices <- as.integer(names(row_ids))
  out[indices] <- res_list

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
  overlapping = FALSE,
  na = c("omit", "keep", "error")
) {
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

empty_extract_result <- function() {
  data.frame(
    matches = character(),
    patterns = character()
  )
}

missing_extract_result <- function() {
  data.frame(
    matches = NA_character_,
    patterns = NA_character_
  )
}

empty_extract_df <- function() {
  data.frame(
    doc_id = integer(),
    matches = character(),
    patterns = character()
  )
}
