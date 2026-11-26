#' Return patterns stored in an automaton
#'
#' @param ac An `<ac_automaton>` object created by `ac_build()`.
#'
#' @return A character vector of stored patterns.
#' @seealso [ac_build()], [ac_info()].
#'
#' @examples
#' ac <- ac_build(c("hello", "world"))
#' ac_patterns(ac)
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
#' @seealso [ac_build()], [ac_patterns()].
#'
#' @examples
#' ac <- ac_build(c("hello", "world"))
#' ac_info(ac)
#' @export
ac_info <- function(ac) {
  ac <- validate_ac_automaton(ac)
  ac$info
}

#' @export
print.ac_automaton <- function(x, ...) {
  # Printing must not fail: an automaton restored with `readRDS()` or loaded
  # from a saved workspace keeps its R fields but loses its Rust pointer.
  if (!has_valid_ac_pointer(x)) {
    cat("<ac_automaton> (invalid pointer, rebuild with ac_build())\n")
    return(invisible(x))
  }

  info <- x$info
  cat("<ac_automaton>\n")
  cat("patterns:", info$patterns_len, "\n")
  cat("match_kind:", info$match_kind, "\n")
  cat("implementation:", info$implementation, "\n")
  cat(
    "case:",
    if (isTRUE(info$ascii_case_insensitive)) {
      "ASCII-insensitive"
    } else {
      "sensitive"
    },
    "\n"
  )
  invisible(x)
}

#' @export
length.ac_automaton <- function(x) {
  if (!is.list(x)) {
    return(length(unclass(x)))
  }
  length(.subset2(x, "patterns"))
}

# Return whether `ac` holds a pointer to a live Rust automaton. Never errors.
has_valid_ac_pointer <- function(ac) {
  if (!is.list(ac)) {
    return(FALSE)
  }
  ptr <- .subset2(ac, "ptr")
  if (is.null(ptr)) {
    return(FALSE)
  }
  isTRUE(
    tryCatch(
      rust_ac_is_valid(ptr),
      error = function(cnd) FALSE
    )
  )
}

# Validate that an object is a properly constructed <ac_automaton>.
validate_ac_automaton <- function(ac) {
  if (!inherits(ac, "ac_automaton")) {
    cli::cli_abort("{.arg ac} must be an <ac_automaton> object.")
  }
  if (is.null(ac$ptr)) {
    cli::cli_abort("{.arg ac} must contain a non-null Rust external pointer.")
  }
  if (!has_valid_ac_pointer(ac)) {
    cli::cli_abort(c(
      "{.arg ac} contains an invalid Rust external pointer.",
      "i" = "Rebuild it with {.fn ac_build}. External pointers cannot be restored across R sessions."
    ))
  }
  ac
}

# Convert strings to UTF-8 and reject any that are still not valid UTF-8.
as_valid_utf8 <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  # Capture the argument name before `x` is reassigned.
  force(arg)
  x <- enc2utf8(x)

  invalid <- which(!is.na(x) & !validUTF8(x))
  if (length(invalid) > 0L) {
    positions <- cli::cli_vec(invalid, list("vec-trunc" = 5L))
    cli::cli_abort(
      c(
        "{.arg {arg}} must contain valid UTF-8 strings.",
        "x" = "Invalid UTF-8 found at {cli::qty(length(invalid))}position{?s} {positions}.",
        "i" = "Re-encode the text with {.fn iconv}, or read it with the correct {.arg encoding}."
      ),
      call = call
    )
  }

  x
}

# Turn a named list of equal-length columns into a data frame. Much cheaper than
# `data.frame()` or `structure()`, which matters when building one data frame
# per document.
new_df <- function(cols) {
  # Compact row names, as `.set_row_names()` creates, without the extra call.
  n <- length(cols[[1L]])
  attr(cols, "row.names") <- if (n > 0L) {
    c(NA_integer_, -n)
  } else {
    integer()
  }
  class(cols) <- "data.frame"
  cols
}

# Store one data frame per document (or file) in `out`, built from the flat
# match columns returned by Rust.
#
# `ids` gives the 1-based position in `out` of each match and must be sorted,
# which holds because Rust searches documents in order. `cols` is a named list
# of match columns, and `build` creates one data frame from one document's
# slice of them; its arguments must be named like `cols`. Elements of `out`
# without matches are left unchanged.
split_by_doc <- function(out, ids, cols, build) {
  if (length(ids) == 0L) {
    return(out)
  }

  runs <- rle(ids)
  n_docs <- length(runs$lengths)

  if (length(ids) <= 16L * n_docs) {
    # Many documents with few matches each: group every column in one
    # `split()` call. The cost is then dominated by calling `build()` once per
    # document, which is fastest with named arguments (not `...`).
    groups <- structure(
      rep.int(seq_len(n_docs), runs$lengths),
      levels = as.character(seq_len(n_docs)),
      class = "factor"
    )
    pieces <- lapply(cols, split.default, f = groups)
    out[runs$values] <- .mapply(build, pieces, NULL)
  } else {
    # Few documents with many matches each: a grouping factor as long as the
    # matches would cost more than it saves, so slice contiguous rows instead.
    ends <- cumsum(runs$lengths)
    starts <- ends - runs$lengths + 1L
    for (i in seq_len(n_docs)) {
      rows <- seq.int(starts[[i]], ends[[i]])
      out[[runs$values[[i]]]] <- do.call(build, lapply(cols, `[`, rows))
    }
  }

  out
}

compact_rows <- function(rows, empty) {
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0L) {
    return(empty())
  }

  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

validate_stream_file_path <- function(path) {
  if (!checkmate::test_character(path, any.missing = FALSE)) {
    cli::cli_abort(
      "{.arg path} must be a character vector with no missing values.",
      call = rlang::caller_env()
    )
  }

  path_names <- names(path)
  path <- normalizePath(
    as_valid_utf8(path, call = rlang::caller_env()),
    mustWork = FALSE
  )
  names(path) <- path_names

  file_exists <- fs::file_info(path)$type == "file"

  if (anyNA(file_exists)) {
    unexisting_files <- path[which(is.na(file_exists))]
    cli::cli_abort(
      c(
        "x" = "The following paths don't exist:",
        "{.path {unexisting_files}}"
      ),
      call = rlang::caller_env()
    )
  }

  if (!all(file_exists)) {
    non_file_paths <- path[which(!file_exists)]
    cli::cli_abort(
      c(
        "x" = "The following paths are not files:",
        "{.path {non_file_paths}}"
      ),
      call = rlang::caller_env()
    )
  }

  file_readable <- fs::file_access(path, "read")

  if (!all(file_readable)) {
    unreadable_files <- path[which(!file_readable)]
    cli::cli_abort(
      c(
        "x" = "Files are not readable:",
        "{.path {unreadable_files}}"
      ),
      call = rlang::caller_env()
    )
  }

  path
}

validate_file_stream <- function(stream) {
  if (!checkmate::test_flag(stream)) {
    cli::cli_abort(
      "{.arg stream} must be a logical value.",
      call = rlang::caller_env()
    )
  }

  stream
}

validate_match_kind_for_file <- function(ac_automaton) {
  if (ac_automaton$info$match_kind != "standard") {
    cli::cli_abort(
      c(
        "File stream search requires {.code match_kind = \"standard\"}.",
        "i" = "Rebuild the automaton with {.code match_kind = \"standard\"} to search files."
      ),
      call = rlang::caller_env()
    )
  }
  ac_automaton
}

validate_file_overlapping <- function(
  ac_automaton,
  overlapping,
  stream = FALSE
) {
  if (!checkmate::test_flag(overlapping)) {
    cli::cli_abort(
      "{.arg overlapping} must be a logical value.",
      call = rlang::caller_env()
    )
  }

  if (overlapping && stream) {
    cli::cli_abort(
      c(
        "{.code overlapping = TRUE} is only supported when {.code stream = FALSE}.",
        "i" = "Use the default non-streaming file search to enable overlapping search."
      ),
      call = rlang::caller_env()
    )
  }

  if (overlapping && ac_automaton$info$match_kind != "standard") {
    cli::cli_abort(
      c(
        "{.code overlapping = TRUE} requires {.code match_kind = \"standard\"}.",
        "i" = "Rebuild the automaton with {.code match_kind = \"standard\"} to enable overlapping file search."
      ),
      call = rlang::caller_env()
    )
  }

  overlapping
}
