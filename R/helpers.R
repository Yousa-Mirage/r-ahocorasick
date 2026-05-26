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
  info <- ac_info(x)
  cat("<ac_automaton>\n")
  cat("patterns:", info$patterns_len, "\n")
  cat("match_kind:", info$match_kind, "\n")
  cat("implementation:", info$implementation, "\n")
  cat(
    "case:",
    if (isTRUE(info$ascii_case_insensitive)) "ASCII-insensitive" else "sensitive",
    "\n"
  )
  invisible(x)
}

#' @export
length.ac_automaton <- function(x) {
  x <- validate_ac_automaton(x)
  x$info$patterns_len
}

# Validate that an object is a properly constructed <ac_automaton>.
validate_ac_automaton <- function(ac) {
  if (!inherits(ac, "ac_automaton")) {
    cli::cli_abort("{.arg ac} must be an <ac_automaton> object.")
  }
  if (is.null(ac$ptr)) {
    cli::cli_abort("{.arg ac} must contain a non-null Rust external pointer.")
  }
  valid <- tryCatch(
    rust_ac_is_valid(ac$ptr),
    error = function(cnd) FALSE
  )
  if (!isTRUE(valid)) {
    cli::cli_abort(c(
      "{.arg ac} contains an invalid Rust external pointer.",
      "i" = "Rebuild it with {.fn ac_build}. External pointers cannot be restored across R sessions."
    ))
  }
  ac
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
  path <- normalizePath(enc2utf8(path), mustWork = FALSE)
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

validate_file_overlapping <- function(ac_automaton, overlapping, stream = FALSE) {
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
