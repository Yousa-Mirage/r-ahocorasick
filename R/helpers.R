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
