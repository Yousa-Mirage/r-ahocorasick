# Block modifying ac_automaton objects with `$<-`, `[<-` and `[[<-`.
#
# An automaton's fields must match its Rust automaton, so they can't be
# changed one at a time: changing `patterns` would not change what is searched.

#' @export
`$<-.ac_automaton` <- function(x, name, value) {
  abort_modify_ac_automaton()
}

#' @export
`[<-.ac_automaton` <- function(x, i, ..., value) {
  abort_modify_ac_automaton()
}

#' @export
`[[<-.ac_automaton` <- function(x, i, ..., value) {
  abort_modify_ac_automaton()
}

abort_modify_ac_automaton <- function(call = rlang::caller_env()) {
  cli::cli_abort(
    c(
      "{.cls ac_automaton} objects can't be modified.",
      "i" = "Build a new automaton with {.fn ac_build} instead."
    ),
    call = call
  )
}
