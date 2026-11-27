# ahocorasick 0.3.0

## Breaking change

- Add `...` dots in functions arguments to force optional parameters to be
  passed explicitly.

## Bug fixes

- Strings that are not valid UTF-8 now give an informative error instead of
  being passed to Rust, where they could crash the R session.
- `ac_extract_file(stream = FALSE)` now searches files that are not UTF-8,
  like the other `ac_*_file()` functions. `ac_locate_file()` still needs UTF-8
  files to compute character positions, and its error now says so.
- `ac_replace_file()` now checks its default output paths the same way as
  explicit ones. Before, with inputs such as `c("a.txt", "a_replaced.txt")`,
  the output of the first file silently overwrote the second input.
- `ac_count()` and `ac_count_file()` now give an error when a document or file
  has more matches than the largest R integer, instead of returning a wrong
  count.
- `print()` and `length()` no longer fail for automatons restored with
  `readRDS()` or from a saved workspace.

## Performance improvements

- Documents are no longer copied when they are passed to Rust, which roughly
  halves peak memory use when searching long texts.
- `ac_locate()`, `ac_extract()`, `ac_locate_file()` and `ac_extract_file()`
  build their per-document results about twice as fast.
- `ac_extract()`, `ac_extract_df()` and `ac_extract_file()` no longer create a
  new string for every match when the automaton is case-sensitive.
- `ac_locate()`, `ac_locate_df()` and `ac_locate_file()` compute character
  positions much faster.
- Functions that return matches no longer collect all matches of a document
  before processing them.
- `rust_ac_extract` now stores `&str` slices of the document directly,
  constructing the final R character vector in a single step, rather than
  allocating a new `String` for every match.

# ahocorasick 0.2.0

- Release on [R-multiverse](https://community.r-multiverse.org/ahocorasick).
- Add file search APIs `ac_*_file()` for compiled automatons. See documentation
  for details.

# ahocorasick 0.1.0

Initial CRAN submission.

Implemented these APIs:

- Build Automatons:
  - `ac_build`
  - `ac_patterns`
  - `ac_info`
- Search in Documents:
  - `ac_detect`
  - `ac_count`
  - `ac_extract`
  - `ac_extract_df`
  - `ac_locate`
  - `ac_locate_df`
  - `ac_locate_bytes`
  - `ac_replace`

Add necessary tests, documents, a benchmark, and the website.
