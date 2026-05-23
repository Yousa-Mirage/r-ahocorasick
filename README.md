# ahocorasick

Fast multi-pattern string matching for R via the Aho-Corasick algorithm, powered
by the Rust [`aho-corasick`](https://docs.rs/aho-corasick/) crate.

`ahocorasick` lets you search for many substrings, called patterns, in one or
more strings, called documents. It builds a reusable automaton once and then
uses it for detection, counting, locating, and extraction.

Inspired by the Python package [`ahocorasick_rs`](https://github.com/G-Research/ahocorasick_rs).

## Installation

Install from source. You need Cargo and `rustc` on `PATH`.

```r
install.packages("remotes")
remotes::install_git("https://github.com/Yousa-Mirage/ggtypst")
```

## Quick Start

```r
library(ahocorasick)

patterns <- c("hello", "world", "fish")
doc <- c(
  "this is my first hello world. hello!",
  "nothing to see",
  "fish and chips"
)

ac <- ac_build(patterns)

ac_detect(ac, doc)
#> [1]  TRUE FALSE  TRUE

ac_count(ac, doc)
#> [1] 3 0 1

ac_extract_df(ac, doc)
#>   doc_id matches patterns
#> 1      1   hello    hello
#> 2      1   world    world
#> 3      1   hello    hello
#> 4      3    fish     fish
```

## Locating Matches

`ac_locate()` returns one data frame per document. Offsets are 1-based character
positions, inclusive on both sides, so they can be used with `substr()`.

```r
hits <- ac_locate(ac, doc)
hits[[1]]
#>   pattern_id start end
#> 1          1    18  22
#> 2          2    24  28
#> 3          1    31  35
```

Use `ac_locate_df()` when you want one data frame for all documents.

```r
ac_locate_df(ac, doc)
#>   doc_id pattern_id start end
#> 1      1          1    18  22
#> 2      1          2    24  28
#> 3      1          1    31  35
#> 4      3          3     1   4
```

Use `ac_locate_bytes()` when byte offsets are more useful than R character
positions. Byte offsets are 0-based and `byte_end` is end-exclusive.

## Matching Semantics

`ac_build()` exposes three match kinds from the Rust library.

The default, `match_kind = "standard"`, returns the match that finishes first.
It is also the only mode that supports overlapping search.

```r
patterns <- c("content", "disco", "disc", "discontent", "winter")
haystack <- "This is the winter of my discontent"

ac <- ac_build(patterns)
ac_extract(ac, haystack)[[1]]
#>   matches patterns
#> 1  winter   winter
#> 2    disc     disc

ac_extract(ac, haystack, overlapping = TRUE)[[1]]
#>      matches   patterns
#> 1     winter     winter
#> 2       disc       disc
#> 3      disco      disco
#> 4 discontent discontent
#> 5    content    content
```

`match_kind = "leftmost_first"` returns the leftmost non-overlapping match. If
several patterns start at the same position, the earlier pattern wins.

```r
ac <- ac_build(
  c("disco", "disc"),
  match_kind = "leftmost_first"
)
ac_extract(ac, "discontent")[[1]]
#>   matches patterns
#> 1   disco    disco
```

`match_kind = "leftmost_longest"` returns the leftmost non-overlapping match. If
several patterns start at the same position, the longest pattern wins.

```r
ac <- ac_build(
  c("disco", "disc", "discontent"),
  match_kind = "leftmost_longest"
)
ac_extract(ac, "discontent")[[1]]
#>      matches   patterns
#> 1 discontent discontent
```

## Performance Options

`implementation` controls the underlying automaton implementation:

- `"auto"` lets the Rust crate choose a heuristic default.
- `"noncontiguous_nfa"` is fast to build and uses moderate memory.
- `"contiguous_nfa"` uses memory efficiently and is usually faster to search
  than a noncontiguous NFA.
- `"dfa"` can be fastest to search, but may be slow to build and can use much
  more memory.

```r
ac <- ac_build(
  c("disco", "disc"),
  implementation = "dfa"
)

ac_info(ac)
```

## Missing Values

Search functions let you choose how `NA` documents behave.

```r
doc <- c("hello", NA_character_, "world")

ac_detect(ac_build(c("hello", "world")), doc, na = "false")
#> [1]  TRUE FALSE  TRUE

ac_count(ac_build(c("hello", "world")), doc, na = "zero")
#> [1] 1 0 1
```

For list-column workflows, `ac_locate(..., na = "empty")` and
`ac_extract(..., na = "empty")` treat missing documents as no matches.
