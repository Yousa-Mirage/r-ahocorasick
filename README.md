
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ahocorasick

<div align="center">

[![GitHub
stars](https://img.shields.io/github/stars/Yousa-Mirage/r-ahocorasick?style=social)](https://github.com/Yousa-Mirage/r-ahocorasick/stargazers)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Ask
DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/Yousa-Mirage/r-ahocorasick)

</div>

**Fast multi-pattern string matching in R with the Aho-Corasick
algorithm.**

`ahocorasick`, powered by the Rust
[`aho-corasick`](https://docs.rs/aho-corasick/) crate, allows you to
search for many substrings in one or more strings with linear complexity
in R. It builds a reusable automaton once and then uses it for
detection, counting, locating, and extraction.

[Aho-Corasick](https://en.wikipedia.org/wiki/Aho–Corasick_algorithm) is
a multiple-pattern string matching algorithm designed to search for many
keywords in a text in a single pass. After preprocessing the pattern set
into an automaton, it finds all matches in
`O(text_length + number_of_matches)`, instead of the naive
`O(text_length × number_of_patterns)` approach. This makes it especially
suitable for high-throughput tasks such as dictionary matching, keyword
extraction, rule-based text filtering, and large-scale log or document
analysis.

## Installation

Install from source. You need Cargo and `rustc` on `PATH`.

``` r
# install.packages("pak")
pak::pak("Yousa-Mirage/r-ahocorasick")
```

## Quick Start

Start with a character vector of patterns you want to search for, then
compile it into an `<ac_automaton>` object by calling `ac_build()` that
can be reused across many documents.

``` r
library(ahocorasick)

patterns <- c("hello", "world", "fish")
doc <- c(
  "this is my first hello world. hello!",
  "nothing to see",
  "fish and chips"
)

ac <- ac_build(patterns)
```

You can set `ascii_case_insensitive = TRUE` to make the search
case-insensitive for ASCII characters.

### Extracting Matches

`ac_extract()` returns one data frame per document, with both the
matched text and the pattern value that produced each match.

``` r
patterns <- c("hello", "world", "fish")
doc <- c(
  "this is my first hello world. hello!",
  "nothing to see",
  "fish and chips"
)

ac <- ac_build(patterns)

hits <- ac_extract(ac, doc)
hits[[1]]
#>   matches patterns
#> 1   hello    hello
#> 2   world    world
#> 3   hello    hello
```

Use `ac_extract_df()` when you want one data frame for all documents.

``` r
ac_extract_df(ac, doc)
#>   doc_id matches patterns
#> 1      1   hello    hello
#> 2      1   world    world
#> 3      1   hello    hello
#> 4      3    fish     fish
```

Set `overlapping = TRUE` to return overlapping matches. This is only
supported when the automaton was built with the default
`match_kind = "standard"`.

``` r
ac_overlap <- ac_build(c("aba", "bab"))
ac_extract_df(ac_overlap, "ababa", overlapping = TRUE)
#>   doc_id matches patterns
#> 1      1     aba      aba
#> 2      1     bab      bab
#> 3      1     aba      aba
```

### Locating Matches

`ac_locate()` returns one data frame per document. Offsets are 1-based
character positions, inclusive on both sides, so they can be used with
`substr()`.

``` r
patterns <- c("hello", "world", "fish")
doc <- c(
  "this is my first hello world. hello!",
  "nothing to see",
  "fish and chips"
)

ac <- ac_build(patterns)

hits <- ac_locate(ac, doc)
hits[[1]]
#>   pattern_id start end
#> 1          1    18  22
#> 2          2    24  28
#> 3          1    31  35
```

Use `ac_locate_df()` when you want one data frame for all documents.

``` r
ac_locate_df(ac, doc)
#>   doc_id pattern_id start end
#> 1      1          1    18  22
#> 2      1          2    24  28
#> 3      1          1    31  35
#> 4      3          3     1   4
```

Use `ac_locate_bytes()` when byte offsets are more useful than R
character positions. Byte offsets are 0-based and `byte_end` is
end-exclusive.

### Detecting And Counting

Use `ac_detect()` when you only need to know whether a document contains
at least one match, and `ac_count()` when you need the number of
matches.

``` r
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
```

These functions are usually the cheapest way to answer yes/no or
frequency questions without materializing the matched strings or
offsets.

### Using With Tidyverse

These search functions work naturally inside `dplyr::mutate()`. Scalar
outputs like `ac_detect()` and `ac_count()` become ordinary columns,
while `ac_extract()` and `ac_locate()` can be stored as list-columns.

``` r
patterns <- c("hello", "world", "fish")
ac <- ac_build(patterns)

docs <- tibble::tibble(
  doc = c(
    "this is my first hello world. hello!",
    "nothing to see",
    "fish and chips"
  )
)

docs |> 
  dplyr::mutate(
    detected = ac_detect(ac, doc),
    n_matches = ac_count(ac, doc),
    extracted = ac_extract(ac, doc)
  ) |> 
  tidyr::unnest(extracted)
#> # A tibble: 4 × 5
#>   doc                                  detected n_matches matches patterns
#>   <chr>                                <lgl>        <int> <chr>   <chr>   
#> 1 this is my first hello world. hello! TRUE             3 hello   hello   
#> 2 this is my first hello world. hello! TRUE             3 world   world   
#> 3 this is my first hello world. hello! TRUE             3 hello   hello   
#> 4 fish and chips                       TRUE             1 fish    fish
```

## Missing Values

Search functions let you choose how `NA` documents behave.

``` r
doc_na <- c("hello", NA_character_, "world")
ac_na <- ac_build(c("hello", "world"))

ac_detect(ac_na, doc_na, na = "false")
#> [1]  TRUE FALSE  TRUE
ac_count(ac_na, doc_na, na = "zero")
#> [1] 1 0 1
```

For list-column workflows, `ac_locate(..., na = "empty")` and
`ac_extract(..., na = "empty")` treat missing documents as no matches.

## Matching Semantics

`ac_build()` exposes three match kinds from the Rust library.

### `standard` (the default)

The default, `match_kind = "standard"`, returns the match that finishes
first. It is also the only mode that supports overlapping search.

``` r
patterns <- c("content", "disco", "disc", "discontent", "winter")
haystack <- "This is the winter of my discontent"

ac <- ac_build(patterns)
ac_extract_df(ac, haystack)
#>   doc_id matches patterns
#> 1      1  winter   winter
#> 2      1    disc     disc
ac_extract_df(ac, haystack, overlapping = TRUE)
#>   doc_id    matches   patterns
#> 1      1     winter     winter
#> 2      1       disc       disc
#> 3      1      disco      disco
#> 4      1 discontent discontent
#> 5      1    content    content
```

### `leftmost_first`

`match_kind = "leftmost_first"` returns the leftmost non-overlapping
match. If several patterns start at the same position, the earlier
pattern wins.

``` r
ac <- ac_build(
  c("disco", "disc"),
  match_kind = "leftmost_first"
)
ac_extract_df(ac, "discontent")
#>   doc_id matches patterns
#> 1      1   disco    disco
```

### `leftmost_longest`

`match_kind = "leftmost_longest"` returns the leftmost non-overlapping
match. If several patterns start at the same position, the longest
pattern wins.

``` r
ac <- ac_build(
  c("disco", "disc", "discontent"),
  match_kind = "leftmost_longest"
)
ac_extract_df(ac, "discontent")
#>   doc_id    matches   patterns
#> 1      1 discontent discontent
```

## Performance Options

`implementation` controls the underlying automaton implementation:

- `"auto"` lets the Rust crate choose a heuristic default.
- `"noncontiguous_nfa"` is fast to build and uses moderate memory.
- `"contiguous_nfa"` uses memory efficiently and is usually faster to
  search than a noncontiguous NFA.
- `"dfa"` can be fastest to search, but may be slow to build and can use
  much more memory.

``` r
ac <- ac_build(
  c("disco", "disc"),
  implementation = "dfa"
)

ac_info(ac)$implementation
#> [1] "dfa"
```

## Benchmark

See the [benchmark
article](https://yousa-mirage.github.io/r-ahocorasick/articles/benchmarks.html)
for results on English, Chinese, and mixed-text workloads. In the
current benchmark, `ahocorasick` is **fastest** for `detect` and `count`
across the tested cases. The `ac_extract_df()` is also the **fastest**
for `extract` while preserving a tidy long data-frame result.

As with any benchmark, real-world results will differ based on your
particular situation. If performance is important to your application,
measure the alternatives yourself!

## Related Work

Packages that developed to handle multiple string matching in R are less
than in Python. These are what I found:

- [`AhoCorasickTrie`](https://github.com/chambm/AhoCorasickTrie):
  Implemented in C++. It only supports 128 ASCII characters and
  currently does not support UTF-8/Unicode (such as CJK and emojis).
- [`Biostrings`](https://bioconductor.org/packages/release/bioc/html/Biostrings.html):
  It is a special tool for bioinformatics, and its object system is
  `XString` / `DNAString` / `XStringSet`. It is not suitable as a
  drop-in multi-mode retrieval tool for general R string vectors.
- [`r-polars`](https://pola-rs.github.io/r-polars): Polars’ string
  expressions also use the Aho-Corasick algorithm to match (based on
  [the same crate](https://docs.rs/aho-corasick/) as this package). **If
  your data is already in a DataFrame, I recommend you use Polars for
  matching strings directly.**

It’s great to see that Rust is providing more and more modern, safe,
high-performance open source tools for R (and also for Python) :)

## Acknowledgments

Inspired by the Python package
[`ahocorasick_rs`](https://github.com/G-Research/ahocorasick_rs). Thanks
for the excellent Rust [`aho-corasick`](https://docs.rs/aho-corasick/)
crate.

MIT © 2026 Yousa Mirage
