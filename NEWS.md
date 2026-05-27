# ahocorasick (development version)

- Release on [R-multiverse](https://community.r-multiverse.org/ahocorasick).
- Add file search APIs `ac_*_file()` for compiled automatons. See documentation for details.

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
