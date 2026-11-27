Sys.setenv(NOT_CRAN = "true", DEBUG = "true")
devtools::load_all(quiet = TRUE)

# Render README.Rmd to README.md
if (file.exists("README.Rmd")) {
  rmarkdown::render(
    input = "README.Rmd",
    output_options = list(html_preview = FALSE),
    quiet = TRUE,
    envir = new.env(parent = globalenv())
  )
}

# Render .github/CONTRIBUTING.Rmd to .github/CONTRIBUTING.md
if (file.exists(".github/CONTRIBUTING.Rmd")) {
  rmarkdown::render(
    input = ".github/CONTRIBUTING.Rmd",
    output_options = list(html_preview = FALSE),
    quiet = TRUE,
    envir = new.env(parent = globalenv())
  )
}

# Build the pkgdown site
pkgdown::init_site()
pkgdown::build_site(
  preview = FALSE,
  devel = TRUE,
  lazy = TRUE,
  install = TRUE
)
