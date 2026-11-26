export LC_CTYPE := "en_US.UTF-8"

alias fmt := format
alias lint := check
alias doc := build

default:
    @just --list

format:
    @air format .
    @cargo fmt --manifest-path src/rust/Cargo.toml
    @echo "✅ All files formatted!"

check:
    @jarl check .
    @Rscript -e "devtools::spell_check()"
    @cargo clippy --manifest-path src/rust/Cargo.toml -- -D warnings
    @echo "✅ All checks passed!"

clean:
    cargo clean --manifest-path src/rust/Cargo.toml
    @echo "✅ Cleaned up!"

build:
    @cd src && cargo run --quiet --manifest-path rust/Cargo.toml --bin document
    @Rscript -e "devtools::document()"
    @echo "✅ Package built successfully!"

test:
    @Rscript -e "devtools::test(reporter = 'summary')"
    @cargo test --quiet --manifest-path src/rust/Cargo.toml
    @echo "🎉 All tests passed!"

site:
    Rscript -e "devtools::build_readme()"
    Rscript -e "rmarkdown::render('.github/CONTRIBUTING.Rmd')"
    Rscript -e "pkgdown::build_site()"
    @echo "✅ Site built successfully!"
    @open docs/index.html || true

pkg-build:
    Rscript -e "devtools::build(path = '.')"
    @echo "✅ Package built successfully!"

pkg-check:
    Rscript -e 'devtools::check(document = FALSE, remote = TRUE, error_on = "warning")'

update-wordlist:
    Rscript -e "spelling::update_wordlist(confirm = FALSE)"
