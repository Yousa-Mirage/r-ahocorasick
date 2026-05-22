alias fmt := format
# alias doc := document

default:
    @just --list

format:
    r-air format .

check:
    jarl check .