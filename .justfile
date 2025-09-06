alias b := build
alias r := run
alias t := test
alias fmt := format

default:
    just --list

# Build all projects
build:
    cabal build all

# Build specific year
build-year year:
    cabal build {{year}}

# Lint specific year
lint-year year:
    hlint {{year}}

# Run a specific day and part (pad day with zero if needed)
run year day part input="input.txt": 
    #!/usr/bin/env bash
    padded_day=$(printf "%02d" {{day}})
    cabal run {{year}} -- --day {{day}} --part {{part}} --input-file {{year}}/data/day${padded_day}/{{input}}

# Run with test input
test year day part:
    just run {{year}} {{day}} {{part}} test_input.txt

# Download files for specific day and year
download year day:
    #!/usr/bin/env bash
    padded_day=$(printf "%02d" {{day}})
    aoc d -d {{day}} -Ioi {{year}}/data/day${padded_day}/input.txt

# Clean build artifacts
clean:
    cabal clean

# Install dependencies
deps:
    cabal build --dependencies-only all

# Format all Haskell files (if fourmolu is installed)
format:
    #!/usr/bin/env bash
    if command -v fourmolu &> /dev/null; then
        fourmolu -i .
    else
        echo "fourmolu not installed, skipping format"
    fi

