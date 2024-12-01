list:
    just --list

run:
    lake build
    echo "" # -- Build finished --
    ./.lake/build/bin/advent_of_lean
