#!/usr/bin/env bash

runhaskell Setup.hs configure > /dev/null

FILES=""
# for each arg that doesn't start with '-'
# add to FILES
for arg in "$@"; do
  if [[ "$arg" != -* ]]; then
    # if $arg is a number
    if [[ "$arg" =~ ^[0-9]+$ ]]; then
      # add every file in the directory to FILES
      for file in `find "app/$arg" -type f`; do
        FILES="$FILES $file"
      done
    else
        FILES="$FILES $arg"
    fi
  fi
done

# FLAGS handling
# -w sets WATCH
# -v sets VERBOSE
while getopts ":wv" opt; do
  case $opt in
    w)
      WATCH=true
      ;;
    v)
      VERBOSE=true
      ;;
   esac
done

# function to run doctest on file
function run_doctest() {
    local file="$1"
    echo $file
    # cat if verbose
    if [[ "${VERBOSE}" == "true" ]]; then
        cat "${file}" | grep "^--"
    fi
    doctest $file
}

# function to run doctest with entr
function run_doctest_entr() {
    local file="$1"
    echo $file
    # cat if verbose
    if [[ "${VERBOSE}" == "true" ]]; then
        cat "${file}" | grep "^--"
    fi
    # run doctest with entr
    echo $file | entr -r -c doctest $file
}

# if FILES is empty
# set to all .hs files in app/
if [[ -z "${FILES}" ]]; then
    FILES="$(find app -name '*.hs' -print)"
fi

for f in $FILES; do
    # check if WATCH is set
    if [[ "${WATCH}" == "true" ]]; then
        run_doctest_entr $f
    else
        run_doctest $f
    fi
done
