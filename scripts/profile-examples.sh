#!/bin/bash

# Generates a profile directory with profiles and outputs
#  from running each .sol file in a given examples directory

# ARGUMENTS:
#  $1 - path to echidna-test to run, defaults to "echidna-test"
#  $2 - examples folder to profile, defaults to "examples"
#  $3 - time limit for each profile, defaults to 60

# TO COMPILE to support profiling:
#   nix: set profilingOn to true in default.nix and run nix-build
#   cabal: add "library-profiling: True" and "executable-profiling: True" to echidna.cabal
#   stack: use the flags "--profile --executable-profiling --library-profiling" when building
#   ghc: use the flags "-prof -fprof-auto -rtsopts" when building

# TO RUN an executable with profiling enabled (this script does this automatically):
#   echidna-test [normal arguments] +RTS -p

if [ -z "$1" ]; then
  echidna="echidna-test"
else
  echidna="$1"
fi

if [ -z "$2" ]; then
  examples="examples"
else
  examples="$2"
fi

if [ -z "$3" ]; then
  timeLimit="60"
else
  timeLimit="$3"
fi

# prepare profiles folder and subfolders
for dir in $(cd "$examples"; find . -type d); do
  mkdir -p "profiles/$dir"
done

# profile each of the files
totalFiles=$(find "$examples" -type f -name "*.sol" -printf '.' | wc -c)
declare -i doneFiles
doneFiles=0
for file in $(cd "$examples"; find . -type f -name "*.sol"); do
  doneFiles+=1
  echo "$doneFiles/$totalFiles - $file"

  cfgFile=$(echo "$examples/$file" | sed "s/\.sol$/.yml/")
  if [ -f "$cfgFile" ]; then
    timeout "$timeLimit" "$echidna" "$examples/$file" --format none --config "$cfgFile" +RTS -p  2> "profiles/$file.otp"
  else
    timeout "$timeLimit" "$echidna" "$examples/$file" --format none +RTS -p  2> "profiles/$file.otp"
  fi

  mv echidna-test.prof "profiles/$file.prof"
done
