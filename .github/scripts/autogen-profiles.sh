#!/bin/bash

# Generates a profile directory with profiles and outputs
#  from running each .sol file in a given examples directory

# ARGUMENTS:
#  $1 - path to echidna-test to run, defaults to "echidna-test"
#  $2 - examples folder to profile, defaults to "examples"

echidna=$1
if [ -z "$1" ]; then
  echidna="echidna-test"
else
  echidna="$1"
fi

examples=$2
if [ -z "$2" ]; then
  examples="examples"
else
  examples="$2"
fi

# prepare profiles folder and subfolders
for dir in $(find "$examples" -type d); do
  mkdir -p "profiles/$dir"
done

# profile each of the files
totalFiles=$(find "$examples" -type f -name "*.sol" -printf '.' | wc -c)
declare -i doneFiles
doneFiles=0
for file in $(find "$examples" -type f -name "*.sol"); do
  doneFiles+=1
  echo "$doneFiles/$totalFiles - $file"
  # timeout 60 is because some of the files, eg examples/solidity/basic/propGasLimit.sol, can run infinitely
  timeout 60 "$echidna" "$file" --format none +RTS -p  2> "profiles/$file.otp"
  mv echidna-test.prof "profiles/$file.prof"
done
