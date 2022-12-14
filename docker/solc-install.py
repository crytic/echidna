#!/usr/bin/python3
import os
import shutil
import subprocess
import sys

## solc-install: simple wrapper script to invoke solc-select install when required
##
## This script will observe the SOLC_VERSION variable. If it is set, it will install
## and globally select said solc version.

if len(sys.argv) < 2:
    print(f"Usage: {sys.argv[0]} other-program [args..]")
    sys.exit(1)

solc_version = os.getenv('SOLC_VERSION', None)
if solc_version:
    silent = os.getenv("SOLC_SELECT_SILENT", "1") == "1" and subprocess.DEVNULL or None
    subprocess.run(['solc-select', 'install', solc_version], stderr=silent, stdout=silent)
    subprocess.run(['solc-select', 'use', solc_version], stderr=silent, stdout=silent)

os.execv(shutil.which(sys.argv[1]), sys.argv[1:])