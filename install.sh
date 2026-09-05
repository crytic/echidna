#!/usr/bin/env bash
set -e

version="2.2.0"
slither_version="0.9.3"

ECHIDNA_MAC_DIGEST="33f4aa8d5e0693711520d1470cd2511d0443a4cf6602d6000e4de00f6208340b"
ECHIDNA_UBUNTU_DIGEST="4301d2e83343ce448a53e8dcc233cdbf29450d2bb6ce52f083ea7bd479989883"

# Do nothing if echidna is already installed
if echidna-test --version > /dev/null 2>&1
then
  echo "WARNING: $(echidna-test --version) is installed but is out-of-date, we recommend removing it"
fi

# Do nothing if echidna is already installed
if echidna --version > /dev/null 2>&1
then
  echo "$(echidna --version) is already installed" && exit
  # TODO: check that the version is up to date, otherwise offer to upgrade
fi

# In the unlikely scenario that python3 isn't available, let the user install it manually
if ! python3 --version > /dev/null 2>&1
then echo "python3 is not available, please install it manually before attempting to install echidna" && exit 1
fi

# install pip3 if not already installed
if ! pip3 --version > /dev/null 2>&1
then
  echo "pip3 is not available, installing it now.."
  curl https://bootstrap.pypa.io/get-pip.py | python3
fi

# install slither if not already installed
if ! slither --version > /dev/null 2>&1
then
  echo "slither is not available, installing it now.."
  pip3 install slither-analyzer --user
elif [[ "$(slither --version)" != "$slither_version" ]]
then
  echo "slither is out of date, upgrading it now.."
  pip3 install --upgrade slither-analyzer --user
fi

echo "Installing Echidna v$version.."

BASE_DIR=${XDG_CONFIG_HOME:-$HOME}
ECHIDNA_DIR=${ECHIDNA_DIR-"$BASE_DIR/.echidna"}
ECHIDNA_BIN_DIR="$ECHIDNA_DIR/bin"
ECHIDNA_ARCHIVE_DIR="$ECHIDNA_DIR/v$version"
ECHIDNA_ARCHIVE_PATH="$ECHIDNA_ARCHIVE_DIR/echidna.tar.gz"
TRUE_BIN_PATH="$ECHIDNA_ARCHIVE_DIR/echidna/echidna"
BIN_PATH="$ECHIDNA_BIN_DIR/echidna"

if [[ "$(uname)" == "Darwin" ]]
then
  arch="MacOS"
  digest="$ECHIDNA_MAC_DIGEST"
elif [[ "$(uname)" == "Linux" ]]
then
  arch="Ubuntu"
  digest="$ECHIDNA_UBUNTU_DIGEST"
else echo "Unsupported system: $(uname)" && exit 1
fi

TARBALL_URL="https://github.com/crytic/echidna/releases/download/v$version/echidna-$version-$arch.tar.gz"

# Create the .echidna bin directory and echidna binary if it doesn't exist.
mkdir -p "$ECHIDNA_ARCHIVE_DIR"
pushd "$ECHIDNA_ARCHIVE_DIR" > /dev/null || exit 1

# Download the tarball if it's not present or if the digest is wrong
if [[ -f "$ECHIDNA_ARCHIVE_PATH" ]]
then
  if [[ "$digest" != "$(sha256sum "$ECHIDNA_ARCHIVE_PATH" | cut -d " " -f 1)" ]] 
  then
    echo "Wrong digest, deleting and re-downloading.."
    rm -f "$ECHIDNA_ARCHIVE_PATH"
    echo "Downloading the echidna archive"
    curl -# -L "$TARBALL_URL" -o "$ECHIDNA_ARCHIVE_PATH"
  fi
  if [[ "$digest" != "$(sha256sum "$ECHIDNA_ARCHIVE_PATH" | cut -d " " -f 1)" ]] 
  then echo "Wrong digest again, something's seriously wrong, aborting.." && exit 1
  else echo "Echidna v$version archive already exists and has the expected sha256 digest, moving on.."
  fi
else
  echo "Downloading the echidna archive"
  curl -# -L "$TARBALL_URL" -o "$ECHIDNA_ARCHIVE_PATH"
  if [[ "$digest" != "$(sha256sum "$ECHIDNA_ARCHIVE_PATH" | cut -d " " -f 1)" ]] 
  then echo "Wrong digest, something's seriously wrong, aborting.." && exit 1
  fi
fi

if [[ ! -d "echidna" ]]
then tar xzf "$ECHIDNA_ARCHIVE_PATH"
fi

if [[ ! -f "$TRUE_BIN_PATH" || ! -x "$TRUE_BIN_PATH" ]]
then echo "Failed to extract an executable echidna binary" && exit 1
fi

popd > /dev/null || exit 1
pushd "$ECHIDNA_DIR" > /dev/null || exit 1
mkdir -p "$ECHIDNA_BIN_DIR"
# If a link alreaady exists here, remove it
if [[ -f "$BIN_PATH" ]]
then rm -f "$BIN_PATH"
fi
# Add a symlink to the current version of echidna
ln -s "$TRUE_BIN_PATH" "$BIN_PATH"
popd > /dev/null || exit 1

# Store the correct profile file (i.e. .profile for bash or .zshenv for ZSH).
case $SHELL in
*/zsh)
    PROFILE=${ZDOTDIR-"$HOME"}/.zshenv
    PREF_SHELL=zsh
    ;;
*/bash)
    PROFILE=$HOME/.bashrc
    PREF_SHELL=bash
    ;;
*/fish)
    PROFILE=$HOME/.config/fish/config.fish
    PREF_SHELL=fish
    ;;
*/ash)
    PROFILE=$HOME/.profile
    PREF_SHELL=ash
    ;;
*)
    echo "echidna: could not detect shell, manually add ${ECHIDNA_BIN_DIR} to your PATH."
    exit 1
esac

# Only add echidna if it isn't already in PATH.
if [[ ":$PATH:" != *":${ECHIDNA_BIN_DIR}:"* ]]; then
    # Add the echidna directory to the path and ensure the old PATH variables remain.
    (echo >> "$PROFILE" && echo "export PATH=\"\$PATH:$ECHIDNA_BIN_DIR\"" >> "$PROFILE") || (echo "Failed to add echidna to your PATH, is $PROFILE read only?" && exit 1)
fi

echo && echo "Detected your preferred shell is ${PREF_SHELL} and added echidna to PATH. Run 'source ${PROFILE}' or start a new terminal session to use echidna."
echo "Then, simply run 'echidna --help' to learn how to use this fuzz tester."
