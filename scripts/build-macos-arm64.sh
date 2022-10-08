#!/bin/bash

set -euo pipefail

ARCH=$(uname -m)
PLATFORM=$(uname -s | tr '[:upper:]' '[:lower:]')

echo ""
echo "=== Preparing $PLATFORM $ARCH binary"

SOURCE_BIN="_build/default/ppx/bin/bin.exe"
RELEASE_BIN="$RELEASE_BIN_DIR/$LIB-$PLATFORM-$ARCH.exe"

dune build
cp $SOURCE_BIN $RELEASE_BIN
chmod $CHMOD $RELEASE_BIN
