#!/bin/bash

set -euo pipefail

echo ""
echo "=== Preparing $PLATFORM $ARCH binary"

ARCH=$(uname -m)
PLATFORM=$(uname -s | tr '[:upper:]' '[:lower:]')

SOURCE_BIN="_build/default/ppx/bin/bin.exe"
RELEASE_BIN="$RELEASE_BIN_DIR/$LIB-$PLATFORM-$ARCH.exe"

dune build
cp $SOURCE_BIN $RELEASE_BIN
chmod $CHMOD $RELEASE_BIN
