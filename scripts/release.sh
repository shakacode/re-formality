#!/bin/bash

set -euo pipefail

OPAM_FILE=$(basename -- "$(find *.opam)")

export LIB="${OPAM_FILE%.*}"
export RELEASE_DIR="_release"
export RELEASE_ZIP="$RELEASE_DIR/release.zip"
export RELEASE_BIN_DIR="$RELEASE_DIR/bin"

echo "=== Releasing $LIB"

if [ ! -f "$RELEASE_ZIP" ]; then
  echo "$RELEASE_ZIP does not exist. Download it from Github and put in $RELEASE_DIR/ dir."
  exit 1
fi

echo "=== Unzipping release archive"
unzip -d $RELEASE_DIR $RELEASE_ZIP
rm $RELEASE_ZIP

echo "Release tree:"
tree -a -L 2 $RELEASE_DIR

export CHMOD=$(stat -c %a "$RELEASE_BIN_DIR/$(ls $RELEASE_BIN_DIR | head -n 1)")

./scripts/build-macos-arm64.sh
./scripts/build-linux-arm64.sh

echo "Release tree:"
tree -a -L 2 $RELEASE_DIR

echo ""
echo "=== Publishing to npm"
cd $RELEASE_DIR
rm .DS_Store >/dev/null 2>&1 || true

echo "package.json:"
cat package.json
echo ""

npm publish
cd ..

echo ""
echo "=== Cleaning up"
rm -rf $RELEASE_DIR/*
tree -a -L 2 $RELEASE_DIR
