set -e
set -o pipefail

ARCH=$(uname -m)
PLATFORM=$(uname -s | tr '[:upper:]' '[:lower:]')

OPAM_FILE=$(basename -- $(find *.opam))
LIB="${OPAM_FILE%.*}"

SOURCE_BIN="_build/default/ppx/bin/bin.exe"
RELEASE_DIR="_release"
RELEASE_ZIP="$RELEASE_DIR/release.zip"
RELEASE_BIN_DIR="$RELEASE_DIR/bin"
RELEASE_BIN="$RELEASE_BIN_DIR/$LIB-$PLATFORM-$ARCH.exe"

if [ ! -f "$RELEASE_ZIP" ]; then
  echo "$RELEASE_ZIP does not exist. Download it from Github and put in $RELEASE_DIR/ dir."
  exit 1
fi

echo "=== Releasing $LIB"
echo "=== Unzipping release archive"
unzip -d $RELEASE_DIR $RELEASE_ZIP
rm $RELEASE_ZIP
tree -a -L 2 $RELEASE_DIR

echo ""
echo "=== Preparing $PLATFORM $ARCH binary"

CHMOD=$(stat -c %a "$RELEASE_BIN_DIR/$(ls $RELEASE_BIN_DIR | head -n 1)")

dune build
cp $SOURCE_BIN $RELEASE_BIN
chmod $CHMOD $RELEASE_BIN
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
