#!/bin/bash

set -euo pipefail

exec > >(tee /var/log/user-data.log|logger -t user-data -s 2>/dev/console) 2>&1

export DEBIAN_FRONTEND=noninteractive

apt update

sudo -u ubuntu bash -xec 'pwd;
cd ~;
curl -fsSL https://get.jetpack.io/devbox -o devbox.sh;
bash devbox.sh -f;
git clone https://github.com/shakacode/re-formality.git;
cd re-formality;
devbox shell;
devbox run build;
ARCH=arm64;
PLATFORM=linux;
OPAM_FILE=$(basename -- $(find *.opam));
LIB="${OPAM_FILE%.*}";
SOURCE_BIN="_build/default/ppx/bin/bin.exe";
RELEASE_BIN="_release/$LIB-$PLATFORM-$ARCH.exe";
mkdir -p $(dirname $RELEASE_BIN);
cp $SOURCE_BIN $RELEASE_BIN;
'
