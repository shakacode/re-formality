#!/bin/bash

set -euo pipefail

exec > >(tee /var/log/user-data.log|logger -t user-data -s 2>/dev/console) 2>&1

export DEBIAN_FRONTEND=noninteractive

apt update
apt install opam -y

sudo -u ubuntu bash -xec 'pwd;
OCAML_VERSION=4.12.0;
opam init --no;
opam switch create $OCAML_VERSION;
eval $(opam env --switch=$OCAML_VERSION);
opam install -y dune ppxlib reason alcotest;
cd ~;
git clone https://github.com/shakacode/re-formality.git;
cd re-formality;
dune build;
ARCH=$(uname -m);
PLATFORM=$(uname -s | tr "[:upper:]" "[:lower:]");
OPAM_FILE=$(basename -- $(find *.opam));
LIB="${OPAM_FILE%.*}";
SOURCE_BIN="_build/default/ppx/bin/bin.exe";
RELEASE_BIN="_release/$LIB-$PLATFORM-$ARCH.exe";
mkdir -p $(dirname $RELEASE_BIN);
cp $SOURCE_BIN $RELEASE_BIN;
'
