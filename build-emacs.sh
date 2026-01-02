#!/bin/env bash

set -e

CONFIGURE_OPTS="\
--with-sound=no \
--with-x-toolkit=gtk3 \
--with-sqlite3 \
--with-tree-sitter \
--with-native-compilation=aot \
"

if [[ "$#" -ne "1" ]]; then
    echo "USAGE: $(basename $0) emacs-tag"
    exit 2
fi

TAG=$1
REMOTE=https://ftpmirror.gnu.org/emacs/${TAG}.tar.gz
TMP=~/tmp
ROOT=$TMP/$TAG

if [[ ! -d $ROOT ]]; then
   echo "* downloading $REMOTE into $ROOT"
   cd $TMP
   curl -L $REMOTE | tar -xz
fi

cd $ROOT
./autogen.sh
./configure $CONFIGURE_OPTS
make -j $(nproc --all)
sudo make install
