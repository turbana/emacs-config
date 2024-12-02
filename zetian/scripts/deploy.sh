#!/bin/env bash

DEST=/mnt/c/Sync/zetian

set -e

cd $(dirname $0)/..
cp -ar build/* $DEST/
mkdir -p $DEST/scripts
cp -a scripts/* $DEST/scripts/
