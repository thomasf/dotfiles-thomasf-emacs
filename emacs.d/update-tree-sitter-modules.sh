#!/bin/sh

set -e

git clone git@github.com:casouri/tree-sitter-module || true

cd tree-sitter-module

git pull

JOBS=12 ./batch.sh

mkdir -p ../tree-sitter
rm  ../tree-sitter/libtree-sitter-*.so || true
cp dist/libtree-sitter-* ../tree-sitter/
