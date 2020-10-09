#!/bin/sh

set -e

cabal sdist
cabal upload dist-newstyle/sdist/churros-0.1.0.0.tar.gz

dir=$(mktemp -d dist-docs.XXXXXX)
trap 'rm -r "$dir"' EXIT

cabal haddock --builddir="$dir" --haddock-for-hackage --enable-doc
cabal upload -d $dir/*-docs.tar.gz
