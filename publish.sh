#!/bin/sh

set -e

cabal sdist
# cabal upload dist-newstyle/sdist/churros-0.1.0.0.tar.gz

dir=$(mktemp -d docs/dist-docs.XXXXXX)
# trap 'rm -r "$dir"' EXIT

cabal haddock --builddir="$dir" --haddock-for-hackage --enable-doc

find "$dir" | grep index.html | head -n 1 | xargs open

# cabal upload -d $dir/*-docs.tar.gz
