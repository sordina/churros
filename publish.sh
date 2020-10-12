#!/bin/sh

set -e

dir=$(mktemp -d docs/dist-docs.XXXXXX)
# trap 'rm -r "$dir"' EXIT

VERSION=$(grep '^version:' churros.cabal | sed 's/[^ ]* *//')
echo "Library VERSION: $VERSION"

echo "Checklist:"
echo "  [ ] - Update version"
echo "  [ ] - Update changelog"
echo "  [ ] - Run build"
echo "  [ ] - Run tests"
echo "  [ ] - Test docs locally"
echo ""
echo "proceed? "
read -r

cabal sdist
cabal haddock --builddir="$dir" --haddock-for-hackage --enable-doc

find "$dir" | grep index.html | head -n 1 | xargs open

if [ "$1" = "--publish" ]
then
    cabal upload --publish dist-newstyle/sdist/churros-${VERSION}.tar.gz
    cabal upload -d --publish $dir/*-docs.tar.gz
else
    echo "Candidate mode. Use --publish argument to publish."
    cabal upload dist-newstyle/sdist/churros-${VERSION}.tar.gz
    cabal upload -d $dir/*-docs.tar.gz
    echo "Candidate mode. Use --publish argument to publish."
fi
