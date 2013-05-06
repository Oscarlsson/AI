#!/bin/bash

f1=$HOME/.cabal/share/gf-3.4/lib/ShrdliteEng.gf
f2=$HOME/.cabal/share/gf-3.4/lib/Shrdlite.gf
f3=$HOME/.cabal/share/gf-3.4/lib/ShrdliteLexEng.gf
f4=$HOME/.cabal/share/gf-3.4/lib/ShrdliteLex.gf
f5=$HOME/.cabal/share/gf-3.4/lib/ShrdliteSem.gf

for i in $f1 $f2 $f3 $f4 $f5; do
    if [ ! -f $i  ]; then
        echo "missing file $i"
        exit 1
    fi
done

DIR="$(pwd)/www"
if [ -d "$DIR" ]; then
    cd $DIR
    echo "starting server!"
    python -m CGIHTTPServer
else
    echo "directory $DIR does not exist"
fi
