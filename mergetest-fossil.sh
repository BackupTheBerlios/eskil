#!/bin/sh
# Build up a merge conflict in fossil
fossil init apa.fossil
mkdir apa_fossil
cd apa_fossil
fossil open ../apa.fossil
fossil settings gmerge-command 'eskil -fine -a "%baseline" "%merge" "%original" -o "%output"'
cp ../tests/ancestor.txt a.txt
fossil add a.txt
fossil commit -m a
fossil branch new b trunk
fossil update b
cp ../tests/right.txt a.txt
fossil commit -m r
fossil update trunk
cp ../tests/left.txt  a.txt
fossil commit -m l
fossil update b
#fossil merge trunk
#fossil commit -m "Merge from trunk"
