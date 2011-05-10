#!/bin/sh
# Build up a merge conflict in git
mkdir apa_git
cd apa_git
git init
cp ../tests/ancestor.txt a.txt
git add a.txt
git commit -m a
git checkout -b b
cp ../tests/right.txt a.txt
git commit -am r
git checkout master
cp ../tests/left.txt  a.txt
git commit -am l
git checkout b
#git merge master
#git mergetool
#git commit -am m
