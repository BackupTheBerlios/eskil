
cd eskil.vfs/lib

ln -s /home/peter/tclkit/griffin.vfs/lib/griffin
mkdir psballoon
cd psballoon
ln -s /home/peter/src/psballoon/psballoon.tcl
ln -s /home/peter/src/psballoon/pkgIndex.tcl
cd ..

mkdir pstools
cd pstools
ln -s /home/peter/src/pstools/pstools.tcl
ln -s /home/peter/src/pstools/pkgIndex.tcl
cd ..

ln -s /home/peter/src/textsearch

cd app-eskil

ln -s ../../../doc
ln -s ../../../eskil.tcl
ln -s ../../../examples
ln -s ../../../Nuisance.gif
ln -s ../../../COPYING
