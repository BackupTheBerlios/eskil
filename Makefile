# Path to the TclKits used for creating StarPacks.
TCLKIT = /home/peter/tclkit
TCLKIT_LINUX   = $(TCLKIT)/tclkit-linux-x86
TCLKIT_SOLARIS = $(TCLKIT)/tclkit-solaris-sparc
TCLKIT_WIN     = $(TCLKIT)/tclkit-win32.upx.exe

# Path to the libraries used
GRIFFIN   = /home/peter/tclkit/griffin.vfs/lib/griffin
PSBALLOON = /home/peter/src/psballoon
PSTOOLS   = /home/peter/src/pstools
TEXTSEARCH = /home/peter/src/textsearch

all: setup

#----------------------------------------------------------------
# Setup symbolic links from the VFS to the real files
#----------------------------------------------------------------

eskil.vfs/lib/app-eskil/eskil.tcl:
	cd eskil.vfs/lib/app-eskil ; ln -s ../../../eskil.tcl
eskil.vfs/lib/app-eskil/examples:
	cd eskil.vfs/lib/app-eskil ; ln -s ../../../examples
eskil.vfs/lib/app-eskil/doc:
	cd eskil.vfs/lib/app-eskil ; ln -s ../../../doc
eskil.vfs/lib/app-eskil/Nuisance.gif:
	cd eskil.vfs/lib/app-eskil ; ln -s ../../../Nuisance.gif
eskil.vfs/lib/app-eskil/COPYING:
	cd eskil.vfs/lib/app-eskil ; ln -s ../../../COPYING
eskil.vfs/lib/griffin:
	cd eskil.vfs/lib ; ln -s $(GRIFFIN) griffin
eskil.vfs/lib/textsearch:
	cd eskil.vfs/lib ; ln -s $(TEXTSEARCH) textsearch
eskil.vfs/lib/psballoon:
	mkdir eskil.vfs/lib/psballoon
	cd eskil.vfs/lib/psballoon ; ln -s $(PSBALLOON)/psballoon.tcl
	cd eskil.vfs/lib/psballoon ; ln -s $(PSBALLOON)/pkgIndex.tcl
eskil.vfs/lib/pstools:
	mkdir eskil.vfs/lib/pstools
	cd eskil.vfs/lib/pstools ; ln -s $(PSTOOLS)/pstools.tcl
	cd eskil.vfs/lib/pstools ; ln -s $(PSTOOLS)/pkgIndex.tcl

links: eskil.vfs/lib/app-eskil/eskil.tcl\
	eskil.vfs/lib/app-eskil/examples\
	eskil.vfs/lib/app-eskil/doc\
	eskil.vfs/lib/app-eskil/Nuisance.gif\
	eskil.vfs/lib/app-eskil/COPYING\
	eskil.vfs/lib/griffin\
	eskil.vfs/lib/textsearch\
	eskil.vfs/lib/psballoon\
	eskil.vfs/lib/pstools

setup: links

#----------------------------------------------------------------
# Testing
#----------------------------------------------------------------

check:
	@nagelfar.kit eskil.tcl

test:
	@./testsuite.tcl

#----------------------------------------------------------------
# Packaging/Releasing
#----------------------------------------------------------------

wrap:
	sdx wrap eskil.kit

wrapexe:
	@\rm -f eskil.linux eskil.exe eskil.solaris
	sdx wrap eskil.linux   -runtime $(TCLKIT_LINUX)
	sdx wrap eskil.solaris -runtime $(TCLKIT_SOLARIS)
	sdx wrap eskil.exe     -runtime $(TCLKIT_WIN)

release: setup wrap wrapexe
