# Path to the TclKits used for creating StarPacks.
TCLKIT = /home/peter/tclkit
TCLKIT_LINUX   = $(TCLKIT)/tclkit-linux-x86
TCLKIT_SOLARIS = $(TCLKIT)/tclkit-solaris-sparc
TCLKIT_WIN     = $(TCLKIT)/tclkit-win32.upx.exe

# Path to the libraries used
STYLE      = /home/peter/src/packages/style
GRIFFIN    = /home/peter/tclkit/griffin.vfs/lib/griffin
PSBALLOON  = /home/peter/src/psballoon
PSTOOLS    = /home/peter/src/pstools
TEXTSEARCH = /home/peter/src/textsearch
DIFFUTIL   = /home/peter/src/DiffUtil/lib.vfs/DiffUtil
WCB        = /home/peter/src/packages/wcb3.0
#DIFFUTIL   = /home/peter/src/DiffUtil/tcl

all: setup

#----------------------------------------------------------------
# Setup symbolic links from the VFS to the real files
#----------------------------------------------------------------

eskil.vfs/src/eskil.tcl:
	cd eskil.vfs/src ; ln -s ../../src/eskil.tcl
eskil.vfs/examples:
	cd eskil.vfs ; ln -s ../examples
eskil.vfs/doc:
	cd eskil.vfs ; ln -s ../doc
eskil.vfs/COPYING:
	cd eskil.vfs ; ln -s ../COPYING
eskil.vfs/lib/wcb:
	cd eskil.vfs/lib ; ln -s $(WCB) wcb
eskil.vfs/lib/style:
#	cd eskil.vfs/lib ; ln -s $(STYLE) style
eskil.vfs/lib/griffin:
	cd eskil.vfs/lib ; ln -s $(GRIFFIN) griffin
eskil.vfs/lib/textsearch:
	cd eskil.vfs/lib ; ln -s $(TEXTSEARCH) textsearch
eskil.vfs/lib/diffutil:
	cd eskil.vfs/lib ; ln -s $(DIFFUTIL) diffutil
eskil.vfs/lib/psballoon:
	mkdir eskil.vfs/lib/psballoon
	cd eskil.vfs/lib/psballoon ; ln -s $(PSBALLOON)/psballoon.tcl
	cd eskil.vfs/lib/psballoon ; ln -s $(PSBALLOON)/pkgIndex.tcl
eskil.vfs/lib/pstools:
	mkdir eskil.vfs/lib/pstools
	cd eskil.vfs/lib/pstools ; ln -s $(PSTOOLS)/pstools.tcl
	cd eskil.vfs/lib/pstools ; ln -s $(PSTOOLS)/pkgIndex.tcl

links: eskil.vfs/src/eskil.tcl\
	eskil.vfs/examples\
	eskil.vfs/doc\
	eskil.vfs/COPYING\
	eskil.vfs/lib/griffin\
	eskil.vfs/lib/style\
	eskil.vfs/lib/textsearch\
	eskil.vfs/lib/psballoon\
	eskil.vfs/lib/pstools\
	eskil.vfs/lib/diffutil\
	eskil.vfs/lib/wcb

setup: links

#----------------------------------------------------------------
# Testing
#----------------------------------------------------------------

spell:
	@cat doc/*.txt | ispell -d british -l | sort -u

check:
	@nagelfar src/eskil.tcl

test:
	@./tests/all.tcl

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
