#----------------------------------------------------------------------
# Make file for Eskil
#----------------------------------------------------------------------

VERSION = 26

# Path to the TclKits used for creating StarPacks.
TCLKIT = /home/peter/tclkit/v85
TCLKIT_LINUX   = $(TCLKIT)/tclkit-8.5.8
TCLKIT_SOLARIS = $(TCLKIT)/tclkit-solaris-sparc
TCLKIT_WIN     = $(TCLKIT)/tclkit-win32.upx.exe

# Path to the libraries used
STYLE      = /home/peter/src/packages/style
GRIFFIN    = /home/peter/tclkit/griffin.vfs/lib/griffin
TEXTSEARCH = /home/peter/src/textsearch
DIFFUTIL   = /home/peter/src/DiffUtilTcl/lib.vfs/DiffUtil
WCB        = /home/peter/src/packages/wcb3.0
PDF4TCL    = /home/peter/src/pdf4tcl/pkg
SNIT       = /home/peter/tcl/tcllib/modules/snit
STRUCT     = /home/peter/tcl/tcllib/modules/struct
CMDLINE    = /home/peter/tcl/tcllib/modules/cmdline
TWAPI      = /home/peter/src/packages/twapi
TKDND      = /home/peter/src/packages/tkdnd/lib/tkdnd1.0

# Tools
NAGELFAR    = nagelfar

all: setup

SRCFILES = src/eskil.tcl src/clip.tcl src/dirdiff.tcl src/help.tcl src/map.tcl \
	   src/print.tcl src/registry.tcl src/rev.tcl \
	   src/compare.tcl src/merge.tcl src/printobj.tcl src/plugin.tcl

#----------------------------------------------------------------
# Setup symbolic links from the VFS to the real files
#----------------------------------------------------------------

eskil.vfs/src/eskil.tcl:
	@cd eskil.vfs/src ; for i in $(SRCFILES); do ln -fs ../../$$i ; done
eskil.vfs/examples:
	cd eskil.vfs ; ln -s ../examples
eskil.vfs/doc:
	cd eskil.vfs ; ln -s ../doc
eskil.vfs/plugins:
	cd eskil.vfs ; ln -s ../plugins
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
eskil.vfs/lib/pdf4tcl:
	cd eskil.vfs/lib ; ln -s $(PDF4TCL) pdf4tcl
eskil.vfs/lib/tkdnd:
	cd eskil.vfs/lib ; ln -s $(TKDND) tkdnd
eskil.vfs/lib/snit:
	cd eskil.vfs/lib ; mkdir snit
	cd eskil.vfs/lib/snit ; ln -s $(SNIT)/pkgIndex.tcl
	cd eskil.vfs/lib/snit ; ln -s $(SNIT)/snit.tcl
	cd eskil.vfs/lib/snit ; ln -s $(SNIT)/snit2.tcl
	cd eskil.vfs/lib/snit ; ln -s $(SNIT)/main2.tcl
	cd eskil.vfs/lib/snit ; ln -s $(SNIT)/main1.tcl
	cd eskil.vfs/lib/snit ; ln -s $(SNIT)/validate.tcl
eskil.vfs/lib/struct:
	cd eskil.vfs/lib ; mkdir struct
	cd eskil.vfs/lib/struct ; ln -s $(STRUCT)/pkgIndex.tcl
	cd eskil.vfs/lib/struct ; ln -s $(STRUCT)/list.tcl
eskil.vfs/lib/cmdline:
	cd eskil.vfs/lib ; ln -s $(CMDLINE) cmdline

links: eskil.vfs/src/eskil.tcl \
	eskil.vfs/examples\
	eskil.vfs/doc\
	eskil.vfs/plugins\
	eskil.vfs/COPYING\
	eskil.vfs/lib/griffin\
	eskil.vfs/lib/style\
	eskil.vfs/lib/textsearch\
	eskil.vfs/lib/diffutil\
	eskil.vfs/lib/pdf4tcl\
	eskil.vfs/lib/snit\
	eskil.vfs/lib/struct\
	eskil.vfs/lib/cmdline\
	eskil.vfs/lib/tkdnd\
	eskil.vfs/lib/wcb

setup: links

#----------------------------------------------------------------
# Testing
#----------------------------------------------------------------

spell:
	@cat doc/*.txt | ispell -d british -l | sort -u

NAGELFARFLAGS = -s syntaxdb86.tcl -filter "*Non constant definition*" -quiet

# Create a common "header" file for all source files.
eskil_h.syntax: $(SRCFILES) src/eskil.syntax
	@echo Creating syntax header file...
	@$(NAGELFAR) $(NAGELFARFLAGS) -header eskil_h.syntax $(SRCFILES)

check: eskil_h.syntax
	@echo Checking...
	@for i in $(SRCFILES); do $(NAGELFAR)  $(NAGELFARFLAGS) eskil_h.syntax $$i ; done

test:
	@./tests/all.tcl

#----------------------------------------------------------------
# Coverage
#----------------------------------------------------------------

# Source files for code coverage
COVFILES = src/rev.tcl src/eskil.tcl
IFILES   = $(COVFILES:.tcl=.tcl_i)
LOGFILES = $(COVFILES:.tcl=.tcl_log)
MFILES   = $(COVFILES:.tcl=.tcl_m)

# Instrument source file for code coverage
%.tcl_i: %.tcl
	@$(NAGELFAR) -instrument $<

# Target to prepare for code coverage run. Makes sure log file is clear.
instrument: $(IFILES)
	@rm -f $(LOGFILES)

# Run tests to create log file.
testcover $(LOGFILES): $(IFILES)
	@./tests/all.tcl $(TESTFLAGS)

# Create markup file for better view of result
%.tcl_m: %.tcl_log
	@$(NAGELFAR) -markup $*.tcl

# View code coverage result
icheck: $(MFILES)
	@for i in $(COVFILES) ; do eskil -noparse $$i $${i}_m & done

# Remove code coverage files
clean:
	@rm -f $(LOGFILES) $(IFILES) $(MFILES)

#----------------------------------------------------------------
# Packaging/Releasing
#----------------------------------------------------------------

wrap:
	sdx wrap eskil.kit

wrapexe:
	@\rm -f eskil.linux eskil.exe eskil.solaris
	sdx wrap eskil.linux   -runtime $(TCLKIT_LINUX)
	sdx wrap eskil.solaris -runtime $(TCLKIT_SOLARIS)
	cd eskil.vfs/lib ; ln -s $(TWAPI) twapi
	sdx wrap eskil.exe     -runtime $(TCLKIT_WIN)
	rm eskil.vfs/lib/twapi

release: setup wrap wrapexe
	@cp eskil.kit eskil`date +%Y%m%d`.kit
	@cp eskil.kit eskil$(VERSION).kit
	@gzip eskil.linux
	@mv eskil.linux.gz eskil$(VERSION).linux.gz
	@gzip eskil.solaris
	@mv eskil.solaris.gz eskil$(VERSION).solaris.gz
	@zip eskil$(VERSION).win.zip eskil.exe
	@zip eskil`date +%Y%m%d`.win.zip eskil.exe
