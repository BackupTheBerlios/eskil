#!/bin/sh
\rm -f eskil.linux eskil.exe eskil.solaris
\rm -f eskil.linux.gz eskil.exe.gz eskil.solaris.gz
sdx wrap eskil.linux -runtime /home/peter/tclkit/tclkit-linux-x86
sdx wrap eskil.solaris -runtime /home/peter/tclkit/tclkit-solaris-sparc
sdx wrap eskil.exe -runtime /home/peter/tclkit/tclkit-win32.upx.exe
#gzip eskil.linux eskil.exe eskil.solaris
