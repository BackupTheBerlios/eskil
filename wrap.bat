
copy diff.tcl diffw.tcl
copy diff.exe c:\
freewrap.exe diffw.tcl c:\diff.exe
del c:\diff.exe
del diffw.tcl

freewrap.exe dirdiff.tcl
