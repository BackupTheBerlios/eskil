
copy diff.tcl tcldiff.tcl
copy diff.exe c:\
freewrap.exe tcldiff.tcl c:\diff.exe
del c:\diff.exe
del tcldiff.tcl

freewrap.exe dirdiff.tcl
