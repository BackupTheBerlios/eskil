#!/bin/sh
#----------------------------------------------------------------------
# A simple instrumenter for code coverage
#----------------------------------------------------------------------
# $Revision$
#----------------------------------------------------------------------
# the next line restarts using tclsh \
exec tclsh "$0" "$@"

set ch [open eskil.tcl r]
set cho [open eskili.tcl w]
if {$argc == 0} {
    set instrument 1
    set endRE ""
} else {
    set instrument 0
    set startRE "proc (?:[join $argv |])"
    set endRE "^\}\\s*$"
}
set commRE "^\\s*\#"
# Missing: switch branches
set iRE {(?:if|elseif|else|while|for|foreach) .*\{\s*$}
set lineNo 0
set prev ""
while {[gets $ch line] >= 0} {
    incr lineNo
    if {$prev ne ""} {
        set line $prev\n$line
        set prev ""
    }
    if {[string index $line end] eq "\\"} {
        set prev $line
        continue
    }
    if {$instrument} {
        if {$endRE ne "" && [regexp $endRE $line]} {
            set instrument 0
            puts $cho $line
            continue
        }
        if {![regexp $commRE $line]&& [regexp $iRE $line]} {
            append line " [list set ::_instrument($lineNo) 1]"
            set ::_instrument($lineNo) 1
        }
    } else {
        if {[regexp $startRE $line]} {
            set instrument 1
        }
    }
    puts $cho $line
}
close $cho
close $ch

set ch [open _instrument_lines w]
puts $ch [join [lsort -integer [array names ::_instrument]] \n]
close $ch
