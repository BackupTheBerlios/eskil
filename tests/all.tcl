#!/bin/sh
#----------------------------------------------------------------------
# $Revision$
#----------------------------------------------------------------------
# the next line restarts using tclsh \
exec tclsh "$0" "$@"

set testScript [file normalize [file join [pwd] [info script]]]
set testDir    [file dirname $testScript]

lappend auto_path eskil.vfs/lib

package require tcltest 2.2
namespace import tcltest::*
tcltest::configure -verbose "body error"
#testConstraint knownbug 1
#tcltest::configure -match eskil-1.*

package require Tk
wm withdraw .

set ::eskil_testsuite 1

set instrument 1
if {$instrument} {
    puts "Instrumenting"
    exec ./instrument.tcl compareBlocks
    source eskili.tcl
} else {
    source eskil.tcl
}
puts "Running Tests"

foreach test [glob -nocomplain $testDir/*.test] {
    source $test
}

if {$instrument} {
    puts "Checking instrumenting result"
    set ch [open _instrument_result w]
    puts $ch [join [lsort -integer [array names ::_instrument]] \n]
    close $ch
    set ch [open {|diff _instrument_lines _instrument_result}]
    while {[gets $ch line] >= 0} {
        if {[regexp {<|>} $line]} {
            puts $line
        }
    }
    catch {close $ch}
    file delete -force _instrument_lines _instrument_result
    file delete -force eskili.tcl
}
exit
