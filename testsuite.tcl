#!/bin/sh
#----------------------------------------------------------------------
# $Revision$
#----------------------------------------------------------------------
# the next line restarts using tclsh \
exec tclsh "$0" "$@"

lappend auto_path eskil.vfs/lib

package require tcltest 2.2
namespace import tcltest::*
tcltest::configure -verbose "body error"
#testConstraint knownbug 1
#tcltest::configure -match eskil-1.*

package require Tk
wm withdraw .

set ::eskil_testsuite 1

set instrument 0
if {$instrument} {
    exec ./instrument.tcl compareBlocks
    source eskili.tcl
} else {
    source eskil.tcl
}

test eskil-2.1 {
    Change-block parsing
} -body {
    set b1 [list "Apa 1" "Bepa 1" "Cepa 1"]
    set b2 [list "Apa 2" "Bepa 2" "Cepa 2"]
    compareBlocks $b1 $b2
} -result {c c c}

test eskil-2.2 {
    Change-block parsing
} -body {
    set b1 [list "Apa 1" "Bepa 1"         "Cepa 1"]
    set b2 [list         "Bepa 2" "Gurka" "Cepa 2"]
    compareBlocks $b1 $b2
} -result {d c a c}

test eskil-2.3 {
    Change-block parsing
} -body {
    set b1 [list "Apa 1" "Bepa 1" "Cepa 1"]
    set b2 [list                  "Cepa 2" "Gurka"]
    compareBlocks $b1 $b2
} -result {d d c a}

test eskil-2.4 {
    Change-block parsing
} -body {
    set b1 [list "Apa 1" "Bepa 1" "Cepa 1"]
    set b2 [list         "Bepa 2" "Gurka"]
    compareBlocks $b1 $b2
} -result {d c C}

test eskil-2.5 {
    Change-block parsing
} -body {
    set b1 [list "Apa 1" "Bepa 1" "Cepa 1" "Depa 1"]
    set b2 [list         "Bepa 2" "Gurka"]
    compareBlocks $b1 $b2
} -result {d c C d}

test eskil-2.6 {
    Change-block parsing
} -body {
    set b1 [list "Apa 1" "Bepa 1"         "Cepa 1" "Depa 1"]
    set b2 [list         "Bepa 2" "Apa 2" "Cepa 2"]
    compareBlocks $b1 $b2
} -result {d c a c d}

if {$instrument} {
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
    #file delete _instrument_lines _instrument_result
    #file delete eskili.tcl
}
exit
