#!/bin/sh
#----------------------------------------------------------------------
# $Revision$
#----------------------------------------------------------------------
# the next line restarts using tclsh \
exec tclsh8.5 "$0" "$@"

set testScript [file normalize [file join [pwd] [info script]]]
set testDir    [file dirname $testScript]

lappend auto_path eskil.vfs/lib

package require tcltest 2.2
namespace import tcltest::*
tcltest::configure -verbose "body error"
#testConstraint knownbug 1
#tcltest::configure -match print-*

if {$argc > 0} {
    eval tcltest::configure $argv
}

package require Tk
wm withdraw .

set ::eskil_testsuite 1

if {[file exists eskil.vfs/src/eskil.tcl_i]} {
    puts "Running with code coverage"
    source eskil.vfs/src/eskil.tcl_i
} else {
    source eskil.vfs/src/eskil.tcl
}
Init

# Helpers to temporarily stub things out
set ::stubs {}
proc stub {name argv body} {
    if {[info commands _stub_$name] eq ""} {
        rename $name _stub_$name
    }
    proc $name $argv $body
    lappend ::stubs $name
}

proc clearstub {} {
    foreach name $::stubs {
        rename $name {}
        rename _stub_$name $name
    }
    set ::stubs {}
}

proc ExecEskil {args} {
    return [exec ./eskil.kit {*}$args]
}

puts "Running Tests"

foreach test [glob -nocomplain $testDir/*.test] {
    source $test
    clearstub
}
tcltest::cleanupTests 1

exit
