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

source src/eskil.tcl
Init

puts "Running Tests"

foreach test [glob -nocomplain $testDir/*.test] {
    source $test
}

exit
