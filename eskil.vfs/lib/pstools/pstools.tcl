#----------------------------------------------------------------------
#
#  pstools.tcl,
#     a package providing misc facilites
#
#  Copyright (c) 2003, Peter Spjuth  (peter.spjuth@gmail.com)
#
#  Permission is granted to use this code under the same terms as
#  for the Tcl core code.
#
#----------------------------------------------------------------------
# $Revision: 1.6 $
#----------------------------------------------------------------------

package provide pstools 0.3
package require Tcl 8.4

namespace eval pstools {
    namespace export safeLoad commonYScroll locateTmp locateEditor

    if {[info commands ::ttk::*] ne ""} {
        catch {namespace path ::ttk}
    }
}

##nagelfar syntax _ipexists l
##nagelfar syntax _ipset    v
##nagelfar syntax _iparray  s v

# Load a preference file
# Args lists the variables allowed to be set by the file
proc pstools::safeLoad {file args} {

    interp create -safe loadinterp
    interp alias {} _ipexists loadinterp info exists
    interp alias {} _ipset    loadinterp set
    interp alias {} _iparray  loadinterp array

    interp invokehidden loadinterp source $file

    foreach arg $args {
        upvar 1 $arg TheVar
        if {[_iparray exists $arg]} {
            foreach {key val} [_iparray get $arg] {
                if {[info exists TheVar($key)]} {
                    set TheVar($key) $val
                }
            }
        } elseif {[_ipexists $arg]} {
            if {[info exists TheVar]} {
                set TheVar [_ipset $arg]
            }
        }
    }

    interp delete loadinterp
}


# Procedures for common y-scroll
proc pstools::CommonYScroll_YView {sby args} {
    variable yscroll
    foreach w $yscroll($sby) {
        eval [list $w yview] $args
    }
}

proc pstools::CommonYScroll_YScroll {sby args} {
    eval [list $sby set] $args
    CommonYScroll_YView $sby moveto [lindex $args 0]
}

# Set up a common yscrollbar for a few scrollable widgets
proc pstools::commonYScroll {sby args} {
    variable yscroll

    $sby configure -command [list pstools::CommonYScroll_YView $sby]
    foreach w $args {
        $w configure -yscrollcommand [list pstools::CommonYScroll_YScroll $sby]
    }
    set yscroll($sby) $args
}

# A simple window for displaying e.g. help.
# Returns the frame where things can be put.
proc pstools::helpWin {w title} {
    destroy $w

    toplevel $w -padx 2 -pady 2
    wm title $w $title
    bind $w <Key-Return> [list destroy $w]
    bind $w <Key-Escape> [list destroy $w]
    frame $w.f
    button $w.b -text "Close" -command [list destroy $w] -width 10 \
            -default active
    pack $w.b -side bottom -pady 2
    pack $w.f -side top -expand y -fill both -padx 2 -pady 2
    focus $w
    return $w.f
}

# Figure out a place to store temporary files.
proc pstools::locateTmp {globVar} {
    upvar "#0" $globVar var

    set candidates {}
    if {[info exists ::env(TEMP)]} {
        lappend candidates $::env(TEMP)
    }
    if {[info exists ::env(TMP)]} {
        lappend candidates $::env(TMP)
    }
    lappend candidates /tmp . ~
    foreach cand $candidates {
        set cand [file normalize $cand]
        if {[file isdirectory $cand] && [file writable $cand]} {
            set var $cand
            return
        }
    }
    # Panic?
    set var .
}

# This is called when an editor is needed to display a file.
# It sets up the variable with the path, unless the var
# already exists.
proc pstools::locateEditor {globVar} {
    upvar "#0" $globVar var

    if {[info exists var]} return
    
    # What is a good value on Mac?
    if {$::tcl_platform(platform) == "unix"} {
        set var emacs
    } else {
        set var wordpad
        foreach dir [lsort -decreasing -dictionary \
                             [glob -nocomplain c:/apps/emacs*]] {
            set em [file join $dir bin runemacs.exe]
            set em [file normalize $em]
            if {[file exists $em]} {
                set var $em
                break
            }
        }
    }
}
