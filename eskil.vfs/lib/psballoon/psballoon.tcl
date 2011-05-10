#----------------------------------------------------------------------
#
#  psballoon.tcl,
#   Procedures to create help message balloons or display balloons for
#   listboxes and labels that can't display all of their contents.
#
#  Copyright (c) 2003, Peter Spjuth  (peter.spjuth@space.se)
#
#  Permission is granted to use this code under the same terms as
#  for the Tcl core code.
#
#----------------------------------------------------------------------
# $Revision: 1.1 $
#----------------------------------------------------------------------

package provide psballoon 1.0

namespace eval psballoon {
    variable balloon

    set balloon(pending) 0
    set balloon(created) 0
    set balloon(id) ""
    namespace export addBalloon
}

proc psballoon::addBalloon {w {msg ""}} {
    variable balloon

    set c [winfo class $w]
    if {$msg == "" && $c != "Listbox" && $c != "Label"} {
        error "Missing message to balloon for $w ($c)"
    }
    set balloon(msg,$w) $msg
    bind $w <Enter> {
        set ::psballoon::balloon(pending) 1
        set ::psballoon::balloon(created) 0
        set ::psballoon::balloon(id) [after 500 {psballoon::createBalloon %W %x %y}]
    }
    bind $w <Button> {
        psballoon::killBalloon
    }
    bind $w <Leave> {
        psballoon::killBalloon
    }
    bind $w <Motion> {
        if {$::psballoon::balloon(pending) == 1} {
            after cancel $::psballoon::balloon(id)
        }
        if {$::psballoon::balloon(created) == 1} {
            psballoon::killBalloon
        }
        set ::psballoon::balloon(id) [after 500 {psballoon::createBalloon %W %x %y}]
        set ::psballoon::balloon(pending) 1
    }
}

proc psballoon::killBalloon {} {
    variable balloon
    if {$balloon(pending) == 1} {
        after cancel $balloon(id)
    }
    if {[winfo exists .balloon] == 1} {
        destroy .balloon
    }
    set balloon(created) 0
    set balloon(pending) 0
}

proc psballoon::createBalloon {w mx my} {
    variable balloon
    if {$balloon(created) == 0} {
        # Figure out widget's font
        if {[catch {set font [$w cget -font]}]} {
            set font [ttk::style lookup [winfo class $w] -font]
        }
        set ww [winfo width $w]
        set ih [winfo height $w]
        set ix 0
        set iy 0
        set create 1
        set msg $balloon(msg,$w)
        if {$msg == ""} {
            switch [winfo class $w] {
                Listbox {
                    set i [$w index @$mx,$my]
                    set msg [$w get $i]
                    foreach {ix iy iw ih} [$w bbox $i] {break}
                }
                Label {
                    set msg [$w cget -text]
		    set iw [font measure $font $msg]
                }
            }
            #Don't create a balloon if the text is fully visible.
            set create [expr {$iw > $ww - 8}]
        } else {
	    set iw [font measure $font $msg]
	}
	if {$create} {
            set x [expr {[winfo rootx $w] + $ix}]
            set y [expr {[winfo rooty $w] + $iy + $ih + 2}]
            if {$x + $iw + 8 > [winfo screenwidth $w]} {
                set x [expr {[winfo screenwidth $w] - $iw - 8}]
            }
            toplevel .balloon -bg black
            wm overrideredirect .balloon 1
            label .balloon.l \
                    -text $msg -relief flat -font $font -justify left \
                    -bg #ffffaa -fg black -padx 2 -pady 0 -anchor w
            pack .balloon.l -side left -padx 1 -pady 1
            wm geometry .balloon +${x}+${y}
            set balloon(created) 1
        }
    }
}
