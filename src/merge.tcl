#----------------------------------------------------------------------
#  Eskil, Merge function
#
#  Copyright (c) 1998-2007, Peter Spjuth  (peter.spjuth@gmail.com)
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; see the file COPYING.  If not, write to
#  the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
#  Boston, MA 02111-1307, USA.
#
#----------------------------------------------------------------------
# $Revision$
#----------------------------------------------------------------------

# Get all data from the files to merge
proc collectMergeData {top} {
    global diff

    set diff($top,leftMergeData) {}
    set diff($top,rightMergeData) {}

    if {![info exists ::diff($top,changes)]} {
        set ::diff($top,changes) {}
    }

    prepareFiles $top

    set ch1 [open $::diff($top,leftFile) r]
    set ch2 [open $::diff($top,rightFile) r]
    set doingLine1 1
    set doingLine2 1
    set changeNo 0
    foreach change $::diff($top,changes) {
        lassign $change start length type line1 n1 line2 n2
        set data1 {}
        set data2 {}
        while {$doingLine1 < $line1} {
            gets $ch1 apa
            append data1 $apa\n
            incr doingLine1
        }
        while {$doingLine2 < $line2} {
            gets $ch2 apa
            append data2 $apa\n
            incr doingLine2
        }
        lappend diff($top,leftMergeData) $data1
        lappend diff($top,rightMergeData) $data2

        set data1 {}
        set data2 {}
        for {set t 0} {$t < $n1} {incr t} {
            gets $ch1 apa
            append data1 $apa\n
            incr doingLine1
        }
        for {set t 0} {$t < $n2} {incr t} {
            gets $ch2 apa
            append data2 $apa\n
            incr doingLine2
        }
        lappend diff($top,leftMergeData) $data1
        lappend diff($top,rightMergeData) $data2
        set diff($top,mergeSelection,$changeNo) 2
        incr changeNo
    }
    set data1 {}
    set data2 {}
    while {[gets $ch1 apa] != -1} {
        append data1 $apa\n
        incr doingLine1
    }
    while {[gets $ch2 apa] != -1} {
        append data2 $apa\n
        incr doingLine2
    }
    lappend diff($top,leftMergeData) $data1
    lappend diff($top,rightMergeData) $data2

    close $ch1
    close $ch2

    cleanupFiles $top
}

# Fill up the merge window with the initial version of merged files.
proc fillMergeWindow {top} {
    global diff

    set w $top.merge.t
    $w delete 1.0 end
    set marks {}
    set t 0
    foreach {commLeft diffLeft} $diff($top,leftMergeData) \
            {commRight diffRight} $diff($top,rightMergeData) {
        $w insert end $commRight
        if {![info exists diff($top,mergeSelection,$t)]} continue
        $w mark set merges$t insert
        $w mark gravity merges$t left
        $w insert end $diffRight merge$t
        lappend marks mergee$t [$w index insert]
        set diff($top,mergeSelection,$t) 2
        incr t
    }
    foreach {mark index} $marks {
        $w mark set $mark $index
    }
    set diff($top,curMerge) 0
    set diff($top,curMergeSel) 2
    $w tag configure merge0 -foreground red
    showDiff $top 0
    update
    # If there is any diff, show the first
    if {$t > 0} {
        seeText $w merges0 mergee0
    }
}

# Move to and highlight another diff.
proc nextMerge {top delta} {
    global diff

    set w $top.merge.t
    $w tag configure merge$diff($top,curMerge) -foreground ""

    set diff($top,curMerge) [expr {$diff($top,curMerge) + $delta}]
    if {$diff($top,curMerge) < 0} {set diff($top,curMerge) 0}
    if {$diff($top,curMerge) >= ([llength $diff($top,leftMergeData)] / 2)} {
        set diff($top,curMerge) \
                [expr {[llength $diff($top,leftMergeData)] / 2 - 1}]
    }
    set diff($top,curMergeSel) $diff($top,mergeSelection,$diff($top,curMerge))
    $w tag configure merge$diff($top,curMerge) -foreground red
    showDiff $top $diff($top,curMerge)
    seeText $w merges$diff($top,curMerge) mergee$diff($top,curMerge)
}

# Select a merge setting for all diffs.
proc selectMergeAll {top new} {
    global diff
    set end [expr {[llength $diff($top,leftMergeData)] / 2}]
    for {set t 0} {$t < $end} {incr t} {
        selectMerge2 $top $t $new
    }
    set diff($top,curMergeSel) $new
    set w $top.merge.t
    seeText $w merges$diff($top,curMerge) mergee$diff($top,curMerge)
}

# Change merge setting fo current diff.
proc selectMerge {top} {
    global diff

    set w $top.merge.t
    selectMerge2 $top $diff($top,curMerge) $diff($top,curMergeSel)
    seeText $w merges$diff($top,curMerge) mergee$diff($top,curMerge)
}

# Change merge setting for a diff.
proc selectMerge2 {top no new} {
    global diff

    set w $top.merge.t
    # Delete current string
    $w delete merges$no mergee$no

    set diff($top,mergeSelection,$no) $new

    set i [expr {$no * 2 + 1}]
    set diffLeft [lindex $diff($top,leftMergeData) $i]
    set diffRight [lindex $diff($top,rightMergeData) $i]

    if {$diff($top,mergeSelection,$no) == 12} {
        $w insert merges$no $diffLeft$diffRight merge$no
    } elseif {$diff($top,mergeSelection,$no) == 21} {
        $w insert merges$no $diffRight$diffLeft merge$no
    } elseif {$diff($top,mergeSelection,$no) == 1} {
        $w insert merges$no $diffLeft merge$no
    } elseif {$diff($top,mergeSelection,$no) == 2} {
        $w insert merges$no $diffRight merge$no
    }
}

# Save the merge result.
proc saveMerge {top} {
    set w $top.merge.t

    if {$::diff($top,mergeFile) eq "" && $::diff($top,mode) eq "conflict"} {
        set apa [tk_messageBox -parent $top.merge -icon question \
                -title "Save merge file" -type yesno -message \
                "Do you want to overwrite the original conflict file?"]
        if {$apa == "yes"} {
            set ::diff($top,mergeFile) $::diff($top,conflictFile)
        }
    }
    if {$::diff($top,mergeFile) eq ""} {
        # Ask user which file
        set buttons {}
        set text "Overwrite file or Browse?"
        if {[file exists $::diff($top,leftFile)] && \
                $::diff($top,leftFile) eq $::diff($top,leftLabel)} {
            lappend buttons Left
            append text "\nLeft: $::diff($top,leftFile)"
        }
        if {[file exists $::diff($top,rightFile)] && \
                $::diff($top,rightFile) eq $::diff($top,rightLabel)} {
            lappend buttons Right
            append text "\nRight: $::diff($top,rightFile)"
        }
        lappend buttons Browse Cancel
        if {[llength $buttons] > 2} {
            set apa [tk_dialog .savemerge "Save merge file" \
                    $text \
                    questhead -1 {*}$buttons]
            if {$apa < 0} return
            set apa [lindex $buttons $apa]
            if {$apa eq "Left"} {
                set ::diff($top,mergeFile) $::diff($top,leftFile)
            } elseif {$apa eq "Right"} {
                set ::diff($top,mergeFile) $::diff($top,rightFile)
            } elseif {$apa eq "Cancel"} {
                return
            }
        }
        if {$::diff($top,mergeFile) eq ""} {
            # Browse
            if {[info exists ::diff($top,rightDir)]} {
                set initDir $::diff($top,rightDir)
            } elseif {[info exists ::diff($top,leftDir)]} {
                set initDir $::diff($top,leftDir)
            } else {
                set initDir [pwd]
            }

            set apa [tk_getSaveFile -title "Save merge file" -initialdir $initDir \
                    -parent $top.merge]
            if {$apa eq ""} return
            set ::diff($top,mergeFile) $apa
        }
    }

    set ch [open $::diff($top,mergeFile) "w"]
    fconfigure $ch -translation $::diff($top,mergetranslation)
    puts -nonewline $ch [$w get 1.0 end-1char]
    close $ch

    # Detect if this is a GIT merge, and possibly add it to the index
    # after save (i.e. git add file)
    if {[detectRevSystem $::diff($top,mergeFile)] eq "GIT"} {
        set apa [tk_messageBox -parent $top.merge -icon info -type yesno \
                -title "Diff" \
                -message "Saved merge to file $::diff($top,mergeFile).\nAdd\
                it to GIT index?"]
        if {$apa eq "yes"} {
            eskil::rev::GIT::add $::diff($top,mergeFile)
        }
    } else {
        tk_messageBox -parent $top.merge -icon info -type ok -title "Diff" \
                -message "Saved merge to file $::diff($top,mergeFile)."
    }
}

# Close merge window and clean up.
proc closeMerge {top} {
    global diff

    destroy $top.merge
    set diff($top,leftMergeData) {}
    set diff($top,rightMergeData) {}
    array unset diff $top,mergeSelection,*
}

# Create a window to display merge result.
proc makeMergeWin {top} {
    if {![info exists ::diff($top,mergetranslation)]} {
        if {$::tcl_platform(platform) eq "windows"} {
            set ::diff($top,mergetranslation) crlf
        } else {
            set ::diff($top,mergetranslation) lf
        }
    }

    set w $top.merge
    if {![winfo exists $w]} {
        toplevel $w
    } else {
        destroy {*}[winfo children $w]
    }

    wm title $w "Merge result"

    menu $w.m
    $w configure -menu $w.m
    $w.m add cascade -label "File" -underline 0 -menu $w.m.mf
    menu $w.m.mf
    $w.m.mf add command -label "Save" -underline 0 -command "saveMerge $top"
    $w.m.mf add separator
    $w.m.mf add command -label "Close" -underline 0 -command "closeMerge $top"

    $w.m add cascade -label "Select" -underline 0 -menu $w.m.ms
    menu $w.m.ms
    $w.m.ms add radiobutton -label "Left+Right"         -value 12 \
            -variable diff($top,curMergeSel) -command "selectMerge $top"
    $w.m.ms add radiobutton -label "Left" -underline 0  -value 1  \
            -variable diff($top,curMergeSel) -command "selectMerge $top"
    $w.m.ms add radiobutton -label "Right" -underline 0 -value 2  \
            -variable diff($top,curMergeSel) -command "selectMerge $top"
    $w.m.ms add radiobutton -label "Right+Left"         -value 21 \
            -variable diff($top,curMergeSel) -command "selectMerge $top"
    $w.m.ms add separator
    $w.m.ms add command -label "All Left"  -command "selectMergeAll $top 1"
    $w.m.ms add command -label "All Right" -command "selectMergeAll $top 2"

    $w.m add cascade -label "Config" -underline 0 -menu $w.m.mc
    menu $w.m.mc
    $w.m.mc add radiobutton -label "Line end LF"   -value lf   -variable diff($top,mergetranslation)
    $w.m.mc add radiobutton -label "Line end CRLF" -value crlf -variable diff($top,mergetranslation)
    if {$::diff($top,mode) eq "conflict"} {
        $w.m.mc add separator
        $w.m.mc add checkbutton -label "Pure" -variable diff($top,modetype) \
                -onvalue "Pure" -offvalue "" -command {doDiff}
    }

    ttk::frame $w.f

    ttk::radiobutton $w.f.rb1 -text "LR" -value 12 \
            -variable diff($top,curMergeSel) \
            -command "selectMerge $top"
    ttk::radiobutton $w.f.rb2 -text "L"  -value 1 \
            -variable diff($top,curMergeSel) \
            -command "selectMerge $top"
    ttk::radiobutton $w.f.rb3 -text "R"  -value 2 \
            -variable diff($top,curMergeSel) \
            -command "selectMerge $top"
    ttk::radiobutton $w.f.rb4 -text "RL" -value 21 \
            -variable diff($top,curMergeSel) \
            -command "selectMerge $top"
    bind $w <Key-Left>  "focus $w; set diff($top,curMergeSel) 1; selectMerge $top"
    bind $w <Key-Right> "focus $w; set diff($top,curMergeSel) 2; selectMerge $top"

    ttk::button $w.f.bl -text "All L" -command "selectMergeAll $top 1"
    ttk::button $w.f.br -text "All R" -command "selectMergeAll $top 2"
    
    ttk::button $w.f.b1 -text "Prev" -command "nextMerge $top -1"
    ttk::button $w.f.b2 -text "Next" -command "nextMerge $top 1"
    bind $w <Key-Down> "focus $w ; nextMerge $top 1"
    bind $w <Key-Up>   "focus $w ; nextMerge $top -1"
    bind $w <Shift-Key-Down> "focus $w ; nextMerge $top 10"
    bind $w <Shift-Key-Up>   "focus $w ; nextMerge $top -10"

    ttk::button $w.f.bs -text "Save" -command "saveMerge $top"
    ttk::button $w.f.bq -text "Close" -command "closeMerge $top"
    wm protocol $w WM_DELETE_WINDOW "closeMerge $top"

    grid $w.f.rb1 $w.f.rb2 $w.f.rb3 $w.f.rb4 x $w.f.b1 $w.f.b2 x \
            $w.f.bl $w.f.br x $w.f.bs $w.f.bq -sticky we -padx 1
    grid columnconfigure $w.f {4 7 10} -minsize 10
    grid columnconfigure $w.f 10 -weight 1
    grid columnconfigure $w.f {0 1 2 3} -uniform a
    grid columnconfigure $w.f {5 6 8 9 11 12} -uniform b
    #grid columnconfigure $w.f {11 13 14} -uniform c

    text $w.t -width 80 -height 20 -xscrollcommand "$w.sbx set" \
            -yscrollcommand "$w.sby set" -font myfont
    scrollbar $w.sbx -orient horizontal -command "$w.t xview"
    scrollbar $w.sby -orient vertical   -command "$w.t yview"

    bind $w.t <Key-Escape> [list focus $w]

    # Prevent toplevel bindings on keys to fire while in the text widget.
    bindtags $w.t [list Text $w.t $w all]
    bind $w.t <Key-Left>  "break"
    bind $w.t <Key-Right> "break"
    bind $w.t <Key-Down>  "break"
    bind $w.t <Key-Up>    "break"
    bind $w.t <Shift-Key-Down> "break"
    bind $w.t <Shift-Key-Up>   "break"

    grid $w.f   -      -sticky news -row 0
    grid $w.t   $w.sby -sticky news
    grid $w.sbx x      -sticky we
    grid columnconfigure $w 0 -weight 1
    grid rowconfigure $w 1 -weight 1

    collectMergeData $top
    fillMergeWindow $top
}
