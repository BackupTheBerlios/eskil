#----------------------------------------------------------------------
#  Eskil, Merge function
#
#  Copyright (c) 1998-2011, Peter Spjuth  (peter.spjuth@gmail.com)
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

# Get all data from the files to merge
proc collectMergeData {top} {
    global eskil

    set eskil($top,leftMergeData) {}
    set eskil($top,rightMergeData) {}
    set eskil($top,mergeSelection,AnyConflict) 0

    if {![info exists eskil($top,changes)]} {
        set eskil($top,changes) {}
    }

    prepareFiles $top

    set ch1 [open $eskil($top,leftFile) r]
    set ch2 [open $eskil($top,rightFile) r]
    set doingLine1 1
    set doingLine2 1
    set changeNo 0
    foreach change $eskil($top,changes) {
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
        lappend eskil($top,leftMergeData) $data1
        lappend eskil($top,rightMergeData) $data2

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
        lappend eskil($top,leftMergeData) $data1
        lappend eskil($top,rightMergeData) $data2
        set eskil($top,mergeSelection,$changeNo) \
                [WhichSide $top $line1 $n1 $line2 $n2 conflict comment]
        set eskil($top,mergeSelection,Conflict,$changeNo) $conflict
        set eskil($top,mergeSelection,Comment,$changeNo) $comment
        if {$conflict} {
            set eskil($top,mergeSelection,AnyConflict) 1
        }
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
    lappend eskil($top,leftMergeData) $data1
    lappend eskil($top,rightMergeData) $data2

    close $ch1
    close $ch2

    cleanupFiles $top
}

# Fill up the merge window with the initial version of merged files.
proc fillMergeWindow {top} {
    global eskil

    set w $top.merge.t
    $w delete 1.0 end
    set marks {}
    set t 0
    set firstConflict -1
    foreach {commLeft diffLeft} $eskil($top,leftMergeData) \
            {commRight diffRight} $eskil($top,rightMergeData) {
        $w insert end $commRight
        if {![info exists eskil($top,mergeSelection,$t)]} continue
        $w mark set merges$t insert
        $w mark gravity merges$t left
        switch $eskil($top,mergeSelection,$t) {
            1 { $w insert end $diffLeft merge$t }
            2 { $w insert end $diffRight merge$t }
            12 { $w insert end $diffLeft merge$t 
                $w insert end $diffRight merge$t }
            21 { $w insert end $diffRight merge$t
                $w insert end $diffLeft merge$t  }
        }
        if {$eskil($top,mergeSelection,Conflict,$t)} {
            $w tag configure merge$t -background grey
            if {$firstConflict == -1} {
                set firstConflict $t
            }
        }
        lappend marks mergee$t [$w index insert]
        incr t
    }
    foreach {mark index} $marks {
        $w mark set $mark $index
    }
    # Add fences to simplify some handling later
    $w mark set mergee-2 1.0
    $w mark set mergee-1 1.0
    $w mark set merges$t end
    $w mark set merges[expr {$t + 1}] end

    set showFirst 0
    if {$firstConflict != -1} {
        set showFirst $firstConflict
    }

    set eskil($top,curMerge) $showFirst
    set eskil($top,curMergeSel) $eskil($top,mergeSelection,$showFirst)
    $w tag configure merge$showFirst -foreground red
    showDiff $top $showFirst
    update
    # If there is any diff, show the first
    if {$t > 0} {
        seeText $w merges$showFirst mergee$showFirst
        # Show status for first chunk
        set eskil($top,mergeStatus) \
                $eskil($top,mergeSelection,Comment,$showFirst)
    }
}

# Move to and highlight another diff.
proc nextMerge {top delta} {
    global eskil

    set w $top.merge.t
    $w tag configure merge$eskil($top,curMerge) -foreground ""

    set last [expr {[llength $eskil($top,leftMergeData)] / 2 - 1}]

    if {$delta == -1000} {
        # Search backward for conflict
        for {set t [expr {$eskil($top,curMerge) - 1}]} {$t >= 0} {incr t -1} {
            if {$eskil($top,mergeSelection,Conflict,$t)} {
                set delta [expr {$t - $eskil($top,curMerge)}]
                break
            }
        }
    } elseif {$delta == 1000} {
        # Search forward for conflict
        for {set t [expr {$eskil($top,curMerge) + 1}]} {$t <= $last} {incr t} {
            if {$eskil($top,mergeSelection,Conflict,$t)} {
                set delta [expr {$t - $eskil($top,curMerge)}]
                break
            }
        }
    }

    set eskil($top,curMerge) [expr {$eskil($top,curMerge) + $delta}]
    if {$eskil($top,curMerge) < 0} {set eskil($top,curMerge) 0}
    if {$eskil($top,curMerge) > $last} {
        set eskil($top,curMerge) $last
    }
    set eskil($top,curMergeSel) $eskil($top,mergeSelection,$eskil($top,curMerge))
    $w tag configure merge$eskil($top,curMerge) -foreground red
    showDiff $top $eskil($top,curMerge)
    seeText $w merges$eskil($top,curMerge) mergee$eskil($top,curMerge)

    set eskil($top,mergeStatus) \
            $eskil($top,mergeSelection,Comment,$eskil($top,curMerge))
}

# Select a merge setting for all diffs.
proc selectMergeAll {top new} {
    global eskil

    set end [expr {[llength $eskil($top,leftMergeData)] / 2}]
    for {set t 0} {$t < $end} {incr t} {
        selectMerge2 $top $t $new
    }
    set eskil($top,curMergeSel) $new
    set w $top.merge.t
    seeText $w merges$eskil($top,curMerge) mergee$eskil($top,curMerge)
}

# Change merge setting fo current diff.
proc selectMerge {top} {
    global eskil

    set w $top.merge.t
    selectMerge2 $top $eskil($top,curMerge) $eskil($top,curMergeSel)
    seeText $w merges$eskil($top,curMerge) mergee$eskil($top,curMerge)
}

# Change merge setting for a diff.
proc selectMerge2 {top no new} {
    global eskil

    set w $top.merge.t
    # Delete current string
    $w delete merges$no mergee$no

    set eskil($top,mergeSelection,$no) $new

    set i [expr {$no * 2 + 1}]
    set diffLeft [lindex $eskil($top,leftMergeData) $i]
    set diffRight [lindex $eskil($top,rightMergeData) $i]

    # Temporarily switch surrounding marks
    # Two steps are enough since there can't be consecutive empty areas
    # The one before and/or the one after the one being switch might
    # be empty.
    $w mark gravity mergee[expr {$no - 2}] left
    $w mark gravity mergee[expr {$no - 1}] left
    $w mark gravity merges[expr {$no + 1}] right
    $w mark gravity merges[expr {$no + 2}] right

    if {$eskil($top,mergeSelection,$no) == 12} {
        $w insert merges$no $diffLeft$diffRight merge$no
    } elseif {$eskil($top,mergeSelection,$no) == 21} {
        $w insert merges$no $diffRight$diffLeft merge$no
    } elseif {$eskil($top,mergeSelection,$no) == 1} {
        $w insert merges$no $diffLeft merge$no
    } elseif {$eskil($top,mergeSelection,$no) == 2} {
        $w insert merges$no $diffRight merge$no
    }
    # Switch back surrounding marks
    $w mark gravity mergee[expr {$no - 2}] right
    $w mark gravity mergee[expr {$no - 1}] right
    $w mark gravity merges[expr {$no + 1}] left
    $w mark gravity merges[expr {$no + 2}] left
}

# Save the merge result.
proc saveMerge {top} {
    set w $top.merge.t

    if {$::eskil($top,mergeFile) eq "" && $::eskil($top,mode) eq "conflict"} {
        set apa [tk_messageBox -parent $top.merge -icon question \
                -title "Save merge file" -type yesno -message \
                "Do you want to overwrite the original conflict file?"]
        if {$apa == "yes"} {
            set ::eskil($top,mergeFile) $::eskil($top,conflictFile)
        }
    }
    if {$::eskil($top,mergeFile) eq ""} {
        # Ask user which file
        set buttons {}
        set text "Overwrite file or Browse?"
        if {[file exists $::eskil($top,leftFile)] && \
                $::eskil($top,leftFile) eq $::eskil($top,leftLabel)} {
            lappend buttons Left
            append text "\nLeft: $::eskil($top,leftFile)"
        }
        if {[file exists $::eskil($top,rightFile)] && \
                $::eskil($top,rightFile) eq $::eskil($top,rightLabel)} {
            lappend buttons Right
            append text "\nRight: $::eskil($top,rightFile)"
        }
        lappend buttons Browse Cancel
        if {[llength $buttons] > 2} {
            set apa [tk_dialog .savemerge "Save merge file" \
                    $text \
                    questhead -1 {*}$buttons]
            if {$apa < 0} return
            set apa [lindex $buttons $apa]
            if {$apa eq "Left"} {
                set ::eskil($top,mergeFile) $::eskil($top,leftFile)
            } elseif {$apa eq "Right"} {
                set ::eskil($top,mergeFile) $::eskil($top,rightFile)
            } elseif {$apa eq "Cancel"} {
                return
            }
        }
        if {$::eskil($top,mergeFile) eq ""} {
            # Browse
            if {[info exists ::eskil($top,rightDir)]} {
                set initDir $::eskil($top,rightDir)
            } elseif {[info exists ::eskil($top,leftDir)]} {
                set initDir $::eskil($top,leftDir)
            } else {
                set initDir [pwd]
            }

            set apa [tk_getSaveFile -title "Save merge file" -initialdir $initDir \
                    -parent $top.merge]
            if {$apa eq ""} return
            set ::eskil($top,mergeFile) $apa
        }
    }

    set ch [open $::eskil($top,mergeFile) "w"]
    fconfigure $ch -translation $::eskil($top,mergetranslation)
    puts -nonewline $ch [$w get 1.0 end-1char]
    close $ch

    # Detect if this is a GIT merge, and possibly add it to the index
    # after save (i.e. git add file)
    if {[detectRevSystem $::eskil($top,mergeFile)] eq "GIT"} {
        set apa [tk_messageBox -parent $top.merge -icon info -type yesno \
                -title "Diff" \
                -message "Saved merge to file $::eskil($top,mergeFile).\nAdd\
                it to GIT index?"]
        if {$apa eq "yes"} {
            eskil::rev::GIT::add $::eskil($top,mergeFile)
        }
    } else {
        tk_messageBox -parent $top.merge -icon info -type ok -title "Diff" \
                -message "Saved merge to file $::eskil($top,mergeFile)."
    }
}

# Close merge window and clean up.
proc closeMerge {top} {
    global eskil

    destroy $top.merge
    set eskil($top,leftMergeData) {}
    set eskil($top,rightMergeData) {}
    array unset eskil $top,mergeSelection,*
}

# Create a window to display merge result.
proc makeMergeWin {top} {
    collectMergeData $top
    if {![info exists ::eskil($top,mergetranslation)]} {
        if {$::tcl_platform(platform) eq "windows"} {
            set ::eskil($top,mergetranslation) crlf
        } else {
            set ::eskil($top,mergetranslation) lf
        }
    }

    set w $top.merge
    if {![winfo exists $w]} {
        toplevel $w
    } else {
        destroy {*}[winfo children $w]
    }
    set anyC $::eskil($top,mergeSelection,AnyConflict)

    wm title $w "Merge result: [TitleTail $top]"

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
            -variable ::eskil($top,curMergeSel) -command "selectMerge $top"
    $w.m.ms add radiobutton -label "Left" -underline 0  -value 1  \
            -variable ::eskil($top,curMergeSel) -command "selectMerge $top"
    $w.m.ms add radiobutton -label "Right" -underline 0 -value 2  \
            -variable ::eskil($top,curMergeSel) -command "selectMerge $top"
    $w.m.ms add radiobutton -label "Right+Left"         -value 21 \
            -variable ::eskil($top,curMergeSel) -command "selectMerge $top"
    $w.m.ms add separator
    $w.m.ms add command -label "All Left"  -command "selectMergeAll $top 1"
    $w.m.ms add command -label "All Right" -command "selectMergeAll $top 2"

    $w.m add cascade -label "Goto" -underline 0 -menu $w.m.mg
    menu $w.m.mg
    $w.m.mg add command -accelerator "Up" -label "Previous" -command "nextMerge $top -1"
    $w.m.mg add command -accelerator "Down" -label "Next" -command "nextMerge $top 1"
    if {$anyC} {
        $w.m.mg add command -accelerator "Shift-Up" -label "Previous Conflict" -command "nextMerge $top -1000"
        $w.m.mg add command -accelerator "Shift-Down" -label "Next Conflict" -command "nextMerge $top 1000"
    } else {
        $w.m.mg add command -accelerator "Shift-Up" -label "Previous 10" -command "nextMerge $top -10"
        $w.m.mg add command -accelerator "Shift-Down" -label "Next 10" -command "nextMerge $top 10"
    }
    

    $w.m add cascade -label "Config" -underline 0 -menu $w.m.mc
    menu $w.m.mc
    $w.m.mc add radiobutton -label "Line end LF"   -value lf   -variable ::eskil($top,mergetranslation)
    $w.m.mc add radiobutton -label "Line end CRLF" -value crlf -variable ::eskil($top,mergetranslation)
    if {$::eskil($top,mode) eq "conflict"} {
        $w.m.mc add separator
        $w.m.mc add checkbutton -label "Pure" -variable ::eskil($top,modetype) \
                -onvalue "Pure" -offvalue "" -command {doDiff}
    }

    ttk::frame $w.f

    ttk::radiobutton $w.f.rb1 -text "LR" -value 12 \
            -variable ::eskil($top,curMergeSel) \
            -command "selectMerge $top"
    ttk::radiobutton $w.f.rb2 -text "L"  -value 1 \
            -variable ::eskil($top,curMergeSel) \
            -command "selectMerge $top"
    ttk::radiobutton $w.f.rb3 -text "R"  -value 2 \
            -variable ::eskil($top,curMergeSel) \
            -command "selectMerge $top"
    ttk::radiobutton $w.f.rb4 -text "RL" -value 21 \
            -variable ::eskil($top,curMergeSel) \
            -command "selectMerge $top"
    bind $w <Key-Left>  "focus $w; set ::eskil($top,curMergeSel) 1; selectMerge $top"
    bind $w <Key-Right> "focus $w; set ::eskil($top,curMergeSel) 2; selectMerge $top"

    ttk::button $w.f.bl -text "Prev C" -command "nextMerge $top -1000"
    ttk::button $w.f.br -text "Next C" -command "nextMerge $top 1000"
    
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
    if {!$anyC} {
        grid forget $w.f.bl $w.f.br
    }
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

    ttk::label $w.ls -textvariable ::eskil($top,mergeStatus)

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
    grid $w.ls  -      -sticky we
    grid columnconfigure $w 0 -weight 1
    grid rowconfigure $w 1 -weight 1

    fillMergeWindow $top
}

# Compare each file against an ancestor file for three-way merge
proc collectAncestorInfo {top dFile1 dFile2 opts} {
    if {![info exists ::eskil($top,mergetranslation)]} {
        # Try to autodetect line endings in ancestor file
        set ch [open $::eskil($top,ancestorFile) rb]
        set data [read $ch 10000]
        close $ch
        if {[string first \r\n $data] >= 0} {
            set ::eskil($top,mergetranslation) crlf
        } else {
            set ::eskil($top,mergetranslation) lf
        }
    }
    array unset ::eskil $top,ancestorLeft,*
    array unset ::eskil $top,ancestorRight,*
    set differrA1 [catch {DiffUtil::diffFiles {*}$opts \
            $::eskil($top,ancestorFile) $dFile1} diffresA1]
    set differrA2 [catch {DiffUtil::diffFiles {*}$opts \
            $::eskil($top,ancestorFile) $dFile2} diffresA2]
    if {$differrA1 != 0 || $differrA2 != 0} {
        puts $diffresA1
        puts $diffresA2
        return
    }
    foreach i $diffresA1 {
        lassign $i line1 n1 line2 n2
        if {$n1 == 0} {
            # Added lines
            for {set t $line2} {$t < $line2 + $n2} {incr t} {
                set ::eskil($top,ancestorLeft,$t) a
            }
        } elseif {$n2 == 0} {
            # Deleted lines
            # Mark the following line
            set ::eskil($top,ancestorLeft,d$line2) d
        } else {
            # Changed lines
            for {set t $line2} {$t < $line2 + $n2} {incr t} {
                set ::eskil($top,ancestorLeft,$t) c
            }
        }
    }            
    foreach i $diffresA2 {
        lassign $i line1 n1 line2 n2
        if {$n1 == 0} {
            # Added lines
            for {set t $line2} {$t < $line2 + $n2} {incr t} {
                set ::eskil($top,ancestorRight,$t) a
            }
        } elseif {$n2 == 0} {
            # Deleted lines
            # Mark the following line
            set ::eskil($top,ancestorRight,d$line2) d
        } else {
            # Changed lines
            for {set t $line2} {$t < $line2 + $n2} {incr t} {
                set ::eskil($top,ancestorRight,$t) c
            }
        }
    }
    #parray ::diff $top,ancestor*
}

# Use ancestor info to select which side to use in a merge chunk
##nagelfar syntax WhichSide x x x x x n n
proc WhichSide {top line1 n1 line2 n2 conflictName commentName} {
    upvar 1 $conflictName conflict $commentName comment
    set conflict 0
    set comment ""
    if {$::eskil($top,ancestorFile) eq ""} {
        # No ancestor info, just select right side
        return 2
    }
    if {$n1 == 0} {
        # Only to the right
        set delLeft [info exists ::eskil($top,ancestorLeft,d$line1)]
        # Inserted to right : Keep right side
        if {!$delLeft} {
            set comment "Right: Add"
            return 2
        }

        for {set t $line2} {$t < $line2 + $n2} {incr t} {
            if {[info exists ::eskil($top,ancestorRight,$t)]} {
                set right($::eskil($top,ancestorRight,$t)) 1
            }
        }
        # Deleted to left   : Keep left side
        if {[array size right] == 0} {
            set comment "Left: Delete"
            return 1
        }
        # Deleted to left and changed to the right : ?? (right for now)
        # FIXA
        set comment "*** Left: Delete, Right: Change"
        set conflict 1
        return 2
    } elseif {$n2 == 0} {
        # Only to the left, this can be:
        set delRight [info exists ::eskil($top,ancestorRight,d$line2)]
        # Inserted to left : Keep left side
        if {!$delRight} {
            set comment "Left: Add"
            return 1
        }

        for {set t $line1} {$t < $line1 + $n1} {incr t} {
            if {[info exists ::eskil($top,ancestorLeft,$t)]} {
                set left($::eskil($top,ancestorLeft,$t)) 1
            }
        }
        # Deleted to right : Keep right side
        if {[array size left] == 0} {
            set comment "Right: Delete"
            return 2
        }
        # Deleted to right and changed to the left : ?? (right for now)
        # FIXA
        set comment "*** Left: Change, Right: Delete"
        set conflict 1
        return 2
    } else {
        # Changed on both sides

        # Collect left side info
        for {set t $line1} {$t < $line1 + $n1} {incr t} {
            if {[info exists ::eskil($top,ancestorLeft,$t)]} {
                set left($::eskil($top,ancestorLeft,$t)) 1
            }
        }

        # No changes against ancestor on left side means it is just
        # changed to the right : Keep right
        if {[array size left] == 0} {
            set comment "Right: Change"
            return 2
        }

        # Collect right side info
        for {set t $line2} {$t < $line2 + $n2} {incr t} {
            if {[info exists ::eskil($top,ancestorRight,$t)]} {
                set right($::eskil($top,ancestorRight,$t)) 1
            }
        }

        # No changes against ancestor on right side means it is just
        # changed to the left : Keep left
        if {[array size right] == 0} {
            set comment "Left: Change"
            return 1
        }

        if {[info exists left(a)] && ![info exists left(c)] && \
                [info exists right(a)] && ![info exists right(c)]} {
            # Pure add on both sides, keep both
            set comment "*** Left: Add, Right: Add"
            set conflict 1
            return 12
        }
        # Changed in both, right for now
        # FIXA
        set comment "*** Left: Change, Right: Change"
        set conflict 1
        return 2
    }
}
