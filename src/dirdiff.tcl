#----------------------------------------------------------------------
#  Eskil, Directory diff section
#
#  Copyright (c) 1998-2005, Peter Spjuth  (peter.spjuth@space.se)
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

# Compare file names
proc FStrCmp {s1 s2} {
    # On Unix filenames are case sensitive
    if {$::tcl_platform(platform) eq "unix"} {
	return [string compare $s1 $s2]
    }
    string compare -nocase $s1 $s2
}

# Sort file names
proc Fsort {l} {
    if {$::tcl_platform(platform) eq "unix"} {
	return [lsort $l]
    }
    # Case insensitive on windows
    lsort -dictionary $l
}

# Compare two files
proc CompareFiles {file1 file2} {
    global Pref
    if {[catch {file stat $file1 stat1}]} {
        return 0
    }
    if {[catch {file stat $file2 stat2}]} {
        return 0
    }

    # Same type?
    if {[FileIsDirectory $file1] != [FileIsDirectory $file2]} {
	return 0
    }
    # If contents is not checked, same size is enough to be equal
    if {$stat1(size) == $stat2(size) && $Pref(comparelevel) == 0} {
        return 1
    }
    # Different size is enough when doing binary compare
    if {$stat1(size) != $stat2(size) && $Pref(comparelevel) eq "1b"} {
        return 0
    }
    # Same size and time is always considered equal
    if {$stat1(size) == $stat2(size) && $stat1(mtime) == $stat2(mtime)} {
	return 1
    }
    # Don't check further if contents should not be checked
    if {$Pref(comparelevel) == 0} {
        return 0
    }
    # Don't check further if any is a directory
    if {[FileIsDirectory $file1] || [FileIsDirectory $file2]} {
	return 0
    }

    set ignorekey $Pref(dir,ignorekey)
    switch $Pref(comparelevel) {
        1b -
        1 { # Check contents internally
            set bufsz 65536
            set eq 1
            set ch1 [open $file1 r]
            set ch2 [open $file2 r]
            if {$Pref(comparelevel) eq "1b"} {
                fconfigure $ch1 -translation binary
                fconfigure $ch2 -translation binary
            }
            if {$ignorekey} {
                # Assume that all keywords are in the first block
                set f1 [read $ch1 $bufsz]
                set f2 [read $ch2 $bufsz]
                regsub -all {\$\w+:[^\$]*\$} $f1 {} f1
                regsub -all {\$\w+:[^\$]*\$} $f2 {} f2
                # Compensate for any change in length
                if {[string length $f1] < [string length $f2]} {
                    append f1 [read $ch1 [expr {[string length $f2] - [string length $f1]}]]
                }
                if {[string length $f2] < [string length $f1]} {
                    append f2 [read $ch2 [expr {[string length $f1] - [string length $f2]}]]
                }
                if {![string equal $f1 $f2]} {
                    set eq 0
                }
            }
            while {$eq && ![eof $ch1] && ![eof $ch2]} {
                set f1 [read $ch1 $bufsz]
                set f2 [read $ch2 $bufsz]
                if {![string equal $f1 $f2]} {
                    set eq 0
                }
            }
            if {![eof $ch1] || ![eof $ch2]} {
                set eq 0
            }
            close $ch1
            close $ch2
        }
        2 { # Simple external diff
            set eq [expr {![catch {exec $::util(diffexe) $file1 $file2}]}]
        }
        3 { # Ignore space
            set eq [expr {![catch {exec $::util(diffexe) -w $file1 $file2}]}]
        }
        4 { # Ignore case
            set eq [expr {![catch {exec $::util(diffexe) -i $file1 $file2}]}]
        }
    }
    return $eq
}

# Display two files in the directory windows and set up info for
# interacting with them.
# diff: Do they differ.
# level: Depth in a recursive run.
# The values in infoFiles are:
# 1 = noLeft, 2 = noRight, 4 = left is dir, 8 = right is dir, 16 = diff
proc ListFiles {df1 df2 diff level} {
    global dirdiff Pref

    # Optionally do not list directories.
    if {$Pref(nodir)} {
        if {$df1 != "" && [FileIsDirectory $df1] && \
                $df2 != "" && [FileIsDirectory $df2] } {
            return
        }
    }

    lappend dirdiff(leftFiles) $df1
    lappend dirdiff(rightFiles) $df2
    set info [expr {$diff? 16 : 0}]
    if {$df1 eq ""} {
        incr info 1
    }
    if {$df2 eq ""} {
        incr info 2
    }
    if {$df1 != ""} {
	set f1 [file split $df1]
	set i [expr {[llength $f1] - $level - 1}]
	set f1 [eval file join [lrange $f1 $i end]]
    }
    if {$df2 != ""} {
	set f2 [file split $df2]
	set i [expr {[llength $f2] - $level - 1}]
	set f2 [eval file join [lrange $f2 $i end]]
    }

    if {[FileIsDirectory $df1]} {
	append f1 /
        incr info 4
    }
    if {[FileIsDirectory $df2]} {
	append f2 /
        incr info 8
    }

    set maptag [expr {$diff ? "change" : ""}]
    if {$df1 eq ""} {
	set tag2 new2
        set maptag new2
    } elseif {!$diff} {
	set tag2 ""
    } else {
        if {$info & 8} {
            set tag2 changed
            set maptag ""
        } else {
            set tag2 change
        }
    }

    if {$df2 eq ""} {
	set tag1 new1
        set maptag new1
    } elseif {!$diff} {
	set tag1 ""
    } else {
        if {$info & 4} {
            set tag1 changed
            set maptag ""
        } else {
            set tag1 change
        }
    }
    addChange .dirdiff 1 $maptag 0 0 0 0

    if {$df2 eq ""} {
	$dirdiff(wRight) insert end \n
    } else {
        if {[catch {set size [file size $df2]}]} {
            set size -1
            set mtime 0
            lappend tag2 invalid
        } else {
            set mtime [file mtime $df2]
        }
	$dirdiff(wRight) insert end [format "%-30s %8d %16s\n" \
                $f2 $size \
		[clock format $mtime -format "%Y-%m-%d %H:%M"]] \
		$tag2
    }
    if {$df1 eq ""} {
	$dirdiff(wLeft) insert end \n
    } else {
        if {[catch {set size [file size $df1]}]} {
            set size -1
            set mtime 0
            lappend tag1 invalid
        } else {
            set mtime [file mtime $df1]
        }
	$dirdiff(wLeft) insert end [format "%-30s %8d %16s\n" \
                $f1 $size \
		[clock format $mtime -format "%Y-%m-%d %H:%M"]] \
		$tag1
    }
    lappend dirdiff(infoFiles) $info
}

# Compare two directories.
proc CompareDirs {dir1 dir2 {level 0}} {
    global Pref
    set olddir [pwd]
    cd $dir1
    set files1 [Fsort [glob -nocomplain * {.[a-zA-Z]*}]]
    cd $dir2
    set files2 [Fsort [glob -nocomplain * {.[a-zA-Z]*}]]
    cd $olddir

    set len1 [llength $files1]
    set len2 [llength $files2]

    set p1 0
    set p2 0
    while 1 {
	if {$p1 < $len1 && $p2 < $len2} {
	    set f1 [lindex $files1 $p1]
	    set df1 [file join $dir1 $f1]
	    set f2 [lindex $files2 $p2]
	    set df2 [file join $dir2 $f2]
            set apa [FStrCmp $f1 $f2]
            if {$apa == 0} {
                set apa [expr {- [FileIsDirectory $df1] \
                               + [FileIsDirectory $df2]}]
            }
	    switch -- $apa {
		0 {
		    set diff [expr {![CompareFiles $df1 $df2]}]
		    if {$diff || !$Pref(dir,onlydiffs)} {
			ListFiles $df1 $df2 $diff $level
		    }
		    if {[FileIsDirectory $df1] && [FileIsDirectory $df2] && \
			    $Pref(recursive) && [file tail $df1] != "CVS"} {
			CompareDirs $df1 $df2 [expr {$level + 1}]
		    }
		    incr p1
		    incr p2
		}
		-1 {
		    ListFiles $df1 "" 0 $level
		    incr p1
		}
		1 {
		    ListFiles "" $df2 0 $level
		    incr p2
		}
	    }
	} elseif {$p1 < $len1 && $p2 >= $len2} {
	    set f1 [lindex $files1 $p1]
	    ListFiles [file join $dir1 $f1] "" 0 $level
	    incr p1
	} elseif {$p1 >= $len1 && $p2 < $len2} {
	    set f2 [lindex $files2 $p2]
	    ListFiles "" [file join $dir2 $f2] 0 $level
	    incr p2
	} else {
	    break
	}
    }
}

# Run the directory comparison
proc doDirCompare {} {
    global dirdiff
    if {![FileIsDirectory $dirdiff(leftDir)]} return
    if {![FileIsDirectory $dirdiff(rightDir)]} return
    set dirdiff(leftFiles)  {}
    set dirdiff(rightFiles) {}
    set dirdiff(infoFiles)  {}
    set dirdiff(leftMark)   ""
    set dirdiff(rightMark)  ""

    set tail1 [file tail $dirdiff(leftDir)]
    set tail2 [file tail $dirdiff(rightDir)]
    if {$tail1 eq $tail2} {
        wm title .dirdiff "Eskil Dir: $tail1"
    } else {
        wm title .dirdiff "Eskil Dir: $tail1 vs $tail2"
    }

    $dirdiff(wLeft) delete 1.0 end
    $dirdiff(wRight) delete 1.0 end
    set top .dirdiff
    busyCursor $top
    clearMap $top
    update idletasks
    CompareDirs $dirdiff(leftDir) $dirdiff(rightDir)
    normalCursor .dirdiff
    drawMap $top -1
}

# Pick a directory for compare
proc BrowseDir {dirVar entryW} {
    global Pref
    upvar "#0" $dirVar dir

    set newdir $dir
    while {$newdir != "." && ![FileIsDirectory $newdir]} {
        set newdir [file dirname $newdir]
    }
    set newdir [tk_chooseDirectory -initialdir $newdir -title "Select Directory"]
    if {$newdir != ""} {
        set dir $newdir
        $entryW xview end
    }
    if {$Pref(autocompare)} doDirCompare
}

# This is called when double clicking on a file in
# the directory compare window
proc SelectFile {w x y} {
    global dirdiff Pref

    set row [expr {int([$w index @$x,$y]) - 1}]
    set lf [lindex $dirdiff(leftFiles) $row]
    set rf [lindex $dirdiff(rightFiles) $row]
    set i [lindex $dirdiff(infoFiles) $row]
    if {($i & 12) == 12} { # Both are dirs
        set dirdiff(leftDir) $lf
        set dirdiff(rightDir) $rf
        .dirdiff.e1 xview end
        .dirdiff.e2 xview end
        if {$Pref(autocompare)} doDirCompare
    } elseif {$i & 4} { # Left is dir
        set dirdiff(leftDir) $lf
        .dirdiff.e1 xview end
        if {$Pref(autocompare)} doDirCompare
    } elseif {$i & 8} { # Right is dir
        set dirdiff(rightDir) $rf
        .dirdiff.e2 xview end
        if {$Pref(autocompare)} doDirCompare
    } elseif {($i & 3) == 0} { # Both exists
        # Open a diff window for them
        newDiff $lf $rf
    }
}

# Bring up a context menu on a file.
proc DirRightClick {w x y X Y} {
    global dirdiff Pref

    set row [expr {int([$w index @$x,$y]) - 1}]
    set lf [lindex $dirdiff(leftFiles) $row]
    set rf [lindex $dirdiff(rightFiles) $row]
    set i [lindex $dirdiff(infoFiles) $row]

    set m .dirdiff.popup
    destroy $m
    menu $m
    if {($i & 12) == 12} { # Both are dirs
        $m add command -label "Compare Directories" -command "
            [list set dirdiff(leftDir) $lf]
            [list set dirdiff(rightDir) $rf]
            [list if \$Pref(autocompare) "after idle doDirCompare"]
        "
    }
    if {$i & 4 && $w eq $dirdiff(wLeft)} { # Left is dir
        $m add command -label "Step down left directory" -command "
            [list set dirdiff(leftDir) $lf]
            [list if \$Pref(autocompare) "after idle doDirCompare"]
        "
    }
    if {$i & 8 && $w eq $dirdiff(wRight)} { # Right is dir
        $m add command -label "Step down right directory" -command "
            [list set dirdiff(rightDir) $rf]
            [list if \$Pref(autocompare) "after idle doDirCompare"]
        "
    }
    if {($i & 12) == 0 && ($i & 3) == 0} { # Neither is dir, Both exists
        $m add command -label "Compare Files" -command [list \
                newDiff $lf $rf]
    }
    if {$w eq $dirdiff(wLeft) && ($i & 13) == 0} {
        $m add command -label "Copy File" \
                -command [list CopyFile $row right]
        $m add command -label "Edit File" \
                -command [list EditFile $row left]
        $m add command -label "Mark File" \
                -command [list set ::dirdiff(leftMark) $lf]
	if {$::dirdiff(rightMark) != ""} {
	    $m add command -label "Compare with $::dirdiff(rightMark)" \
		    -command [list newDiff $lf $::dirdiff(rightMark)]
	}
    }
    if {$w eq $dirdiff(wRight) && ($i & 14) == 0} {
        $m add command -label "Copy File" \
                -command [list CopyFile $row left]
        $m add command -label "Edit File" \
                -command [list EditFile $row right]
        $m add command -label "Mark File" \
                -command [list set ::dirdiff(rightMark) $rf]
	if {$::dirdiff(leftMark) != ""} {
	    $m add command -label "Compare with $::dirdiff(leftMark)" \
		    -command [list newDiff $::dirdiff(leftMark) $rf]
	}
    }

    tk_popup $m $X $Y
}

# Copy a file from one directory to the other
proc CopyFile {row to} {
    global dirdiff Pref

    if {$to eq "left"} {
        set src [lindex $dirdiff(rightFiles) $row]
        set n [expr {[string length $dirdiff(rightDir)] + 1}]
        set dst [file join $dirdiff(leftDir) [string range $src $n end]]
    } elseif {$to eq "right"} {
        set src [lindex $dirdiff(leftFiles) $row]
        set n [expr {[string length $dirdiff(leftDir)] + 1}]
        set dst [file join $dirdiff(rightDir) [string range $src $n end]]
    } else {
        error "Bad to argument to CopyFile: $to"
    }

    if {[file exists $dst]} {
        if {[tk_messageBox -icon question -title "Overwrite file?" -message \
                "Copy\n$src\noverwriting\n$dst ?" -type yesno] eq "yes"} {
            file copy -force $src $dst
        }
    } else {
        if {[tk_messageBox -icon question -title "Copy file?" -message \
                "Copy\n$src\nto\n$dst ?" -type yesno] eq "yes"} {
            file copy $src $dst
        }
    }
}

# Bring up an editor to display a file.
proc EditFile {row from} {
    global dirdiff Pref

    if {$from eq "left"} {
        set src [file join $dirdiff(leftDir) [lindex $dirdiff(leftFiles) $row]]
    } elseif {$from eq "right"} {
        set src [file join $dirdiff(rightDir) [lindex $dirdiff(rightFiles) $row]]
    } else {
        error "Bad from argument to EditFile: $from"
    }

    locateEditor ::util(editor)
    exec $::util(editor) $src &
}

# Go up one level in directory hierarchy.
# 0 = both
proc UpDir {{n 0}} {
    global dirdiff Pref
    switch $n {
        0 {
            set dirdiff(leftDir) [file dirname $dirdiff(leftDir)]
            set dirdiff(rightDir) [file dirname $dirdiff(rightDir)]
            .dirdiff.e1 xview end
            .dirdiff.e2 xview end
            if {$Pref(autocompare)} doDirCompare
        }
        1 {
            set dirdiff(leftDir) [file dirname $dirdiff(leftDir)]
            .dirdiff.e1 xview end
            if {$Pref(autocompare)} doDirCompare
        }
        2 {
            set dirdiff(rightDir) [file dirname $dirdiff(rightDir)]
            .dirdiff.e2 xview end
            if {$Pref(autocompare)} doDirCompare
        }
    }
}

# Create directory diff window.
proc makeDirDiffWin {{redraw 0}} {
    global Pref dirdiff

    set top .dirdiff
    if {[winfo exists $top] && [winfo toplevel $top] eq $top} {
        if {$redraw} {
            eval destroy [winfo children $top]
        } else {
            raise $top
            focus -force $top
            return
        }
    } else {
        destroy $top
        toplevel $top
        lappend ::diff(diffWindows) $top
    }

    wm title $top "Eskil Dir"
    wm protocol $top WM_DELETE_WINDOW [list cleanupAndExit $top]

    frame $top.fm
    frame $top.fe1
    frame $top.fe2

    menu $top.m
    $top configure -menu $top.m

    $top.m add cascade -menu $top.m.mf -label "File" -underline 0
    menu $top.m.mf
    $top.m.mf add command -label "Compare" -underline 1 \
            -command doDirCompare
    $top.m.mf add separator
    $top.m.mf add command -label "Close" -underline 0 \
            -command [list cleanupAndExit $top]
    $top.m.mf add separator
    $top.m.mf add command -label "Quit" -underline 0 \
            -command [list cleanupAndExit all]

    $top.m add cascade -menu $top.m.mo -label "Preferences" -underline 0
    menu $top.m.mo
    $top.m.mo add checkbutton -variable Pref(recursive) -label "Recursive"
    $top.m.mo add cascade -label "Check" -menu $top.m.mo.mc
    $top.m.mo add checkbutton -variable Pref(dir,onlydiffs) -label "Diffs Only"
    $top.m.mo add checkbutton -variable Pref(nodir)    -label "No Directory"
    $top.m.mo add checkbutton -variable Pref(autocompare) -label "Auto Compare"

    menu $top.m.mo.mc
    $top.m.mo.mc add radiobutton -variable Pref(comparelevel) -value 0 \
            -label "Do not check contents"
    $top.m.mo.mc add radiobutton -variable Pref(comparelevel) -value 1 \
            -label "Internal compare"
    $top.m.mo.mc add radiobutton -variable Pref(comparelevel) -value 1b \
            -label "Internal compare (bin)"
    $top.m.mo.mc add radiobutton -variable Pref(comparelevel) -value 2 \
            -label "Use Diff"
    $top.m.mo.mc add radiobutton -variable Pref(comparelevel) -value 3 \
            -label "Diff, ignore blanks"
    $top.m.mo.mc add radiobutton -variable Pref(comparelevel) -value 4 \
            -label "Diff, ignore case"
    $top.m.mo.mc add checkbutton -variable Pref(dir,ignorekey) \
            -label "Ignore \$Keyword:\$"

    $top.m add cascade -label "Tools" -underline 0 -menu $top.m.mt
    menu $top.m.mt
    $top.m.mt add command -label "New Diff Window" -underline 0 \
            -command makeDiffWin
    $top.m.mt add command -label "Clip Diff" -underline 0 \
            -command makeClipDiffWin
    if {$::tcl_platform(platform) eq "windows"} {
        if {![catch {package require registry}]} {
            $top.m.mt add separator
            $top.m.mt add command -label "Setup Registry" -underline 6 \
                    -command makeRegistryWin
        }
    }

    $top.m add cascade -label "Help" -underline 0 -menu $top.m.help
    menu $top.m.help
    $top.m.help add command -label "Tutorial" -command makeTutorialWin \
            -underline 0
    $top.m.help add command -label "About" -command makeAboutWin -underline 0

    if {$::debug} {
        $top.m add cascade -label "Debug" -menu $top.m.md -underline 0
        menu $top.m.md
        if {$::tcl_platform(platform) eq "windows"} {
            $top.m.md add checkbutton -label "Console" -variable consolestate \
                    -onvalue show -offvalue hide -command {console $consolestate}
            $top.m.md add separator
        }
        $top.m.md add command -label "Reread Source" -underline 0 \
                -command {EskilRereadSource}
        $top.m.md add separator
        $top.m.md add command -label "Redraw Window" -command {makeDirDiffWin 1}
    }

    button $top.bu -text "Up Both" -command UpDir -underline 0 -padx 10
    bind $top <Alt-u> "$top.bu invoke"
    button $top.bc -text "Compare" -command doDirCompare -underline 0 -padx 10
    bind $top <Alt-c> "$top.bc invoke"
    pack $top.bc $top.bu -in $top.fm -side right
    pack $top.bu -padx 6

    catch {font delete myfont}
    font create myfont -family $Pref(fontfamily) -size $Pref(fontsize)

    entry $top.e1 -textvariable dirdiff(leftDir)
    button $top.bu1 -text "Up" -padx 10 -command {UpDir 1}
    button $top.bb1 -text "Browse"  -padx 10 \
            -command [list BrowseDir dirdiff(leftDir) $top.e1]
    $top.e1 xview end
    entry $top.e2 -textvariable dirdiff(rightDir)
    button $top.bu2 -text "Up" -padx 10 -command {UpDir 2}
    button $top.bb2 -text "Browse" -padx 10 \
            -command [list BrowseDir dirdiff(rightDir) $top.e2]
    $top.e2 xview end
    bind $top.e1 <Return> doDirCompare
    bind $top.e2 <Return> doDirCompare

    pack $top.bb1 $top.bu1 -in $top.fe1 -side right -pady 1
    pack $top.bu1 -padx 6
    pack $top.e1 -in $top.fe1 -side left -fill x -expand 1
    pack $top.bb2 $top.bu2 -in $top.fe2 -side right -pady 1
    pack $top.bu2 -padx 6
    pack $top.e2 -in $top.fe2 -side left -fill x -expand 1

    text $top.t1 -height 40 -width 60 -wrap none -font myfont \
	    -xscrollcommand "$top.sbx1 set" -takefocus 0
    scrollbar $top.sby -orient vertical
    scrollbar $top.sbx1 -orient horizontal -command "$top.t1 xview"
    text $top.t2 -height 40 -width 60 -wrap none -font myfont \
	    -xscrollcommand "$top.sbx2 set" -takefocus 0
    scrollbar $top.sbx2 -orient horizontal -command "$top.t2 xview"
    commonYScroll $top.sby $top.t1 $top.t2
    set map [createMap $top]

    bind $top.t1 <Double-Button-1> "after idle SelectFile $top.t1 %x %y"
    bind $top.t2 <Double-Button-1> "after idle SelectFile $top.t2 %x %y"
    bind $top.t1 <Button-3> "DirRightClick $top.t1 %x %y %X %Y"
    bind $top.t2 <Button-3> "DirRightClick $top.t2 %x %y %X %Y"

    set dirdiff(wLeft)  $top.t1
    set dirdiff(wRight) $top.t2
    set dirdiff(wY) $top.sby
    # Interact better with diff by setting these
    set ::widgets($top,wDiff1) $top.t1
    set ::widgets($top,wDiff2) $top.t2

    applyColor

    grid $top.fm   -    -    -   -     -sticky we
    grid $top.fe1  x    x    $top.fe2  -sticky we
    grid $top.t1   $map $top.sby $top.t2   -sticky news
    grid $top.sbx1 x    x    $top.sbx2 -sticky we

    grid $map -pady [expr {[$top.sby cget -width] + 2}]

    grid rowconfigure    $top  2    -weight 1
    grid columnconfigure $top {0 3} -weight 1
}

# Experimental...
proc makeRegSubWin {} {
    set top .ddregsub
    if {[winfo exists $top] && [winfo toplevel $top] eq $top} {
        raise $top
        focus -force $top
        return
    } else {
        destroy $top
        toplevel $top
    }

    wm title $top "Eskil Dir Preprocess"

    entry $top.e1 -textvariable ::dirdiff(pattern) -width 15
    entry $top.e2 -textvariable ::dirdiff(replace) -width 15

    label $top.l1 -text "Pattern" -anchor w
    label $top.l2 -text "Subst"   -anchor w
    
    grid $top.l1 $top.e1 -sticky we
    grid $top.l2 $top.e2 -sticky we
    grid columnconfigure $top 1 -weight 1
    grid rowconfigure    $top 2 -weight 1
    
}
