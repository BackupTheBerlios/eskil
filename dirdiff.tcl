#!/bin/sh
#
# Copyright (C) 1999-2003 Peter Spjuth
#
#-----------------------------------------------
# $Revision$
#-----------------------------------------------
# the next line restarts using wish \
exec wish "$0" "$@"

package require Tk 8.3

set debug 1
set thisScript [file join [pwd] [info script]]
set thisDir [file dirname $thisScript]

# Support for FreeWrap 5.5
if {[info procs ::freewrap::unpack] != ""} {
    set debug 0
    set thisDir [file dirname [info nameofexecutable]]
    set thisScript ""
    # Assume a wrapped diff too
    set tclDiffExe [list [file join $::thisDir diffw.exe]]
} else {
    if {[file type $thisScript] == "link"} {
        set tmplink [file readlink $thisScript]
        set thisDir [file dirname [file join $thisDir $tmplink]]
        unset tmplink
    }
    set tclDiffExe [list [info nameofexecutable] \
                            [file join $::thisDir diff.kit.tcl]]
}


if {$::tcl_platform(platform) == "windows"} {
    package require dde
}

if {$::tcl_platform(platform) == "unix"} {
    set util(editor) emacs
    set util(diffexe) diff
} else {
    set util(editor) wordpad
    foreach dir [lsort -decreasing -dictionary [glob c:/apps/emacs*]] {
        set em [file join $dir bin runemacs.exe]
        if {[file exists $em]} {
            set util(editor) $em
            break
        }
    }
    set util(diffexe) [file join $::thisDir diff.exe]
}

# Compare file names
proc FStrCmp {s1 s2} {
    # On Unix filenames are case sensitive
    if {$::tcl_platform(platform) == "unix"} {
	return [string compare $s1 $s2]
    }
    string compare -nocase $s1 $s2
}

# Sort file names
proc Fsort {l} {
    if {$::tcl_platform(platform) == "unix"} {
	return [lsort $l]
    }
    # Case insensitive on windows
    lsort -dictionary $l
}

# Compare two files
proc compareFiles {file1 file2} {
    global Pref
    file stat $file1 stat1
    file stat $file2 stat2

    if {[file isdirectory $file1] != [file isdirectory $file2]} {
	return 0
    }
    if {$stat1(size) == $stat2(size) && $Pref(comparelevel) == 0} {
        return 1
    }
    if {$stat1(size) == $stat2(size) && $stat1(mtime) == $stat2(mtime)} {
	return 1
    }
    if {$Pref(comparelevel) == 0} { # Do not check contents
        return 0
    }
    if {[file isdirectory $file1] || [file isdirectory $file2]} {
	return 0
    }

    switch $Pref(comparelevel) {
        1b -
        1 { # Check contents internally
            set bufsz 65536
            set eq 1
            set ch1 [open $file1 r]
            set ch2 [open $file2 r]
            if {$Pref(comparelevel) == "1b"} {
                fconfigure $ch1 -translation binary
                fconfigure $ch2 -translation binary
            }
            while {![eof $ch1] && ![eof $ch2]} {
                set f1 [read $ch1 $bufsz]
                set f2 [read $ch2 $bufsz]
                if {![string equal $f1 $f2]} {
                    set eq 0
                    break
                }
            }
            if {([eof $ch1] + [eof $ch2]) < 2} {
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
        5 { # Ignore RCS
            set eq [expr {![catch {exec $::util(diffexe) {--ignore-matching-lines=RCS: @(#) $Id} $file1 $file2} differr]}]
        }
    }
    return $eq
}

# infoFiles: 1= noLeft 2 = noRight 4=left is dir  8= right is dir 16=diff
proc listFiles {df1 df2 diff level} {
    global dirdiff Pref

    if {$Pref(nodir)} {
        if {$df1 != "" && [file isdirectory $df1] && \
                $df2 != "" && [file isdirectory $df2] } {
            return
        }
    }

    lappend dirdiff(leftFiles) $df1
    lappend dirdiff(rightFiles) $df2
    set info 16
    if {$df1 == ""} {
        incr info 1
    }
    if {$df2 == ""} {
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

    if {[file isdirectory $df1]} {
	append f1 /
        incr info 4
    }
    if {[file isdirectory $df2]} {
	append f2 /
        incr info 8
    }

    if {!$diff} {
	set tag2 ""
        incr info -16
    } elseif {$df1 == ""} {
	set tag2 new2
    } else {
        if {$info & 8} {
            set tag2 changed
        } else {
            set tag2 change
        }
    }
    if {$df2 == ""} {
	set tag1 new1
	$dirdiff(wRight) insert end \n
    } else {
        if {$info & 4} {
            set tag1 changed
        } else {
            set tag1 change
        }
	$dirdiff(wRight) insert end [format "%-30s %8d %16s\n" \
                $f2 [file size $df2] \
		[clock format [file mtime $df2] -format "%Y-%m-%d %H:%M"]] \
		$tag2
    }
    if {!$diff} {
	set tag1 ""
    }
    if {$df1 == ""} {
	$dirdiff(wLeft) insert end \n
    } else {
	$dirdiff(wLeft) insert end [format "%-30s %8d %16s\n" \
                $f1 [file size $df1] \
		[clock format [file mtime $df1] -format "%Y-%m-%d %H:%M"]] \
		$tag1
    }
    lappend dirdiff(infoFiles) $info
}

proc compareDirs {dir1 dir2 {level 0}} {
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
                set apa [expr {- [file isdirectory $df1] \
                               + [file isdirectory $df2]}]
            }
	    switch -- $apa {
		0 {
		    set diff [expr {![compareFiles $df1 $df2]}]
		    if {$diff || !$Pref(dir,onlydiffs)} { 
			listFiles $df1 $df2 $diff $level
		    }
		    if {[file isdirectory $df1] && [file isdirectory $df2] && \
			    $Pref(recursive) && [file tail $df1] != "CVS"} {
			compareDirs $df1 $df2 [expr {$level + 1}]
		    }
		    incr p1
		    incr p2
		}
		-1 {
		    listFiles $df1 "" 0 $level
		    incr p1
		} 
		1 {
		    listFiles "" $df2 0 $level
		    incr p2
		}
	    }
	} elseif {$p1 < $len1 && $p2 >= $len2} {
	    set f1 [lindex $files1 $p1]
	    listFiles [file join $dir1 $f1] "" 0 $level
	    incr p1
	} elseif {$p1 >= $len1 && $p2 < $len2} {
	    set f2 [lindex $files2 $p2]
	    listFiles "" [file join $dir2 $f2] 0 $level
	    incr p2
	} else {
	    break
	}
    }
}

proc doCompare {} {
    global dirdiff
    if {![file isdirectory $dirdiff(leftDir)]} return
    if {![file isdirectory $dirdiff(rightDir)]} return
    set dirdiff(leftFiles) {}
    set dirdiff(rightFiles) {}
    set dirdiff(infoFiles) {}
    $dirdiff(wLeft) delete 1.0 end
    $dirdiff(wRight) delete 1.0 end
    compareDirs $dirdiff(leftDir) $dirdiff(rightDir)
}

proc browseDir {dirVar} {
    global Pref
    upvar "#0" $dirVar dir

    set newdir $dir
    while {$newdir != "." && ![file isdirectory $newdir]} {
        set newdir [file dirname $newdir]
    }
    set newdir [tk_chooseDirectory -initialdir $newdir -title "Select Directory"]
    if {$newdir != ""} {
        set dir $newdir
    }
    if {$Pref(autocompare)} doCompare
}

proc selectFile {w x y} {
    global dirdiff Pref

    set row [expr {int([$w index @$x,$y]) - 1}]
    set lf [lindex $dirdiff(leftFiles) $row]
    set rf [lindex $dirdiff(rightFiles) $row]
    set i [lindex $dirdiff(infoFiles) $row]
    if {($i & 12) == 12} { # Both are dirs
        set dirdiff(leftDir) $lf
        set dirdiff(rightDir) $rf
        if {$Pref(autocompare)} doCompare
    } elseif {$i & 4} { # Left is dir
        set dirdiff(leftDir) $lf
        if {$Pref(autocompare)} doCompare
    } elseif {$i & 8} { # Right is dir
        set dirdiff(rightDir) $rf
        if {$Pref(autocompare)} doCompare
    } elseif {($i & 3) == 0} { # Both exists
        remoteDiff $lf $rf
    }
}

proc rightClick {w x y X Y} {
    global dirdiff Pref

    set row [expr {int([$w index @$x,$y]) - 1}]
    set lf [lindex $dirdiff(leftFiles) $row]
    set rf [lindex $dirdiff(rightFiles) $row]
    set i [lindex $dirdiff(infoFiles) $row]

    set m .dirdiff.m
    destroy $m
    menu $m -tearoff 0
    if {($i & 12) == 12} { # Both are dirs
        $m add command -label "Compare Directories" -command "
            [list set dirdiff(leftDir) $lf]
            [list set dirdiff(rightDir) $rf]
            [list if \$Pref(autocompare) "after idle doCompare"]
        "
    } elseif {$i & 4} { # Left is dir
        $m add command -label "Step down left directory" -command "
            [list set dirdiff(leftDir) $lf]
            [list if \$Pref(autocompare) "after idle doCompare"]
        "
    } elseif {$i & 8} { # Right is dir
        $m add command -label "Step down right directory" -command "
            [list set dirdiff(rightDir) $rf]
            [list if \$Pref(autocompare) "after idle doCompare"]
        "
    } elseif {($i & 3) == 0} { # Both exists
        $m add command -label "Compare Files" -command [list \
                remoteDiff $lf $rf]
    }
    if {$w == $dirdiff(wLeft) && ($i & 13) == 0} {
        $m add command -label "Copy File" \
                -command [list copyFile $row right]
        $m add command -label "Edit File" \
                -command [list editFile $row left]
    }
    if {$w == $dirdiff(wRight) && ($i & 14) == 0} {
        $m add command -label "Copy File" \
                -command [list copyFile $row left]
        $m add command -label "Edit File" \
                -command [list editFile $row right]
    }

    tk_popup $m $X $Y
}

proc copyFile {row to} {
    global dirdiff Pref

    if {$to == "left"} {
        set src [lindex $dirdiff(rightFiles) $row]
        set n [expr {[string length $dirdiff(rightDir)] + 1}]
        set dst [file join $dirdiff(leftDir) [string range $src $n end]]
    } elseif {$to == "right"} {
        set src [lindex $dirdiff(leftFiles) $row]
        set n [expr {[string length $dirdiff(leftDir)] + 1}]
        set dst [file join $dirdiff(rightDir) [string range $src $n end]]
    } else {
        error "Bad to argument to copyFile: $to"
    }

    if {[file exists $dst]} {
        if {[tk_messageBox -icon question -title "Overwrite file?" -message \
                "Copy\n$src\noverwriting\n$dst ?" -type yesno] == "yes"} {
            file copy -force $src $dst
        }
    } else {
        if {[tk_messageBox -icon question -title "Copy file?" -message \
                "Copy\n$src\nto\n$dst ?" -type yesno] == "yes"} {
            file copy $src $dst
        }
    }
}

proc editFile {row from} {
    global dirdiff Pref

    if {$from == "left"} {
        set src [file join $dirdiff(leftDir) [lindex $dirdiff(leftFiles) $row]]
    } elseif {$from == "right"} {
        set src [file join $dirdiff(rightDir) [lindex $dirdiff(rightFiles) $row]]
    } else {
        error "Bad from argument to editFile: $from"
    }

    exec $::util(editor) $src &
}

proc remoteDiff {file1 file2} {
    set cmd [list remoteDiff $file1 $file2]

    if {$::tcl_platform(platform) == "unix"} {
        # send -async Diff $cmd
        eval exec $::tclDiffExe -server \$file1 \$file2 &
    } else {
	if {[catch {dde eval -async Diff $cmd}]} {
	    catch {eval exec $::tclDiffExe -server &}
	    after 500
	    catch {dde eval -async Diff $cmd}
	}
    }
}

proc upDir {{n 0}} {
    global dirdiff Pref
    switch $n {
        0 {
            set dirdiff(leftDir) [file dirname $dirdiff(leftDir)]
            set dirdiff(rightDir) [file dirname $dirdiff(rightDir)]
            if {$Pref(autocompare)} doCompare
        } 
        1 {
            set dirdiff(leftDir) [file dirname $dirdiff(leftDir)]
            if {$Pref(autocompare)} doCompare
        }
        2 {
            set dirdiff(rightDir) [file dirname $dirdiff(rightDir)]
            if {$Pref(autocompare)} doCompare
        }
    }            
}

# Procedures for common y-scroll
proc commonYScroll_YView {sby args} {
    global yscroll
    foreach w $yscroll($sby) {
        eval [list $w yview] $args
    }
}

proc commonYScroll_YScroll {sby args} {
    eval [list $sby set] $args
    commonYScroll_YView $sby moveto [lindex $args 0]
}

# Set up a common yscrollbar for a few scrollable widgets
proc commonYScroll {sby args} {
    global yscroll

    $sby configure -command [list commonYScroll_YView $sby]
    foreach w $args {
        $w configure -yscrollcommand [list commonYScroll_YScroll $sby]
    }
    set yscroll($sby) $args
}

proc chFont {} {
    global Pref

    font configure myfont -size $Pref(fontsize) -family $Pref(fontfamily)
}

proc applyColor {} {
    global dirdiff Pref

    $dirdiff(wLeft) tag configure new1 -foreground $Pref(colornew1) \
            -background $Pref(bgnew1)
    $dirdiff(wLeft) tag configure change -foreground $Pref(colorchange) \
            -background $Pref(bgchange)
    $dirdiff(wLeft) tag configure changed -foreground $Pref(colorchange)
    $dirdiff(wRight) tag configure new2 -foreground $Pref(colornew2) \
            -background $Pref(bgnew2)
    $dirdiff(wRight) tag configure change -foreground $Pref(colorchange) \
            -background $Pref(bgchange)
    $dirdiff(wRight) tag configure changed -foreground $Pref(colorchange)
}

proc makeDirDiffWin {} {
    global Pref dirdiff

    set top .dirdiff
    if {[winfo exists $top] && [winfo toplevel $top] == $top} {
        eval destroy [winfo children $top]
    } else {
        toplevel $top
    }

    wm title $top "Directory Diff"
    wm protocol $top WM_DELETE_WINDOW exit

    frame $top.fm
    frame $top.fe1
    frame $top.fe2

    menubutton $top.mf -menu $top.mf.m -text "File" -underline 0
    menu $top.mf.m
    $top.mf.m add command -label "Quit" -underline 0 -command exit

    menubutton $top.mo -menu $top.mo.m -text "Preferences" -underline 0
    menu $top.mo.m
    $top.mo.m add checkbutton -variable Pref(recursive) -label "Recursive"
    $top.mo.m add cascade -label "Check" -menu $top.mo.mc
    $top.mo.m add checkbutton -variable Pref(dir,onlydiffs) -label "Diffs Only"
    $top.mo.m add checkbutton -variable Pref(nodir)    -label "No Directory"
    $top.mo.m add checkbutton -variable Pref(autocompare) -label "Auto Compare"

    menu $top.mo.mc
    $top.mo.mc add radiobutton -variable Pref(comparelevel) -value 0 \
            -label "Do not check contents"
    $top.mo.mc add radiobutton -variable Pref(comparelevel) -value 1 \
            -label "Internal compare"
    $top.mo.mc add radiobutton -variable Pref(comparelevel) -value 1b \
            -label "Internal compare (bin)"
    $top.mo.mc add radiobutton -variable Pref(comparelevel) -value 2 \
            -label "Use Diff"
    $top.mo.mc add radiobutton -variable Pref(comparelevel) -value 3 \
            -label "Diff, ignore blanks"
    $top.mo.mc add radiobutton -variable Pref(comparelevel) -value 4 \
            -label "Diff, ignore case"
    $top.mo.mc add radiobutton -variable Pref(comparelevel) -value 5 \
            -label "Diff, ignore RCS"
    pack $top.mf $top.mo -in $top.fm -side left
    if {$::debug} {
        menubutton $top.md -text "Debug" -menu $top.md.m -underline 0
        menu $top.md.m
        if {$::tcl_platform(platform) == "windows"} {
            $top.md.m add checkbutton -label "Console" -variable consolestate \
                    -onvalue show -offvalue hide -command {console $consolestate}
            $top.md.m add separator
        }
        $top.md.m add command -label "Reread Source" -command {source $thisScript}
        $top.md.m add separator
        $top.md.m add command -label "Redraw Window" -command {makeDirDiffWin}
        pack $top.md -in $top.fm -side left -padx 20
    }

    button $top.bc -text "Compare" -command doCompare -underline 0
    bind $top <Alt-c> "$top.bc invoke"
    button $top.bu -text "Up Both" -command upDir -underline 0
    bind $top <Alt-u> "$top.bu invoke"
    button $top.bu1 -text "Up" -command {upDir 1}
    button $top.bu2 -text "Up" -command {upDir 2}
    pack $top.bc $top.bu -in $top.fm -side right

    catch {font delete myfont}
    font create myfont -family $Pref(fontfamily) -size $Pref(fontsize)

    entry $top.e1 -textvariable dirdiff(leftDir)
    entry $top.e2 -textvariable dirdiff(rightDir)
    button $top.bb1 -text "Browse" -command {browseDir dirdiff(leftDir)}
    button $top.bb2 -text "Browse" -command {browseDir dirdiff(rightDir)}
    bind $top.e1 <Return> doCompare
    bind $top.e2 <Return> doCompare

    pack $top.bb1 $top.bu1 -in $top.fe1 -side right
    pack $top.e1 -in $top.fe1 -side left -fill x -expand 1
    pack $top.bb2 $top.bu2 -in $top.fe2 -side right
    pack $top.e2 -in $top.fe2 -side left -fill x -expand 1

    text $top.t1 -height 40 -width 60 -wrap none -font myfont \
	    -xscrollcommand "$top.sbx1 set"
    scrollbar $top.sby -orient vertical
    scrollbar $top.sbx1 -orient horizontal -command "$top.t1 xview"
    text $top.t2 -height 40 -width 60 -wrap none -font myfont \
	    -xscrollcommand "$top.sbx2 set"
    scrollbar $top.sbx2 -orient horizontal -command "$top.t2 xview"
    commonYScroll $top.sby $top.t1 $top.t2
    canvas $top.c -width 4

    bind $top.t1 <Double-Button-1> "after idle selectFile $top.t1 %x %y"
    bind $top.t2 <Double-Button-1> "after idle selectFile $top.t2 %x %y"
    bind $top.t1 <Button-3> "rightClick $top.t1 %x %y %X %Y"
    bind $top.t2 <Button-3> "rightClick $top.t2 %x %y %X %Y"

    set dirdiff(wLeft)  $top.t1
    set dirdiff(wRight) $top.t2
    set dirdiff(wY) $top.sby

    applyColor

    grid $top.fm   - - -   -     -sticky we
    grid $top.fe1  x  x    $top.fe2  -sticky we
    grid $top.t1   $top.c $top.sby $top.t2   -sticky news
    grid $top.sbx1 x  x    $top.sbx2 -sticky we

    grid rowconfigure    $top  2    -weight 1
    grid columnconfigure $top {0 3} -weight 1
}

proc getOptions {} {
    global Pref

    set Pref(fontsize) 9
    set Pref(fontfamily) courier
    set Pref(colorchange) red
    set Pref(colornew1) darkgreen
    set Pref(colornew2) blue
    set Pref(bgchange) gray
    set Pref(bgnew1) gray
    set Pref(bgnew2) gray
    set Pref(comparelevel) 1
    set Pref(recursive) 0
    set Pref(dir,onlydiffs) 0
    set Pref(nodir) 0
    set Pref(autocompare) 1

    if {[file exists "~/.dirdiffrc"]} {
        source "~/.dirdiffrc"
    }
}

proc parseCommandLine {} {
    global argc argv dirdiff Pref

    if {$argc == 2} {
        set dirdiff(leftDir) [file join [pwd] [lindex $argv 0]]
        set dirdiff(rightDir) [file join [pwd] [lindex $argv 1]]
    } elseif {$argc == 1} {
        set dirdiff(leftDir) [file join [pwd] [lindex $argv 0]]
        set dirdiff(rightDir) $dirdiff(leftDir)
    } else {
        set dirdiff(leftDir) [pwd]
        set dirdiff(rightDir) [pwd]
    }
}


if {![info exists gurkmeja]} {
    set gurkmeja 1
    getOptions
    parseCommandLine
    makeDirDiffWin
    wm withdraw .
    if {$dirdiff(leftDir) != "" && $dirdiff(rightDir) != "" && \
                $dirdiff(leftDir) != $dirdiff(rightDir)} {
        update idletasks
        #.e1 xview end
        #.e2 xview end
        doCompare
    }
}
