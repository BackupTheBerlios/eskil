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

set debug 0
set thisScript [file join [pwd] [info script]]
set thisDir [file dirname $thisScript]

# Support for FreeWrap 5.5
if {[info proc ::freewrap::unpack] != ""} {
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
                            [file join $::thisDir diff.tcl]]
}


if {$::tcl_platform(platform) == "windows"} {
    package require dde
}

if {$::tcl_platform(platform) == "unix"} {
    set editor emacs
    set diffExe diff
} else {
    set editor wordpad
    foreach dir [lsort -decreasing -dictionary [glob c:/apps/emacs*]] {
        set em [file join $dir bin runemacs.exe]
        if {[file exists $em]} {
            set editor $em
            break
        }
    }
    set diffExe [file join $::thisDir diff.exe]
}

# Compare file names
proc fstrcmp {s1 s2} {
    # On Unix filenames are case sensitive
    if {$::tcl_platform(platform) == "unix"} {
	return [string compare $s1 $s2]
    }
    string compare -nocase $s1 $s2
}

proc flsort {l} {
    if {$::tcl_platform(platform) == "unix"} {
	return [lsort $l]
    }
    lsort -dictionary $l
}

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
            set eq [expr {![catch {exec $::diffExe $file1 $file2}]}]
        }
        3 { # Ignore space
            set eq [expr {![catch {exec $::diffExe -w $file1 $file2}]}]
        }
        4 { # Ignore case
            set eq [expr {![catch {exec $::diffExe -i $file1 $file2}]}]
        }
        5 { # Ignore RCS
            set eq [expr {![catch {exec $::diffExe {--ignore-matching-lines=RCS: @(#) $Id} $file1 $file2} differr]}]
        }
    }
    return $eq
}

# infoFiles: 1= noLeft 2 = noRight 4=left is dir  8= right is dir 16=diff
proc listFiles {df1 df2 diff level} {
    global leftFiles rightFiles infoFiles

    if {$::Pref(nodir)} {
        if {$df1 != "" && [file isdirectory $df1] && \
                $df2 != "" && [file isdirectory $df2] } {
            return
        }
    }

    lappend leftFiles $df1
    lappend rightFiles $df2
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
	.t2 insert end \n
    } else {
        if {$info & 4} {
            set tag1 changed
        } else {
            set tag1 change
        }
	.t2 insert end [format "%-30s %8d %16s\n" $f2 [file size $df2] \
		[clock format [file mtime $df2] -format "%Y-%m-%d %H:%M"]] \
		$tag2
    }
    if {!$diff} {
	set tag1 ""
    }
    if {$df1 == ""} {
	.t1 insert end \n
    } else {
	.t1 insert end [format "%-30s %8d %16s\n" $f1 [file size $df1] \
		[clock format [file mtime $df1] -format "%Y-%m-%d %H:%M"]] \
		$tag1
    }
    lappend infoFiles $info
}

proc compareDirs {dir1 dir2 {level 0}} {
    global Pref
    set olddir [pwd]
    cd $dir1
    set files1 [flsort [glob -nocomplain * {.[a-zA-Z]*}]]
    cd $dir2
    set files2 [flsort [glob -nocomplain * {.[a-zA-Z]*}]]
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
            set apa [fstrcmp $f1 $f2]
            if {$apa == 0} {
                set apa [expr {- [file isdirectory $df1] \
                               + [file isdirectory $df2]}]
            }
	    switch -- $apa {
		0 {
		    set diff [expr {![compareFiles $df1 $df2]}]
		    if {$diff || !$Pref(diffonly)} { 
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
    global leftDir rightDir leftFiles rightFiles infoFiles
    if {![file isdirectory $leftDir]} return
    if {![file isdirectory $rightDir]} return
    set leftFiles {}
    set rightFiles {}
    set infoFiles {}
    .t1 delete 1.0 end
    .t2 delete 1.0 end
    compareDirs $leftDir $rightDir
}

proc browseDir {dirVar} {
    global Pref
    upvar #0 $dirVar dir

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
    global leftDir rightDir leftFiles rightFiles infoFiles Pref

    set row [expr {int([$w index @$x,$y]) - 1}]
    set lf [lindex $leftFiles $row]
    set rf [lindex $rightFiles $row]
    set i [lindex $infoFiles $row]
    if {($i & 12) == 12} { # Both are dirs
        set leftDir $lf
        set rightDir $rf
        if {$Pref(autocompare)} doCompare
    } elseif {$i & 4} { # Left is dir
        set leftDir $lf
        if {$Pref(autocompare)} doCompare
    } elseif {$i & 8} { # Right is dir
        set rightDir $rf
        if {$Pref(autocompare)} doCompare
    } elseif {($i & 3) == 0} { # Both exists
        remoteDiff $lf $rf
    }
}

proc rightClick {w x y X Y} {
    global leftDir rightDir leftFiles rightFiles infoFiles Pref

    set row [expr {int([$w index @$x,$y]) - 1}]
    set lf [lindex $leftFiles $row]
    set rf [lindex $rightFiles $row]
    set i [lindex $infoFiles $row]

    destroy .m
    menu .m -tearoff 0
    if {($i & 12) == 12} { # Both are dirs
        .m add command -label "Compare Directories" -command "
            [list set leftDir $lf]
            [list set rightDir $rf]
            [list if \$Pref(autocompare) "after idle doCompare"]
        "
    } elseif {$i & 4} { # Left is dir
        .m add command -label "Step down left directory" -command "
            [list set leftDir $lf]
            [list if \$Pref(autocompare) "after idle doCompare"]
        "
    } elseif {$i & 8} { # Right is dir
        .m add command -label "Step down right directory" -command "
            [list set rightDir $rf]
            [list if \$Pref(autocompare) "after idle doCompare"]
        "
    } elseif {($i & 3) == 0} { # Both exists
        .m add command -label "Compare Files" -command [list \
                remoteDiff $lf $rf]
    }
    if {$w == ".t1" && ($i & 13) == 0} {
        .m add command -label "Copy File" -command [list \
                copyFile $row right]
        .m add command -label "Edit File" -command [list \
                editFile $row left]
    }
    if {$w == ".t2" && ($i & 14) == 0} {
        .m add command -label "Copy File" -command [list \
                copyFile $row left]
        .m add command -label "Edit File" -command [list \
                editFile $row right]
    }

    tk_popup .m $X $Y
}

proc copyFile {row to} {
    global leftDir rightDir leftFiles rightFiles infoFiles Pref

    if {$to == "left"} {
        set src [lindex $rightFiles $row]
        set n [expr {[string length $rightDir] + 1}]
        set dst [file join $leftDir [string range $src $n end]]
    } elseif {$to == "right"} {
        set src [lindex $leftFiles $row]
        set n [expr {[string length $leftDir] + 1}]
        set dst [file join $rightDir [string range $src $n end]]
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
    global leftDir rightDir leftFiles rightFiles infoFiles Pref

    if {$from == "left"} {
        set src [file join $leftDir [lindex $leftFiles $row]]
    } elseif {$from == "right"} {
        set src [file join $rightDir [lindex $rightFiles $row]]
    } else {
        error "Bad from argument to editFile: $from"
    }

    exec $::editor $src &
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
    global leftDir rightDir Pref
    switch $n {
        0 {
            set leftDir [file dirname $leftDir]
            set rightDir [file dirname $rightDir]
            if {$Pref(autocompare)} doCompare
        } 
        1 {
            set leftDir [file dirname $leftDir]
            if {$Pref(autocompare)} doCompare
        }
        2 {
            set rightDir [file dirname $rightDir]
            if {$Pref(autocompare)} doCompare
        }
    }            
}

proc my_yview {args} {
    eval .t1 yview $args
    eval .t2 yview $args
}

proc my_yscroll {args} {
    eval .sby set $args
    my_yview moveto [lindex $args 0]
}

proc chFont {} {
    global Pref

    font configure myfont -size $Pref(fontsize) -family $Pref(fontfamily)
}

proc applyColor {} {
    global Pref

    .t1 tag configure new1 -foreground $Pref(colornew1) -background $Pref(bgnew1)
    .t1 tag configure change -foreground $Pref(colorchange) -background $Pref(bgchange)
    .t1 tag configure changed -foreground $Pref(colorchange)
    .t2 tag configure new2 -foreground $Pref(colornew2) -background $Pref(bgnew2)
    .t2 tag configure change -foreground $Pref(colorchange) -background $Pref(bgchange)
    .t2 tag configure changed -foreground $Pref(colorchange)
}

proc makeDirDiffWin {} {
    global Pref

    eval destroy [winfo children .]

    frame .fm
    frame .fe1
    frame .fe2

    menubutton .mo -menu .mo.m -text Preferences
    menu .mo.m
    .mo.m add checkbutton -variable Pref(recursive) -label Recursive
    .mo.m add cascade -label Check -menu .mo.mc
    .mo.m add checkbutton -variable Pref(diffonly) -label "Diffs Only"
    .mo.m add checkbutton -variable Pref(nodir)    -label "No Directory"
    .mo.m add checkbutton -variable Pref(autocompare) -label "Auto Compare"

    menu .mo.mc
    .mo.mc add radiobutton -variable Pref(comparelevel) -value 0 \
            -label "Do not check contents"
    .mo.mc add radiobutton -variable Pref(comparelevel) -value 1 \
            -label "Internal compare"
    .mo.mc add radiobutton -variable Pref(comparelevel) -value 1b \
            -label "Internal compare (bin)"
    .mo.mc add radiobutton -variable Pref(comparelevel) -value 2 \
            -label "Use Diff"
    .mo.mc add radiobutton -variable Pref(comparelevel) -value 3 \
            -label "Diff, ignore blanks"
    .mo.mc add radiobutton -variable Pref(comparelevel) -value 4 \
            -label "Diff, ignore case"
    .mo.mc add radiobutton -variable Pref(comparelevel) -value 5 \
            -label "Diff, ignore RCS"
    pack .mo -in .fm -side left
    if {$::debug} {
        menubutton .md -text Debug -menu .md.m -relief ridge
        menu .md.m
        if {$::tcl_platform(platform) == "windows"} {
            .md.m add checkbutton -label Console -variable consolestate \
                    -onvalue show -offvalue hide -command {console $consolestate}
            .md.m add separator
        }
        .md.m add command -label "Reread Source" -command {source $thisScript}
        .md.m add separator
        .md.m add command -label "Redraw Window" -command {makeDirDiffWin}
        pack .md -in .fm -side left
    }

    button .bc -text Compare -command doCompare
    button .bu -text Up -command upDir
    button .bu1 -text Up -command {upDir 1}
    button .bu2 -text Up -command {upDir 2}
    pack .bc .bu -in .fm -side right

    catch {font delete myfont}
    font create myfont -family $Pref(fontfamily) -size $Pref(fontsize)

    entry .e1 -textvariable leftDir
    entry .e2 -textvariable rightDir
    button .bb1 -text Browse -command {browseDir leftDir}
    button .bb2 -text Browse -command {browseDir rightDir}
    bind .e1 <Return> doCompare
    bind .e2 <Return> doCompare

    pack .bb1 .bu1 -in .fe1 -side right
    pack .e1 -in .fe1 -side left -fill x -expand 1
    pack .bb2 .bu2 -in .fe2 -side right
    pack .e2 -in .fe2 -side left -fill x -expand 1

    text .t1 -height 40 -width 60 -wrap none -yscrollcommand my_yscroll \
	    -xscrollcommand ".sbx1 set" -font myfont
    scrollbar .sby -orient vertical -command "my_yview"
    scrollbar .sbx1 -orient horizontal -command ".t1 xview"
    text .t2 -height 40 -width 60 -wrap none -yscrollcommand my_yscroll \
	    -xscrollcommand ".sbx2 set" -font myfont
    scrollbar .sbx2 -orient horizontal -command ".t2 xview"
    canvas .c -width 4

    bind .t1 <Double-Button-1> "after idle selectFile .t1 %x %y"
    bind .t2 <Double-Button-1> "after idle selectFile .t2 %x %y"
    bind .t1 <Button-3> "rightClick .t1 %x %y %X %Y"
    bind .t2 <Button-3> "rightClick .t2 %x %y %X %Y"

    applyColor

    grid .fm   - - -   -     -sticky we
    grid .fe1  x  x    .fe2  -sticky we
    grid .t1   .c .sby .t2   -sticky news
    grid .sbx1 x  x    .sbx2 -sticky we

    grid rowconfigure    . 2 -weight 1
    grid columnconfigure . {0 3} -weight 1
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
    set Pref(diffonly) 0
    set Pref(nodir) 0
    set Pref(autocompare) 1

    if {[file exists "~/.dirdiffrc"]} {
        source "~/.dirdiffrc"
    }
}

proc parseCommandLine {} {
    global argc argv leftDir rightDir Pref

    if {$argc == 2} {
        set leftDir [file join [pwd] [lindex $argv 0]]
        set rightDir [file join [pwd] [lindex $argv 1]]
    } elseif {$argc == 1} {
        set leftDir [file join [pwd] [lindex $argv 0]]
        set rightDir $leftDir
    } else {
        set leftDir [pwd]
        set rightDir [pwd]
    }
}

if {![winfo exists .fm]} {
    getOptions
    parseCommandLine
    makeDirDiffWin
    if {$leftDir != "" && $rightDir != "" && $leftDir != $rightDir} {
        update idletasks
        .e1 xview end
        .e2 xview end
        doCompare
    }
}
