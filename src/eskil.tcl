#!/bin/sh
#
#   diff.tcl
#
#   Purpose
#             Graphical frontend to diff
#
#   Usage
#             diff.tcl [diff options] [file1] [file2]
#
#             [diff options]         Options passed to diff. 
#             [file1],[file2]        Files to be compared
#                                    If no files are given, the program is
#                                    started anyway and you can select files
#                                    from within.
#                                    If only one file is given, the program
#                                    looks for an RCS directory next to the
#                                    file, and if found, runs rcsdiff.
#
#   Author    Peter Spjuth  980612
#
#   Revised   By       Date     Remark
#
#     1.0     DC-PS    980612   New Version.
#
#-----------------------------------------------
# the next line restarts using wish \
exec wish "$0" "$@"

proc myform {line text} {
    return [format "%3d: %s\n" $line $text]
}

proc myforml {line} {
    return [format "%3d: " $line]
}

proc comparelines {line1 line2 res1var res2var} {
    upvar $res1var res1
    upvar $res2var res2

    #Delete white space in both ends
    set apa1 [string trimleft $line1]
    set left1 [string range $line1 0 [expr [string length $line1] - [string length $apa1] - 1]]
    set mid1 [string trimright $apa1]
    set right1 [string range $apa1 [string length $mid1] end]

    set apa2 [string trimleft $line2]
    set left2 [string range $line2 0 [expr [string length $line2] - [string length $apa2] - 1]]
    set mid2 [string trimright $apa2]
    set right2 [string range $apa2 [string length $mid2] end]

    #Move matching left chars to left string.
    set len1 [string length $mid1]
    set len2 [string length $mid2]
    set len [expr $len1 < $len2 ? $len1 : $len2]

    for {set t 0} {$t < $len} {incr t} {
        if {[string index $mid1 $t] != [string index $mid2 $t]} {
            break
        }
    }
    set u [expr $t - 1]
    set left1 ${left1}[string range $mid1 0 $u]
    set mid1 [string range $mid1 $t end]
    set left2 ${left2}[string range $mid2 0 $u]
    set mid2 [string range $mid2 $t end]

    #Move matching right chars to right string.
    set len1 [string length $mid1]
    set len2 [string length $mid2]

    set t1 [expr $len1 - 1]
    set t2 [expr $len2 - 1]

    for {} {$t1 >= 0 && $t2 >= 0} {incr t1 -1;incr t2 -1} {
        if {[string index $mid1 $t1] != [string index $mid2 $t2]} {
            break
        }
    }
    set u1 [expr $t1 + 1]
    set u2 [expr $t2 + 1]
    set right1 [string range $mid1 $u1 end]$right1
    set mid1 [string range $mid1 0 $t1]
    set right2 [string range $mid2 $u2 end]$right2
    set mid2 [string range $mid2 0 $t2]

    if {$len1 == 0} {
        set res1 [list $left1$right1]
    } else {
        set res1 [list $left1 $mid1 $right1]
    }
    if {$len2 == 0} {
        set res2 [list $left1$right1]
    } else {
        set res2 [list $left2 $mid2 $right2]
    }
}

proc dotext {ch1data ch2 tag1 tag2 n1 n2 line1 line2} {
    global  doingLine1 doingLine2

    if {$n1 == 0 && $n2 == 0} {
        while {[gets $ch2 apa] != -1} {
            .t2 insert end [myform $doingLine2 $apa]
            incr doingLine2
            .t1 insert end [myform $doingLine1 $apa]
            incr doingLine1
        }
        return
    }

    while {$doingLine1 < $line1} {
        gets $ch2 apa
        .t1 insert end [myform $doingLine1 $apa]
        incr doingLine1
        .t2 insert end [myform $doingLine2 $apa]
        incr doingLine2
    }
    if {$doingLine2 != $line2} {
        .t1 insert end "**Bad alignment here!! $doingLine2 $line2**\n"
        .t2 insert end "**Bad alignment here!! $doingLine2 $line2**\n"
    }

    if {$n1 == $n2} {
        for {set t 0} {$t < $n1} {incr t} {
            set line1 [lindex $ch1data $t]
            gets $ch2 line2
            comparelines $line1 $line2 res1 res2
            set dotag 0
            .t1 insert end [myforml $doingLine1] $tag1
            foreach i $res1 {
                if {$dotag} {
                    .t1 insert end $i $tag1
                    set dotag 0
                } else {
                    .t1 insert end $i
                    set dotag 1
                }
            }
            .t1 insert end "\n"

            set dotag 0
            .t2 insert end [myforml $doingLine2] $tag2
            foreach i $res2 {
                if {$dotag} {
                    .t2 insert end $i $tag2
                    set dotag 0
                } else {
                    .t2 insert end $i
                    set dotag 1
                }
            }
            .t2 insert end "\n"
            incr doingLine1
            incr doingLine2
        }
    } else {
        for {set t 0} {$t < $n1} {incr t} {
            set apa [lindex $ch1data $t]
            .t1 insert end [myform $doingLine1 $apa] $tag1
            incr doingLine1
        }
        for {set t 0} {$t < $n2} {incr t} {
            gets $ch2 apa
            .t2 insert end [myform $doingLine2 $apa] $tag2
            incr doingLine2
        }
    }
    if {$n1 < $n2} {
        for {set t $n1} {$t < $n2} {incr t} {
            .t1 insert end "\n"
        }
    } elseif {$n2 < $n1} {
        for {set t $n2} {$t < $n1} {incr t} {
            .t2 insert end "\n"
        }
    }
}

proc findNext {} {
    set i [.t1 index @0,0+1line]
    set n1 [.t1 tag nextrange new $i]
    set c1 [.t1 tag nextrange change $i]
    set i [.t2 index @0,0+1line]
    set n2 [.t2 tag nextrange new $i]
    set c2 [.t2 tag nextrange change $i]

    set apa [lsort -dictionary "$n1 $c1 $n2 $c2"]

    if {[llength $apa] != 0} {
        .t1 yview [lindex $apa 0]
        .t2 yview [lindex $apa 0]
    } else {
        .t1 yview end
        .t2 yview end
    }
}

proc enableRedo {} {
    .mf.m entryconfigure 1 -state normal
}

proc disableRedo {} {
    .mf.m entryconfigure 1 -state disabled
}

proc busyCursor {} {
    global oldcursor oldcursor2
    set oldcursor [. cget -cursor]
    set oldcursor2 [.t1 cget -cursor]
    . config -cursor watch
    .t1 config -cursor watch
    .t2 config -cursor watch
}

proc normalCursor {} {
    global oldcursor oldcursor2
    . config -cursor $oldcursor
    .t1 config -cursor $oldcursor2
    .t2 config -cursor $oldcursor2
}

proc time1 {} {
    global tid1
    set tid1 [clock clicks]
}

proc time2 {} {
    global tid1
    set tid2 [clock clicks]
    puts "[expr {$tid2 - $tid1}]"
}

proc doDiff {} {
    global leftFile rightFile leftOK rightOK
    global eqLabel ignorePref RCS MiscPref doingLine1 doingLine2
    global difflines

    if {$RCS == 0 && ($leftOK == 0 || $rightOK == 0)} {
        disableRedo
        return
    } else {
        enableRedo
    }

    busyCursor

    .t1 delete 1.0 end
    .t2 delete 1.0 end
    set difflines {}
    update idletasks

    if {$RCS} {
        set differr [catch {eval exec rcsdiff $MiscPref $ignorePref $rightFile} diffres]
    } else {
        set differr [catch {eval exec diff $MiscPref $ignorePref $leftFile $rightFile} diffres]
    }

    time1

    set apa [split $diffres "\n"]
    set result {}
    set result2 {}
    foreach i $apa {
        if {[string match {[0-9]*} $i]} {
            lappend result $i
        }
        if {[string match {<*} $i]} {
            lappend result2 [string range $i 2 end]
        }
    }

    if {[llength $result] == 0} {
        if {$differr == 1} {
            .t1 insert end $diffres
            normalCursor
            return
        } else {
            set eqLabel "="
        }
    } else {
        set eqLabel " "
    }

    set t2 0
    set ch2 [open $rightFile]
    set doingLine1 1
    set doingLine2 1

    foreach i $result {
        if {![regexp {(.*)([acd])(.*)} $i apa l c r]} {
            .t1 insert 1.0 "No regexp match for $i\n"
        } else {
            if {[regexp {([0-9]+),([0-9]+)} $l apa start stop]} {
                set n1 [expr $stop - $start + 1]
                set line1 $start
            } else {
                set n1 1
                set line1 $l
            }
            if {[regexp {([0-9]+),([0-9]+)} $r apa start stop]} {
                set n2 [expr $stop - $start + 1]
                set line2 $start
            } else {
                set n2 1
                set line2 $r
            }
            switch $c {
                a {
                    # lucka i left, new i right
                    lappend difflist "new [.t1 index end] $n2"
                    dotext "" $ch2 "" new 0 $n2 [expr $line1 +1] $line2
                } c {
                    set apa [lrange $result2 $t2 [expr $t2 + $n1 - 1]]
                    incr t2 $n1
                    if {$n1 > $n2} {
                        lappend difflist "change [.t1 index end] $n1"
                    } else {
                        lappend difflist "change [.t1 index end] $n2"
                    }
                    dotext $apa $ch2 change change $n1 $n2 $line1 $line2
                } d {
                    # lucka i right, new i left
                    set apa [lrange $result2 $t2 [expr $t2 + $n1 - 1]]
                    incr t2 $n1
                    lappend difflist "new [.t1 index end] $n1"
                    dotext $apa $ch2 new "" $n1 0 $line1 [expr $line2 +1]
                }
            }
        }
    }

    dotext "" $ch2 "" "" 0 0 0 0

    close $ch2
    normalCursor
    time2
}

proc doOpenLeft {} {
    global leftFile leftDir rightDir leftOK
    if {![info exists leftDir]} {
        if {[info exists rightDir]} {
            set leftDir $rightDir
        } else {
            set leftDir [pwd]
        }
    }
    set apa [tk_getOpenFile -title "Select left file" -initialdir $leftDir]
    if {$apa != ""} {
	set leftDir [file dirname $apa]
	set leftFile $apa
        set leftOK 1
        return 1
    }
    return 0
}

proc doOpenRight {} {
    global rightFile rightDir leftDir rightOK
    if {![info exists rightDir]} {
        if {[info exists leftDir]} {
            set rightDir $leftDir
        } else {
            set rightDir [pwd]
        }
    }
    set apa [tk_getOpenFile -title "Select right file" -initialdir $rightDir]
    if {$apa != ""} {
        set rightDir [file dirname $apa]
        set rightFile $apa
        set rightOK 1
        return 1
    }
    return 0
}

proc openLeft {} {
    global RCS
    if {[doOpenLeft]} {
        set RCS 0
        doDiff
    }
}

proc openRight {} {
    global RCS
    if {[doOpenRight]} {
        set RCS 0
        doDiff
    }
}

proc openRCS {} {
    global RCS leftFile leftOK
    if {[doOpenRight]} {
        set RCS 1
        set leftFile "RCS"
        set leftOK 0
        doDiff
    }
}

proc openBoth {} {
    global RCS
    if {[doOpenLeft]} {
        if {[doOpenRight]} {
            set RCS 0
            doDiff
        }
    }
}

proc my_yview args {
    eval .t1 yview $args
    eval .t2 yview $args
}

proc chFont {} {
    global fontsize

    font configure myfont -size $fontsize
#    .t1 configure -font "Courier $fontsize"
#    .t2 configure -font "Courier $fontsize"
}

proc makeDiffWin {} {
    global fontsize ignorePref tcl_platform
    eval destroy [winfo children .]

    frame .f
    grid .f - - -row 0 -sticky news

    menubutton .mf -text File -underline 0 -menu .mf.m
    menu .mf.m
    .mf.m add command -label "Redo Diff" -underline 5 -command doDiff -state disabled
    .mf.m add separator
    .mf.m add command -label "Open Both" -underline 0 -command openBoth
    .mf.m add command -label "Open Left File" -command openLeft
    .mf.m add command -label "Open Right File" -command openRight
    if {$tcl_platform(platform) == "unix"} {
        .mf.m add command -label "RCSDiff" -underline 0 -command openRCS
    }
    .mf.m add separator
    .mf.m add command -label "Quit" -command exit

    menubutton .ms -text Fontsize -underline 4 -menu .ms.m
    menu .ms.m
    .ms.m add radiobutton -label 6 -variable fontsize -value 6 -command chFont
    .ms.m add radiobutton -label 7 -variable fontsize -value 7 -command chFont
    .ms.m add radiobutton -label 8 -variable fontsize -value 8 -command chFont
    .ms.m add radiobutton -label 9 -variable fontsize -value 9 -command chFont
    .ms.m add radiobutton -label 10 -variable fontsize -value 10 -command chFont
    set fontsize 9

    menubutton .mi -text Ignore -underline 0 -menu .mi.m
    menu .mi.m
    .mi.m add radiobutton -label "Nothing" -variable ignorePref -value ""
    .mi.m add radiobutton -label "Space changes (-b)" -variable ignorePref -value "-b"
    .mi.m add radiobutton -label "All spaces (-w)" -variable ignorePref -value "-w"
    set ignorePref "-b"

    menubutton .mh -text Help -underline 0 -menu .mh.m
    menu .mh.m
    .mh.m add command -label "About" -command makeAboutWin

    button .bfn -text "Next Diff" -relief raised -command findNext
    entry .eo -width 10 -textvariable MiscPref
    label .lo -text "Diff Options"

    font create myfont -family courier -size $fontsize

    label .l1 -textvariable leftFile -anchor e -width 10
    label .l2 -textvariable rightFile -anchor e -width 10
    text .t1 -height 40 -width 60 -wrap none -yscrollcommand ".sby set" \
	    -xscrollcommand ".sbx1 set" -font myfont
    scrollbar .sby -orient vertical -command "my_yview"
    scrollbar .sbx1 -orient horizontal -command ".t1 xview"
    text .t2 -height 40 -width 60 -wrap none \
	    -xscrollcommand ".sbx2 set" -font myfont
    scrollbar .sbx2 -orient horizontal -command ".t2 xview"
    label .le -textvariable eqLabel -width 1

    .t1 tag configure new -foreground blue -background gray 
    .t1 tag configure change -foreground red -background gray
    .t2 tag configure new -foreground blue -background gray
    .t2 tag configure change -foreground red -background gray
    
    grid .l1 .le .l2 -row 1 -sticky news
    grid .t1 .sby .t2 -row 2 -sticky news
    grid .sbx1 x .sbx2 -row 3 -sticky news
    grid columnconfigure . {0 2} -weight 1
    grid rowconfigure . 2 -weight 1

    pack .mf .ms .mi .mh -in .f -side left
    pack .bfn .eo .lo -in .f -side right
}

proc makeAboutWin {} {
    destroy .ab

    toplevel .ab
    wm title .ab "About"
    text .ab.t -width 45 -height 8 -wrap word
    button .ab.b -text "Close" -command "destroy .ab"
    pack .ab.b -side bottom
    pack .ab.t -side top -expand y -fill both
    
    .ab.t insert end "A Tcl/Tk frontend to diff\n\n"
    .ab.t insert end "Made by Peter Spjuth\n"
    .ab.t insert end "E-Mail: peter.spjuth@space.se\n\n"

}

proc parseCommandLine {} {
    global argv argc MiscPref ignorePref 
    global rightDir rightFile rightOK leftDir leftFile leftOK RCS

    set leftOK 0
    set rightOK 0
    set RCS 0

    if {$argc == 0} return

    set files ""
    foreach arg $argv {
        if {$arg == "-w"} {
            set ignorePref "-w"
        } elseif {$arg == "-b"} {
            set ignorePref "-b"
        } elseif {[string range $arg 0 0] == "-"} {
            set MiscPref "$MiscPref $arg"
        } else {
            set apa [glob -nocomplain $arg]
            if {$apa == ""} {
                puts "Ignoring argument: $arg"
            } else {
                lappend files $apa
            }
        }
    }

    set len [llength $files]
    if {$len == 1} {
        set fullname [file join [pwd] $files]
        set fulldir [file dirname $fullname]
        if {[glob -nocomplain [file join $fulldir RCS]] != ""} {
            set RCS 1
            set rightDir $fulldir
            set rightFile $fullname
            set rightOK 1
            set leftFile "RCS"
            doDiff
        } else {
            set leftDir $fulldir
            set leftFile $fullname
            set leftOK 1
        }
    } elseif {$len >= 2} {
        set fullname [file join [pwd] [lindex $files 0]]
        set fulldir [file dirname $fullname]
        set leftDir $fulldir
        set leftFile $fullname
        set leftOK 1
        set fullname [file join [pwd] [lindex $files 1]]
        set fulldir [file dirname $fullname]
        set rightDir $fulldir
        set rightFile $fullname
        set rightOK 1
        doDiff
    }
}

makeDiffWin
parseCommandLine
