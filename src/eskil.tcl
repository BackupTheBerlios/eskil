#!/bin/sh
#
#   diff.tcl
#
#   Purpose
#             Graphical frontend to diff
#
#   Usage
#             diff.tcl [options] [file1] [file2]
#
#             [options]              All options but the ones below are 
#                                    passed to diff. 
#             [file1],[file2]        Files to be compared
#                                    If no files are given, the program is
#                                    started anyway and you can select files
#                                    from within.
#                                    If only one file is given, the program
#                                    looks for an RCS directory next to the
#                                    file, and if found, runs rcsdiff.
#
#             Options for diff.tcl:
#
#             -nodiff  : Normally if there are enough information on the
#                        command line to run diff, diff.tcl will do so unless
#                        this option is specified.
#
#             -noparse : Diff.tcl can perform analysis of changed blocks to
#             -line    : improve display. See online help for details.
#             -block   : The default is -block, but this can be slow if there
#                        are large change blocks.
#
#             -char    : The analysis of changes can be done on either
#             -word    : character or word basis. -char is the default.
#
#   Author    Peter Spjuth  980612
#
#   Revised   By       Date     Remark
#
#     1.0     DC-PS    980612   New Version.
#     1.1     DC-PS    980807   Parsing of change blocks added
#                               Options menu and variables changed
#                               Command line options added
#     1.2     DC-PS    980812   Improved yscroll
#                               Added map next to y-scrollbar
#
#-----------------------------------------------
# the next line restarts using wish \
exec wish "$0" "$@"

set debug 1
set diffver "Version 1.2  beta"
set color(change) red
set color(new) blue

proc myform {line text} {
    return [format "%3d: %s\n" $line $text]
}

proc myforml {line} {
    return [format "%3d: " $line]
}

#Compare two lines to find inequalities to highlight.
#The return value is, for each line, a list where the first, third etc.
#element is equal between the lines. The second, fourth etc. will be
#highlighted.
#The current implementation returns one or three elements.
proc comparelines {line1 line2 res1var res2var} {
    global Pref
    upvar $res1var res1
    upvar $res2var res2

    #Skip white space in both ends
    set apa1 [string trimleft $line1]
    set left1 [expr {[string length $line1] - [string length $apa1]}]
    set mid1 [string trimright $line1]

    set apa2 [string trimleft $line2]
    set left2 [expr {[string length $line2] - [string length $apa2]}]
    set mid2 [string trimright $line2]

    #Check for matching left chars/words.

    set len1 [string length $apa1]
    set len2 [string length $apa2]
    set len [expr {$len1 < $len2 ? $len1 : $len2}]
    for {set t 0; set s 0; set flag 0} {$t < $len} {incr t} {
        if {[set c [string index $apa1 $t]] != [string index $apa2 $t]} {
            incr flag 2
            break
        }
        if {$c == " "} {set s $t; set flag 1}
    }
    
    if {$Pref(lineparsewords) == 0} {
        incr left1 $t
        incr left2 $t
    } else {
        if {$flag < 2} {
            set s $len
        } elseif {$flag == 3} {
            incr s
        }
        incr left1 $s
        incr left2 $s
    }

    #Check for matching right chars.
    
    set len1 [string length $mid1]
    set len2 [string length $mid2]
    
    set t1 [expr {$len1 - 1}]
    set t2 [expr {$len2 - 1}]
    set s1 $t1
    set s2 $t2
    set flag 0
    for {} {$t1 >= $left1 && $t2 >= $left2} {incr t1 -1;incr t2 -1} {
        if {[set c [string index $mid1 $t1]] != [string index $mid2 $t2]} {
            incr flag 2
            break
        }
        if {$c == " "} {set s1 $t1; set s2 $t2; set flag 1}
    }
    if {$Pref(lineparsewords) == 1} {
        if {$flag >= 2} {
            if {$flag == 3} {
                incr s1 -1
                incr s2 -1
            }
            set t1 $s1
            set t2 $s2
        }
    }
    
    #Make the result
    if {$left1 > $t1} {
        set res1 [list $line1]
    } else {
        set right1 [string range $line1 [expr {$t1 + 1}] end]
        set mid1 [string range $line1 $left1 $t1]
        set left1 [string range $line1 0 [expr {$left1 - 1}]]
        set res1 [list $left1 $mid1 $right1]
    }
    if {$left2 > $t2} {
        set res2 [list $line2]
    } else {
        set right2 [string range $line2 [expr {$t2 + 1}] end]
        set mid2 [string range $line2 $left2 $t2]
        set left2 [string range $line2 0 [expr {$left2 - 1}]]
        set res2 [list $left2 $mid2 $right2]
    }
}

#Count how many characters are common between the lines
proc comparelines2 {line1 line2} {
    comparelines $line1 $line2 res1 res2

    set len1 [llength $res1]
    for {set t 0; set sum1 0} {$t < $len1} {incr t 2} {
        incr sum1 [string length [lindex $res1 $t]]
    } 

    return $sum1
}

#Decide how to display change blocks
proc compareblocks {block1 block2} {
    set size1 [llength $block1]
    set size2 [llength $block2]

    if {$size1 > $size2} {
        set apa $block1
        set block1 $block2
        set block2 $apa
        set size1 [llength $block1]
        set size2 [llength $block2]
        set dsym a
        set asym d
    } else {
        set dsym d
        set asym a
    }

    set result {}
    foreach line $block1 {
        set bestscore 0
        set bestline 0
        for {set i 0} {$i < $size2} {incr i} {
            set x [comparelines2 $line [lindex $block2 $i]]
            if {$x > $bestscore} {
                set bestscore $x
                set bestline $i
            }
        }
        lappend result $bestline
    }

    #Check that $result is in order
    if {$size1 > 1} {
        set bad ""
        for {set i 0; set j 1} {$j < $size1} {incr i; incr j} {
            if {[lindex $result $i] >= [lindex $result $j]} {
                lappend bad $i
            }
        }
        foreach i $bad {
            set next 0
            set j [expr {$i + 1}]
            if {$i == 0} {
                set l1 0
            } else {
                set l1 [lindex $result [expr {$i - 1}]]
            }
            set l2 [lindex $result $i]
            set l3 [lindex $result $j]
            if {$i + 2 >= $size1} {
                set l4 [expr {$size2 - 1}]
            } else {
                set l4 [lindex $result [expr {$i + 2}]]
            }
            for {set t [expr {$l1 + 1}]} {$t < $l3} {incr t} {
                if {[lsearch $result $t] == -1} {
                    set result [lreplace $result $i $i $t]
                    set next 1
                    break
                }
            }
            if {$next == 1} continue
            for {set t [expr {$l2 + 1}]} {$t < $l4} {incr t} {
                if {[lsearch $result $t] == -1} {
                    set result [lreplace $result $j $j $t]
                    set next 1
                    break
                }
            }
        }
    }
    
    set apa {}
    set t1 0 
    set t2 0
    while {$t1 < $size1 || $t2 < $size2} {
        if {$t1 < $size1} {
            set r [lindex $result $t1]
            if {$r < $t2} {
                lappend apa $dsym
                incr t1
            } elseif {$r == $t2} {
                lappend apa "c"
                incr t1
                incr t2
            } else {
                lappend apa $asym
                incr t2
            }
        } else {
            lappend apa $asym
            incr t2
        }
    }
    return $apa
}

proc insertMatchingLines {line1 line2 tag1 tag2} {
    global doingLine1 doingLine2 Pref

    if {$Pref(parse) != "none"} {
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
    } else {
        .t1 insert end [myform $doingLine1 $line1] $tag1
        .t2 insert end [myform $doingLine2 $line2] $tag2
    }
    incr doingLine1
    incr doingLine2
}

proc dotext {ch1data ch2 tag1 tag2 n1 n2 line1 line2} {
    global doingLine1 doingLine2 Pref mapList mapMax

    if {$n1 == 0 && $n2 == 0} {
        while {[gets $ch2 apa] != -1} {
            .t2 insert end [myform $doingLine2 $apa]
            incr doingLine2
            .t1 insert end [myform $doingLine1 $apa]
            incr doingLine1
            incr mapMax
        }
        return
    }

    #Display all equal lines before next diff
    while {$doingLine1 < $line1} {
        gets $ch2 apa
        .t1 insert end [myform $doingLine1 $apa]
        incr doingLine1
        .t2 insert end [myform $doingLine2 $apa]
        incr doingLine2
        incr mapMax
    }
    if {$doingLine2 != $line2} {
        .t1 insert end "**Bad alignment here!! $doingLine2 $line2**\n"
        .t2 insert end "**Bad alignment here!! $doingLine2 $line2**\n"
    }

    if {$n1 == $n2} {
        for {set t 0} {$t < $n1} {incr t} {
            set line1 [lindex $ch1data $t]
            gets $ch2 line2
            insertMatchingLines $line1 $line2 $tag1 $tag2
        }
        lappend mapList $mapMax
        incr mapMax $n1
        lappend mapList $mapMax $tag1
    } else {
        if {$n1 != 0 && $n2 != 0 && $Pref(parse) == "block"} {
            set block1 {}
            for {set t 0} {$t < $n1} {incr t} {
                set apa [lindex $ch1data $t]
                lappend block1 $apa
            }
            set block2 {}
            for {set t 0} {$t < $n2} {incr t} {
                gets $ch2 apa
                lappend block2 $apa
            }
            set apa [compareblocks $block1 $block2]

            set t1 0
            set t2 0
            foreach c $apa {
                if {$c == "c"} {
                    set line1 [lindex $block1 $t1]
                    set line2 [lindex $block2 $t2]
                    insertMatchingLines $line1 $line2 $tag1 $tag2
                    incr t1
                    incr t2
                }
                if {$c == "d"} {
                    set bepa [lindex $block1 $t1]
                    .t1 insert end [myform $doingLine1 $bepa] $tag1
                    .t2 insert end "\n"
                    incr doingLine1
                    incr t1
                }
                if {$c == "a"} {
                    set bepa [lindex $block2 $t2]
                    .t2 insert end [myform $doingLine2 $bepa] $tag2
                    .t1 insert end "\n"
                    incr doingLine2
                    incr t2
                }
            }
            lappend mapList $mapMax
            incr mapMax [llength $apa]
            lappend mapList $mapMax $tag1
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
            if {$n1 < $n2} {
                for {set t $n1} {$t < $n2} {incr t} {
                    .t1 insert end "\n"
                }
                lappend mapList $mapMax
                incr mapMax $n2
                lappend mapList $mapMax $tag2
            } elseif {$n2 < $n1} {
                for {set t $n2} {$t < $n1} {incr t} {
                    .t2 insert end "\n"
                }
                lappend mapList $mapMax
                incr mapMax $n1
                lappend mapList $mapMax $tag1
            }
        }
    }
}

#Scroll windows to next diff
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
    global tid1 debug
    set tid2 [clock clicks]
    if {$debug == 1} {
        puts "[expr {$tid2 - $tid1}]"
    }
}

proc doDiff {} {
    global leftFile rightFile leftOK rightOK RCS
    global eqLabel RCS Pref doingLine1 doingLine2
    global mapList mapMax

    if {$RCS == 0 && ($leftOK == 0 || $rightOK == 0)} {
        disableRedo
        return
    } else {
        enableRedo
    }

    busyCursor

    .t1 delete 1.0 end
    .t2 delete 1.0 end
    set mapList {}
    set mapMax 0

    update

    if {$RCS} {
        set differr [catch {eval exec rcsdiff $Pref(dopt) $Pref(ignore) $rightFile} diffres]
    } else {
        set differr [catch {eval exec diff $Pref(dopt) $Pref(ignore) $leftFile $rightFile} diffres]
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
                set n1 [expr {$stop - $start + 1}]
                set line1 $start
            } else {
                set n1 1
                set line1 $l
            }
            if {[regexp {([0-9]+),([0-9]+)} $r apa start stop]} {
                set n2 [expr {$stop - $start + 1}]
                set line2 $start
            } else {
                set n2 1
                set line2 $r
            }
            switch $c {
                a {
                    # lucka i left, new i right
                    lappend difflist "new [.t1 index end] $n2"
                    dotext "" $ch2 "" new 0 $n2 [expr {$line1 + 1}] $line2
                } c {
                    set apa [lrange $result2 $t2 [expr {$t2 + $n1 - 1}]]
                    incr t2 $n1
                    if {$n1 > $n2} {
                        lappend difflist "change [.t1 index end] $n1"
                    } else {
                        lappend difflist "change [.t1 index end] $n2"
                    }
                    dotext $apa $ch2 change change $n1 $n2 $line1 $line2
                } d {
                    # lucka i right, new i left
                    set apa [lrange $result2 $t2 [expr {$t2 + $n1 - 1}]]
                    incr t2 $n1
                    lappend difflist "new [.t1 index end] $n1"
                    dotext $apa $ch2 new "" $n1 0 $line1 [expr {$line2 + 1}]
                }
            }
        }
    }

    dotext "" $ch2 "" "" 0 0 0 0

    close $ch2
    drawMap -1
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

proc drawMap {newh} {
    global mapList mapMax color
 
    set oldh [map cget -height]
    if {$oldh == $newh} return
 
    map blank
    if {![info exists mapList] || $mapList == ""} return

    set w [winfo width .c]
    set h [winfo height .c]
    set x2 [expr {$w - 1}]
    map configure -width $w -height $h
 
    foreach {start stop type} $mapList {
        set y1 [expr {$start * $h / $mapMax}]
        set y2 [expr {$stop * $h / $mapMax + 1}]
        map put $color($type) -to 1 $y1 $x2 $y2
    }
}

proc my_yview args {
    eval .t1 yview $args
    eval .t2 yview $args
}

proc my_yscroll args {
    eval .sby set $args
    my_yview moveto [lindex $args 0]
}

proc chFont {} {
    global Pref

    font configure myfont -size $Pref(fontsize)
#    .t1 configure -font "Courier $Pref(fontsize)"
#    .t2 configure -font "Courier $Pref(fontsize)"
}

proc makeDiffWin {} {
    global Pref tcl_platform debug color
    eval destroy [winfo children .]

    frame .f
    grid .f - - - -row 0 -sticky news

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

    menubutton .mo -text Options -underline 0 -menu .mo.m
    menu .mo.m
    .mo.m add cascade -label Fontsize -underline 0 -menu .mo.mf
    .mo.m add cascade -label Ignore -underline 0 -menu .mo.mi
    .mo.m add cascade -label Parse -underline 0 -menu .mo.mp
    .mo.m add separator
    .mo.m add command -label "Save default" -command saveOptions

    menu .mo.mf
    .mo.mf add radiobutton -label 6 -variable Pref(fontsize) -value 6 -command chFont
    .mo.mf add radiobutton -label 7 -variable Pref(fontsize) -value 7 -command chFont
    .mo.mf add radiobutton -label 8 -variable Pref(fontsize) -value 8 -command chFont
    .mo.mf add radiobutton -label 9 -variable Pref(fontsize) -value 9 -command chFont
    .mo.mf add radiobutton -label 10 -variable Pref(fontsize) -value 10 -command chFont

    menu .mo.mi
    .mo.mi add radiobutton -label "Nothing" -variable Pref(ignore) -value ""
    .mo.mi add radiobutton -label "Space changes (-b)" -variable Pref(ignore) -value "-b"
    .mo.mi add radiobutton -label "All spaces (-w)" -variable Pref(ignore) -value "-w"

    menu .mo.mp
    .mo.mp add radiobutton -label "Nothing" -variable Pref(parse) -value "none"
    .mo.mp add radiobutton -label "Lines" -variable Pref(parse) -value "line"
    .mo.mp add radiobutton -label "Blocks" -variable Pref(parse) -value "block"
    .mo.mp add separator
    .mo.mp add radiobutton -label "Characters" -variable Pref(lineparsewords) -value "0"
    .mo.mp add radiobutton -label "Words" -variable Pref(lineparsewords) -value "1"

    menubutton .mh -text Help -underline 0 -menu .mh.m
    menu .mh.m
    .mh.m add command -label "Help" -command {after 100 makeHelpWin}
    .mh.m add command -label "About" -command makeAboutWin

    button .bfn -text "Next Diff" -relief raised -command findNext
    entry .eo -width 10 -textvariable Pref(dopt)
    label .lo -text "Diff Options"

    catch {font delete myfont}
    font create myfont -family courier -size $Pref(fontsize)

    label .l1 -textvariable leftFile -anchor e -width 10
    label .l2 -textvariable rightFile -anchor e -width 10
    text .t1 -height 40 -width 60 -wrap none -yscrollcommand my_yscroll \
	    -xscrollcommand ".sbx1 set" -font myfont
    scrollbar .sby -orient vertical -command "my_yview"
    scrollbar .sbx1 -orient horizontal -command ".t1 xview"
    text .t2 -height 40 -width 60 -wrap none -yscrollcommand my_yscroll \
	    -xscrollcommand ".sbx2 set" -font myfont
    scrollbar .sbx2 -orient horizontal -command ".t2 xview"
    label .le -textvariable eqLabel -width 1
    canvas .c -width 4

    .t1 tag configure new -foreground $color(new) -background gray 
    .t1 tag configure change -foreground $color(change) -background gray
    .t2 tag configure new -foreground $color(new) -background gray
    .t2 tag configure change -foreground $color(change) -background gray
    
    grid .l1 .le - .l2 -row 1 -sticky news
    grid .t1 .c .sby .t2 -row 2 -sticky news
    grid .sbx1 x x .sbx2 -row 3 -sticky news
    grid columnconfigure . {0 3} -weight 1
    grid rowconfigure . 2 -weight 1
    grid .c -pady [expr {[.sby cget -width] + 2}]

    image create photo map
    .c create image 0 0 -anchor nw -image map
    bind .c <Configure> {drawMap %h}

    if {$debug == 1} {
        menubutton .md -text Debug -menu .md.m -relief ridge
        menu .md.m
        .md.m add checkbutton -label Console -variable consolestate \
                -onvalue show -offvalue hide -command {console $consolestate}
        .md.m add separator
        .md.m add command -label "Reread Source" -command {source diff.tcl}
        .md.m add separator
        .md.m add command -label "Redraw Window" -command {makeDiffWin}
        
        pack .mf .mo .mh .md -in .f -side left
    } else {
        pack .mf .mo .mh -in .f -side left
    }
    pack .bfn .eo .lo -in .f -side right
}

proc makeAboutWin {} {
    global diffver
    destroy .ab

    toplevel .ab
    wm title .ab "About Diff.tcl"
    text .ab.t -width 45 -height 8 -wrap word
    button .ab.b -text "Close" -command "destroy .ab"
    pack .ab.b -side bottom
    pack .ab.t -side top -expand y -fill both
    
    .ab.t insert end "A Tcl/Tk frontend to diff\n\n"
    .ab.t insert end "$diffver\n"
    .ab.t insert end "Made by Peter Spjuth\n"
    .ab.t insert end "E-Mail: peter.spjuth@space.se\n\n"

}

proc makeHelpWin {} {
    global color
    destroy .he

    toplevel .he
    wm title .he "Diff.tcl Help"
    text .he.t -width 80 -height 15 -wrap word -yscrollcommand ".he.sb set"\
            -font "Courier 8"
    scrollbar .he.sb -orient vert -command ".he.t yview"
    button .he.b -text "Close" -command "destroy .he"
    pack .he.b -side bottom
    pack .he.sb -side right -fill y
    pack .he.t -side left -expand y -fill both
    .he.t tag configure new -foreground $color(new) -background gray 
    .he.t tag configure change -foreground $color(change) -background gray
    .he.t tag configure ul -underline 1

    .he.t insert end {\

} "" {Commands} ul {

File Menu
  Redo Diff      : Run diff again on the same files.
  Open Both      : Select two files, run diff.
  Open Left File : Select a file for left window, run diff 
  Open Right File: Select a file for right window, run diff
  RCSDiff        : (UNIX only) Select one file and run rcsdiff.
  Quit           : Guess

Options Menu
  Fontsize : Select fontsize for the two main text windows
  Ignore   : Diff options for handling whitespace
  Parse    : Additional parsing made by diff.tcl to improve the display.
             See examples below.
             Nothing: No parsing made.
             Lines  : When there is a changed block with the same number
                      of lines in both right and left files, diff.tcl
                      compares corresponding lines and tries to only
                      highlight the part that has been changed.
             Blocks : When the number of lines in a changed block is not
                      the same in both files, diff.tcl tries to find lines
                      that look the same and place them abreast.
             The Char and Word options selects if the line parsing should
             highlight full words only, or check single characters.

             Save default: Save current option settings in ~/.diffrc

Diff Options Field: Any text written here will be passed to diff.

Next Diff Button: Scrolls to the next differing block, or to the bottom
                  if there are no more diffs.

Equal sign: Above the vertical scrollbar, a "=" will appear if the files
            are equal.

} "" {Examples of effects of parse options.} ul {

Below are two example files, and four different results when using
different options with those files.

Left file:                       Right file:
NET '/I$1/N$1454' IC2-15 IC5-7   NET '/I$1/N$1454' IC2-15 IC5-2 IC5-7
NET '/I$1/N$1455' IC2-14 IC6-8   NET '/I$1/N$1456' IC2-12            
NET '/I$1/N$1456' IC2-13 IC2-12  NET '/I$1/N$1457' IC2-12 IC6-7      
NET '/I$1/N$1457' IC2-12 IC6-7   NET '/I$1/N$1458' IC2-11            
NET '/I$1/N$1458' IC2-10       

}

.he.t insert end "Example 1. No parsing.\n"
.he.t insert end {1: NET '/I$1/N$1454' IC2-15 IC5-7   1: NET '/I$1/N$1454' IC2-15 IC5-2 IC5-7
} change
.he.t insert end {2: NET '/I$1/N$1455' IC2-14 IC6-8   2: NET '/I$1/N$1456' IC2-12            
} change
.he.t insert end {3: NET '/I$1/N$1456' IC2-13 IC2-12  } change
.he.t insert end {
4: NET '/I$1/N$1457' IC2-12 IC6-7   3: NET '/I$1/N$1457' IC2-12 IC6-7
}
.he.t insert end {5: NET '/I$1/N$1458' IC2-10         4: NET '/I$1/N$1458' IC2-11            
} change 

.he.t insert end "\n"

.he.t insert end "Example 2. Lines and characters\n"
.he.t insert end {1: NET '/I$1/N$1454' IC2-15 IC5-7   1: NET '/I$1/N$1454' IC2-15 IC5-2 IC5-7
} change
.he.t insert end {2: NET '/I$1/N$1455' IC2-14 IC6-8   2: NET '/I$1/N$1456' IC2-12            
} change
.he.t insert end {3: NET '/I$1/N$1456' IC2-13 IC2-12  } change
.he.t insert end {
4: NET '/I$1/N$1457' IC2-12 IC6-7   3: NET '/I$1/N$1457' IC2-12 IC6-7
}
.he.t insert end {5: } change {NET '/I$1/N$1458' IC2-1} "" {0} change {         } "" {4: } change {NET '/I$1/N$1458' IC2-1} "" {1} change "\n"

.he.t insert end "\n"

.he.t insert end "Example 3. Blocks and characters\n"

.he.t insert end {1: } change {NET '/I$1/N$1454' IC2-15 IC5-7   } "" {1: } change {NET '/I$1/N$1454' IC2-15 IC5-} "" {2 IC5-} change "7\n"
.he.t insert end {2: NET '/I$1/N$1455' IC2-14 IC6-8   } change "\n" ""
.he.t insert end {3: } change {NET '/I$1/N$1456' IC2-1} "" {3 IC2-1} change {2  } "" {2: } change {NET '/I$1/N$1456' IC2-12
}
.he.t insert end {4: NET '/I$1/N$1457' IC2-12 IC6-7   3: NET '/I$1/N$1457' IC2-12 IC6-7
}
.he.t insert end {5: } change {NET '/I$1/N$1458' IC2-1} "" {0} change {         } "" {4: } change {NET '/I$1/N$1458' IC2-1} "" {1} change "\n"

.he.t insert end "\n"

.he.t insert end "Example 4. Blocks and words\n"

.he.t insert end {1: } change {NET '/I$1/N$1454' IC2-15 IC5-7   } "" {1: } change {NET '/I$1/N$1454' IC2-15 } "" {IC5-2 } change "IC5-7\n"
.he.t insert end {2: NET '/I$1/N$1455' IC2-14 IC6-8   } change "\n" ""
.he.t insert end {3: } change {NET '/I$1/N$1456' } "" {IC2-13 } change {IC2-12  } "" {2: } change {NET '/I$1/N$1456' IC2-12
}
.he.t insert end {4: NET '/I$1/N$1457' IC2-12 IC6-7   3: NET '/I$1/N$1457' IC2-12 IC6-7
}
.he.t insert end {5: } change {NET '/I$1/N$1458' } "" {IC2-10} change {         } "" {4: } change {NET '/I$1/N$1458' } "" {IC2-11} change "\n"

}

proc parseCommandLine {} {
    global argv argc Pref 
    global rightDir rightFile rightOK leftDir leftFile leftOK RCS

    set leftOK 0
    set rightOK 0
    set RCS 0
    set noautodiff 0

    if {$argc == 0} return

    set files ""
    foreach arg $argv {
        if {$arg == "-w"} {
            set Pref(ignore) "-w"
        } elseif {$arg == "-b"} {
            set Pref(ignore) "-b"
        } elseif {$arg == "-noparse"} {
            set Pref(parse) "none"
        } elseif {$arg == "-line"} {
            set Pref(parse) "line"
        } elseif {$arg == "-block"} {
            set Pref(parse) "block"
        } elseif {$arg == "-char"} {
            set Pref(lineparsewords) 0
        } elseif {$arg == "-word"} {
            set Pref(lineparsewords) 1
        } elseif {$arg == "-nodiff"} {
            set noautodiff 1
        } elseif {[string range $arg 0 0] == "-"} {
            set Pref(dopt) "$Pref(dopt) $arg"
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
            if {$noautodiff == "1"} {
                enableRedo
            } else {
                doDiff
            }
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
        if {$noautodiff == "1"} {
            enableRedo
        } else {
            doDiff
        }
    }
}

proc saveOptions {} {
    global Pref
    set ch [open "~/.diffrc" "w"]

    set a [array names Pref]
    foreach i $a {
        if {$i != "dopt"} {
            puts $ch "set Pref($i) \"$Pref($i)\""
        }
    }
    close $ch
}

proc getOptions {} {
    global Pref

    set Pref(fontsize) 9
    set Pref(ignore) "-b"
    set Pref(dopt) ""
    set Pref(parse) "block"
    set Pref(lineparsewords) "0"

    if {[file exists "~/.diffrc"]} {
        source "~/.diffrc"
    }
}

if {![winfo exists .f]} {
    getOptions
    makeDiffWin
    parseCommandLine
}
