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
#             [options]              All options but the ones listed below
#                                    are passed to diff. 
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
#             -2nd     : Turn on or off second stage parsing.
#             -no2nd   : It is on by default.
#
#   Author    Peter Spjuth  980612
#
#   Revised   By       Date     Remark
#
#     1.0     DC-PS    980612   New Version.
#     1.1     DC-PS    980807   Parsing of change blocks added
#                               Options menu and variables changed
#                               Command line options added
#     1.2     DC-PS    980818   Improved yscroll
#                               Added map next to y-scrollbar
#     1.3     DC-PS    980921   Added Prev Diff button
#                               Added colour options, and Only diffs option
#                               Added 2nd stage line parsing
#                               Improved block parsing
#                               Added print
#
#-----------------------------------------------
# the next line restarts using wish \
exec wish "$0" "$@"

set debug 0
set diffver "Version 1.3 980921"

proc myform {lineNo text} {
    return [format "%3d: %s\n" $lineNo $text]
}

proc myforml {lineNo} {
    return [format "%3d: " $lineNo]
}

proc maxabs {a b} {
    return [expr {abs($a) > abs($b) ? $a : $b}]
}

#2nd stage line parsing
#Recursively look for common substrings in strings s1 and s2
proc compareMidString {s1 s2 res1Name res2Name {test 0}} {
    global Pref
    upvar $res1Name res1
    upvar $res2Name res2

    set len1 [string length $s1]
    set len2 [string length $s2]

    #Is s1 a substring of s2 ?
    if {$len1 < $len2} {
        set t [string first $s1 $s2]
        if {$t != -1} {
            set left2 [string range $s2 0 [expr {$t - 1}]]
            set mid2 [string range $s2 $t [expr {$t + $len1 - 1}]]
            set right2 [string range $s2 [expr {$t + $len1}] end]
            set res2 [list $left2 $mid2 $right2]
            set res1 [list "" $s1 ""]
            return
        }
    }
        
    #Is s2 a substring of s1 ?
    if {$len2 < $len1} {
        set t [string first $s2 $s1]
        if {$t != -1} {
            set left1 [string range $s1 0 [expr {$t - 1}]]
            set mid1 [string range $s1 $t [expr {$t + $len2 - 1}]]
            set right1 [string range $s1 [expr {$t + $len2}] end]
            set res1 [list $left1 $mid1 $right1]
            set res2 [list "" $s2 ""]
            return
        }
    }

    #Are they too short to be considered ?
    if {$len1 < 4 || $len2 < 4} {
        set res1 [list $s1]
        set res2 [list $s2]
        return
    }

    set foundlen -1
    set minlen 3

    #Find the longest string common to both strings
    for {set t 0 ; set u $minlen} {$u <= $len1} {incr t ; incr u} {
        set i [string first [string range $s1 $t $u] $s2]
        if {$i >= 0} {
            for {set p1 $u ; set p2 [expr {$i + $minlen}]} \
                    {$p1 < $len1 && $p2 < $len2} {incr p1 ; incr p2} {
                if {[string index $s1 $p1] != [string index $s2 $p2]} {
                    break
                }
            }
            if {$Pref(lineparsewords) != 0 && $test == 0} {
                set newt $t
                if {($t > 0 && [string index $s1 [expr {$t - 1}]] != " ") || \
                        ($i > 0 && [string index $s2 [expr {$i - 1}]] != " ")} {
                    for {} {$newt < $p1} {incr newt} {
                        if {[string index $s1 $newt] == " "} break
                    }
                }

                set newp1 [expr {$p1 - 1}]
                if {($p1 < $len1 && [string index $s1 $p1] != " ") || \
                        ($p2 < $len2 && [string index $s2 $p2] != " ")} {
                    for {} {$newp1 > $newt} {incr newp1 -1} {
                        if {[string index $s1 $newp1] == " "} break
                    }
                }
                incr newp1

                if {$newp1 - $newt > $minlen} {
                    set foundlen [expr {$newp1 - $newt}]
                    set found1 $newt
                    set found2 [expr {$i + $newt - $t}]
                    set minlen $foundlen
                    set u [expr {$t + $minlen}]
                }
            } else {
                set foundlen [expr {$p1 - $t}]
                set found1 $t
                set found2 $i
                set minlen $foundlen
                set u [expr {$t + $minlen}]
            }
        }
    }
    
    if {$foundlen == -1} {
        set res1 [list $s1]
        set res2 [list $s2]
    } else {
        set left1 [string range $s1 0 [expr {$found1 - 1}]]
        set mid1 [string range $s1 $found1 [expr {$found1 + $foundlen - 1}]]
        set right1 [string range $s1 [expr {$found1 + $foundlen}] end]

        set left2 [string range $s2 0 [expr {$found2 - 1}]]
        set mid2 [string range $s2 $found2 [expr {$found2 + $foundlen - 1}]]
        set right2 [string range $s2 [expr {$found2 + $foundlen}] end]

        compareMidString $left1 $left2 left1 left2 $test
        compareMidString $right1 $right2 right1 right2 $test

        set res1 [concat $left1 [list $mid1] $right1]
        set res2 [concat $left2 [list $mid2] $right2]
    }

    return
}

#Compare two lines to find inequalities to highlight.
#The return value is, for each line, a list where the first, third etc.
#element is equal between the lines. The second, fourth etc. will be
#highlighted.
proc comparelines {line1 line2 res1Name res2Name {test 0}} {
    global Pref
    upvar $res1Name res1
    upvar $res2Name res2

    #Skip white space in both ends

    set apa1 [string trimleft $line1]
    set leftp1 [expr {[string length $line1] - [string length $apa1]}]
    set mid1 [string trimright $line1]

    set apa2 [string trimleft $line2]
    set leftp2 [expr {[string length $line2] - [string length $apa2]}]
    set mid2 [string trimright $line2]

    #Check for matching left chars/words.
    #leftp1 and leftp2 will be the indicies of the first difference

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
    
    if {$Pref(lineparsewords) == 0 && $test == 0} {
        incr leftp1 $t
        incr leftp2 $t
    } else {
        if {$flag < 2} {
            set s $len
        } elseif {$flag == 3} {
            incr s
        }
        incr leftp1 $s
        incr leftp2 $s
    }

    #Check for matching right chars/words.
    #t1 and t2 will be the indicies of the last difference
    
    set len1 [string length $mid1]
    set len2 [string length $mid2]
    
    set t1 [expr {$len1 - 1}]
    set t2 [expr {$len2 - 1}]
    set s1 $t1
    set s2 $t2
    set flag 0
    for {} {$t1 >= $leftp1 && $t2 >= $leftp2} {incr t1 -1;incr t2 -1} {
        if {[set c [string index $mid1 $t1]] != [string index $mid2 $t2]} {
            incr flag 2
            break
        }
        if {$c == " "} {set s1 $t1; set s2 $t2; set flag 1}
    }
    if {$Pref(lineparsewords) == 1 && $test == 0} {
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
    if {$leftp1 > $t1} {
        set res1 [list $line1]
    } else {
        set right1 [string range $line1 [expr {$t1 + 1}] end]
        set mid1 [string range $line1 $leftp1 $t1]
        set left1 [string range $line1 0 [expr {$leftp1 - 1}]]
        set res1 [list $left1 $mid1 $right1]
    }
    if {$leftp2 > $t2} {
        set res2 [list $line2]
    } else {
        set right2 [string range $line2 [expr {$t2 + 1}] end]
        set mid2 [string range $line2 $leftp2 $t2]
        set left2 [string range $line2 0 [expr {$leftp2 - 1}]]
        set res2 [list $left2 $mid2 $right2]
    }
    if {$Pref(extralineparse) != 0 && $leftp1 <= $t1 && $leftp2 <= $t2} {
        compareMidString $mid1 $mid2 mid1 mid2 $test
        set res1 [eval lreplace \$res1 1 1 $mid1]
        set res2 [eval lreplace \$res2 1 1 $mid2]
    }
}

#Count how many characters are common between two lines
proc comparelines2 {line1 line2} {
    comparelines $line1 $line2 res1 res2 1

    #Add lengths of every other element
    set sumsame 0
    set sumdiff1 0
    set sumdiff2 0
    foreach {same diff} $res1 {
        incr sumsame [string length $same]
        incr sumdiff1 [string length $diff]
    }
    foreach {same diff} $res2 {
        incr sumdiff2 [string length $diff]
    }

    return [expr {$sumsame - [maxabs $sumdiff1 $sumdiff2]}]
}

#Decide how to display change blocks
proc compareblocks {block1 block2} {
    set size1 [llength $block1]
    set size2 [llength $block2]

    #Swap if block1 is bigger
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

    #Collect statistics
    set result {}
    set scores {}
    foreach line1 $block1 {
        set bestscore 0
        set bestline 0
        set i 0
        foreach line2 $block2 {  
            set x [comparelines2 $line1 $line2]
            if {$x > $bestscore} {
                set bestscore $x
                set bestline $i
            }
            incr i
        }
        lappend result $bestline
        lappend scores $bestscore
    }

    #If result is in order, no problem.
    #Otherwise, try to adjust result to make it ordered
    if {$size1 > 1} {
        set bad 1
        for {set loop 0} {$bad != "" && $loop < 2} {incr loop} {
            set bad {}
            for {set i 0; set j 1} {$j < $size1} {incr i; incr j} {
                if {[lindex $result $i] >= [lindex $result $j]} {
                    lappend bad $i
                }
            }
            foreach i $bad {
                set next 0
                set j [expr {$i + 1}]
                if {$i == 0} {
                    set l1 -10
                } else {
                    set l1 [lindex $result [expr {$i - 1}]]
                }
                set l2 [lindex $result $i]
                set l3 [lindex $result $j]
                if {$i + 2 >= $size1} {
                    set l4 [expr {$size2 + 10}]
                } else {
                    set l4 [lindex $result [expr {$i + 2}]]
                }

                #Try to move the one with lowest score first
                set si [lindex $scores $i]
                set sj [lindex $scores $j]
                if {$si < $sj} {
                    for {set t [expr {$l3 - 1}]} {$t > $l1} {incr t -1} {
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
                    if {$next == 1} continue
                } else {
                    for {set t [expr {$l2 + 1}]} {$t < $l4} {incr t} {
                        if {[lsearch $result $t] == -1} {
                            set result [lreplace $result $j $j $t]
                            set next 1
                            break
                        }
                    }
                    if {$next == 1} continue
                    for {set t [expr {$l3 - 1}]} {$t > $l1} {incr t -1} {
                        if {[lsearch $result $t] == -1} {
                            set result [lreplace $result $i $i $t]
                            set next 1
                            break
                        }
                    }
                    if {$next == 1} continue
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
            if {$r < $t2 || $t2 >= $size2} {
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

#Insert one line in each text widget.
#Mark them as changed, and optionally parse them.
proc insertMatchingLines {line1 line2} {
    global doingLine1 doingLine2 Pref

    if {$Pref(parse) != "none"} {
        comparelines $line1 $line2 res1 res2
        set dotag 0
        .t1 insert end [myforml $doingLine1] change
        .t2 insert end [myforml $doingLine2] change
        foreach i1 $res1 i2 $res2 {
            if {$dotag} {
                if {$i1 == ""} {
                    .t2 insert end $i2 new2
                } elseif {$i2 == ""} {
                    .t1 insert end $i1 new1
                } else {
                    .t1 insert end $i1 change
                    .t2 insert end $i2 change
                }
                set dotag 0
            } else {
                .t1 insert end $i1
                .t2 insert end $i2
                set dotag 1
            }
        }
        .t1 insert end "\n"
        .t2 insert end "\n"
    } else {
        .t1 insert end [myform $doingLine1 $line1] change
        .t2 insert end [myform $doingLine2 $line2] change
    }
    incr doingLine1
    incr doingLine2
}

#Process one of the change/add/delete blocks reported by diff.
#ch1data contains n1 lines of text from the left file
#ch2 is a file channel for the right file
#n1/n2 is the number of lines involved
#line1/line2 says on what lines this block starts
proc dotext {ch1data ch2 n1 n2 line1 line2} {
    global doingLine1 doingLine2 Pref mapList mapMax

    if {$n1 == 0 && $n2 == 0} {
        #All blocks has been processed. Continue until end of file.
        if {$Pref(onlydiffs) == 1} return
        while {[gets $ch2 apa] != -1} {
            .t2 insert end [myform $doingLine2 $apa]
            .t1 insert end [myform $doingLine1 $apa]
            incr doingLine2
            incr doingLine1
            incr mapMax
        }
        return
    }

    if {$n1 == 0} {set tag2 new2} else {set tag2 change}
    if {$n2 == 0} {set tag1 new1} else {set tag1 change}

    #Display all equal lines before next diff
    if {$Pref(onlydiffs) == 1 && $doingLine1 < $line1} {
        .t1 insert end "\n"
        .t2 insert end "\n"
        incr mapMax
    }
    while {$doingLine1 < $line1} {
        gets $ch2 apa
        if {$Pref(onlydiffs) == 0} {
            .t1 insert end [myform $doingLine1 $apa]
            .t2 insert end [myform $doingLine2 $apa]
            incr mapMax
        }
        incr doingLine1
        incr doingLine2
    }
    if {$doingLine2 != $line2} {
        .t1 insert end "**Bad alignment here!! $doingLine2 $line2**\n"
        .t2 insert end "**Bad alignment here!! $doingLine2 $line2**\n"
    }

    #Process the block

    if {$n1 == $n2 && ($n1 == 1 || $Pref(parse) != "block")} {
        for {set t 0} {$t < $n1} {incr t} {
            set line1 [lindex $ch1data $t]
            gets $ch2 line2
            insertMatchingLines $line1 $line2
        }
        lappend mapList $mapMax
        incr mapMax $n1
        lappend mapList $mapMax change
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
                    insertMatchingLines $line1 $line2
                    incr t1
                    incr t2
                }
                if {$c == "d"} {
                    set bepa [lindex $block1 $t1]
                    .t1 insert end [myform $doingLine1 $bepa] change
                    .t2 insert end "\n"
                    incr doingLine1
                    incr t1
                }
                if {$c == "a"} {
                    set bepa [lindex $block2 $t2]
                    .t2 insert end [myform $doingLine2 $bepa] change
                    .t1 insert end "\n"
                    incr doingLine2
                    incr t2
                }
            }
            lappend mapList $mapMax
            incr mapMax [llength $apa]
            lappend mapList $mapMax change
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
    set n1 [.t1 tag nextrange new1 $i]
    set c1 [.t1 tag nextrange change $i]
    set i [.t2 index @0,0+1line]
    set n2 [.t2 tag nextrange new2 $i]
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

#Scroll windows to previous diff
proc findPrev {} {
    set i [.t1 index @0,0]
    set n1 [.t1 tag prevrange new1 $i]
    set c1 [.t1 tag prevrange change $i]
    set i [.t2 index @0,0]
    set n2 [.t2 tag prevrange new2 $i]
    set c2 [.t2 tag prevrange change $i]

    set apa [lsort -decreasing -dictionary "$n1 $c1 $n2 $c2"]
    if {[llength $apa] != 0} {
        .t1 yview [lindex $apa 1]
        .t2 yview [lindex $apa 1]
    } else {
        .t1 yview 1.0
        .t2 yview 1.0
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
    if {![info exists oldcursor]} {
        set oldcursor [. cget -cursor]
        set oldcursor2 [.t1 cget -cursor]
    }
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

    update idletasks

    if {$RCS} {
        set differr [catch {eval exec rcsdiff $Pref(dopt) $Pref(ignore) $rightFile} diffres]
    } else {
        set differr [catch {eval exec diff $Pref(dopt) $Pref(ignore) $leftFile $rightFile} diffres]
    }

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
                    dotext "" $ch2 0 $n2 [expr {$line1 + 1}] $line2
                } c {
                    set apa [lrange $result2 $t2 [expr {$t2 + $n1 - 1}]]
                    incr t2 $n1
                    dotext $apa $ch2 $n1 $n2 $line1 $line2
                } d {
                    # lucka i right, new i left
                    set apa [lrange $result2 $t2 [expr {$t2 + $n1 - 1}]]
                    incr t2 $n1
                    dotext $apa $ch2 $n1 0 $line1 [expr {$line2 + 1}]
                }
            }
        }
    }

    dotext "" $ch2 0 0 0 0

    close $ch2
    drawMap -1
    normalCursor
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
    global mapList mapMax Pref
 
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
        map put $Pref(color$type) -to 1 $y1 $x2 $y2
    }
}

proc linewrap {gray} {
    if {$gray == "1.0"} {
        return "\n     "
    } else {
        return "\0bggray\{1.0\}\n     \0bggray\{$gray\}"
    }
}

proc printDiffs {} {
    busyCursor
    set tmpFile [file nativename ~/tcldiff.enscript]
    set tmpFile2 [file nativename ~/tcldifftmp.ps]
    set tmpFile3 [file nativename ~/tcldiff.ps]
    set ch [open $tmpFile "w"]

    set lines1 {}
    set lines2 {}

    set tdump1 [.t1 dump -tag -text 1.0 end]
    set tdump2 [.t2 dump -tag -text 1.0 end]

    foreach tdump [list $tdump1 $tdump2] \
            lineName {lines1 lines2} wrapName {wrap1 wrap2} {
        set lines {}
        set wraps {}
        set line ""
        set newline 0
        set gray 1.0
        set chars 0
        set wrapc 0
        foreach {key value index} $tdump {
            if {$key != "tagoff" && $newline == 1} {
                lappend lines $line
                lappend wraps $wrapc
                set newline 0
                set line "\0bggray\{$gray\}"
                set chars 0
                set wrapc 0
            }
            switch $key {
                text {
                    if {[string range $value end end] == "\n"} {
                        set newline 1
                        set value [string trimright $value "\n"]
                    }
                    set len [string length $value]
                    while {$chars + $len > 85} {
                        set wrap [expr {85 - $chars}]
                        set val1 [string range $value 0 [expr {$wrap - 1}]]
                        set value [string range $value $wrap end]
                        append line $val1
                        append line [linewrap $gray]
                        set chars 5
                        incr wrapc
                        set len [string length $value]
                    }
                    append line $value
                    incr chars $len
                }
                tagon {
                    if {$value == "change"} {
                        append line "\0bggray\{.6\}"
                        set gray 0.6
                    } else {
                        append line "\0bggray\{.8\}"
                        set gray 0.8
                    }
                }
                tagoff {
                    append line "\0bggray\{1.0\}"
                    set gray 1.0
                }
            }
        }
        set $lineName $lines
        set $wrapName $wraps
    }

    set wraplines1 {}
    set wraplines2 {}

    foreach l1 $lines1 l2 $lines2 w1 $wrap1 w2 $wrap2 {
        if {$w1 > 0} {
            set apa [split $l1 "\n"]
            set wraplines1 [concat $wraplines1 $apa]
        } else {
            lappend wraplines1 $l1
        }
        if {$w2 > 0} {
            set apa [split $l2 "\n"]
            set wraplines2 [concat $wraplines2 $apa]
        } else {
            lappend wraplines2 $l2
        }
        if {$w1 > $w2} {
            for {set t $w2} {$t < $w1} {incr t} {
                lappend wraplines2 ""
            }
        } elseif {$w2 > $w1} {
            for {set t $w1} {$t < $w2} {incr t} {
                lappend wraplines1 ""
            }
        }
    }

    set len1 [llength $wraplines1]
    set len2 [llength $wraplines2]

    set i1 0
    set i2 0

    while {$i1 < $len1 && $i2 < $len2} {
        for {set i 0} {$i < 66 && $i1 < $len1} {incr i ; incr i1} {
            puts $ch [lindex $wraplines1 $i1]
        }
        puts -nonewline $ch "\f"
        for {set i 0} {$i < 66 && $i2 < $len2} {incr i ; incr i2} {
            puts $ch [lindex $wraplines2 $i2]
        }
        puts -nonewline $ch "\f"
    }

    close $ch

    catch {exec enscript -c -B -e -p $tmpFile2 $tmpFile}
    catch {exec mpage -aA2P $tmpFile2 > $tmpFile3}

    normalCursor

    destroy .dp
    toplevel .dp
    wm title .dp "Diff Print"
    button .dp.b -text Close -command {destroy .dp}
    label .dp.l -anchor w -justify left -text "The following files have\
            been created:\n$tmpFile\nInput file to enscript.\
            \n$tmpFile2\nCreated with 'enscript -c -B -e -p $tmpFile2\
            $tmpFile'\n$tmpFile3\nCreated with 'mpage -aA2P $tmpFile2 >\
            $tmpFile3'"
    pack .dp.b -side bottom
    pack .dp.l -side top
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
}

proc applyColor {} {
    global Pref

    .t1 tag configure new1 -foreground $Pref(colornew1) -background $Pref(bgnew1)
    .t1 tag configure change -foreground $Pref(colorchange) -background $Pref(bgchange)
    .t2 tag configure new2 -foreground $Pref(colornew2) -background $Pref(bgnew2)
    .t2 tag configure change -foreground $Pref(colorchange) -background $Pref(bgchange)
}

#Build the main window
proc makeDiffWin {} {
    global Pref tcl_platform debug
    eval destroy [winfo children .]

    frame .f
    grid .f - - - -row 0 -sticky news

    menubutton .mf -text File -underline 0 -menu .mf.m
    menu .mf.m
    if {$debug == 1} {
        .mf.m add command -label "Redo Diff" -underline 5 -command doDiff
    } else {
        .mf.m add command -label "Redo Diff" -underline 5 -command doDiff -state disabled
    }
    .mf.m add separator
    .mf.m add command -label "Open Both" -underline 0 -command openBoth
    .mf.m add command -label "Open Left File" -command openLeft
    .mf.m add command -label "Open Right File" -command openRight
    if {$tcl_platform(platform) == "unix"} {
        .mf.m add command -label "RCSDiff" -underline 0 -command openRCS
        .mf.m add separator
        .mf.m add command -label "Print" -underline 0 -command printDiffs
    }
    .mf.m add separator
    .mf.m add command -label "Quit" -command exit

    menubutton .mo -text Options -underline 0 -menu .mo.m
    menu .mo.m
    .mo.m add cascade -label Fontsize -underline 0 -menu .mo.mf
    .mo.m add cascade -label Ignore -underline 0 -menu .mo.mi
    .mo.m add cascade -label Parse -underline 0 -menu .mo.mp
    .mo.m add command -label Colours -underline 0 -command makePrefWin
    .mo.m add checkbutton -label "Diffs only" -variable Pref(onlydiffs)
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
    .mo.mp add separator
    .mo.mp add checkbutton -label "Use 2nd stage" -variable Pref(extralineparse)

    menubutton .mh -text Help -underline 0 -menu .mh.m
    menu .mh.m
    .mh.m add command -label "Help" -command makeHelpWin
    .mh.m add command -label "About" -command makeAboutWin

    button .bfn -text "Next Diff" -relief raised -command findNext
    button .bfp -text "Prev Diff" -relief raised -command findPrev
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

    applyColor

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
        if {$tcl_platform(platform) == "windows"} {
            .md.m add checkbutton -label Console -variable consolestate \
                    -onvalue show -offvalue hide -command {console $consolestate}
            .md.m add separator
        }
        .md.m add command -label "Stack trace" -command {bgerror Debug}
        .md.m add separator
        .md.m add command -label "Reread Source" -command {source diff.tcl}
        .md.m add separator
        .md.m add command -label "Redraw Window" -command {makeDiffWin}
        
        pack .mf .mo .mh .md -in .f -side left
    } else {
        pack .mf .mo .mh -in .f -side left
    }
    pack .bfn .bfp .eo .lo -in .f -side right
}

proc applyPref {} {
    global Pref TmpPref

    array set Pref [array get TmpPref]
    applyColor
}

proc testColor {} {
    global TmpPref
    
    .pr.fc.t1 tag configure change -foreground $TmpPref(colorchange) -background $TmpPref(bgchange)
    .pr.fc.t2 tag configure new1 -foreground $TmpPref(colornew1) -background $TmpPref(bgnew1)
    .pr.fc.t3 tag configure new2 -foreground $TmpPref(colornew2) -background $TmpPref(bgnew2)
}

proc selColor {name} {
    global TmpPref

    set t [tk_chooseColor -parent .pr -initialcolor $TmpPref($name)]
    if {$t != ""} {
        set TmpPref($name) $t
    }
}

proc makePrefWin {} {
    global Pref TmpPref

    array set TmpPref [array get Pref]

    destroy .pr
    
    toplevel .pr
    wm title .pr "Diff Preferences"

    frame .pr.fc -borderwidth 1 -relief solid
    label .pr.fc.l1 -text Colours -anchor w
    label .pr.fc.l2 -text Text -anchor w
    label .pr.fc.l3 -text Background -anchor w

    entry .pr.fc.e1 -textvariable "TmpPref(colorchange)" -width 10
    entry .pr.fc.e2 -textvariable "TmpPref(colornew1)" -width 10
    entry .pr.fc.e3 -textvariable "TmpPref(colornew2)" -width 10
    entry .pr.fc.e4 -textvariable "TmpPref(bgchange)" -width 10
    entry .pr.fc.e5 -textvariable "TmpPref(bgnew1)" -width 10
    entry .pr.fc.e6 -textvariable "TmpPref(bgnew2)" -width 10

    button .pr.fc.b1 -text Sel -command "selColor colorchange"
    button .pr.fc.b2 -text Sel -command "selColor colornew1"
    button .pr.fc.b3 -text Sel -command "selColor colornew2"
    button .pr.fc.b4 -text Sel -command "selColor bgchange"
    button .pr.fc.b5 -text Sel -command "selColor bgnew1"
    button .pr.fc.b6 -text Sel -command "selColor bgnew2"

    text .pr.fc.t1 -width 12 -height 1 -font "Courier 8"
    text .pr.fc.t2 -width 12 -height 1 -font "Courier 8"
    text .pr.fc.t3 -width 12 -height 1 -font "Courier 8"
    .pr.fc.t1 tag configure change -foreground $TmpPref(colorchange) -background $TmpPref(bgchange)
    .pr.fc.t2 tag configure new1 -foreground $TmpPref(colornew1) -background $TmpPref(bgnew1)
    .pr.fc.t3 tag configure new2 -foreground $TmpPref(colornew2) -background $TmpPref(bgnew2)
    .pr.fc.t1 insert end "Changed text" change
    .pr.fc.t2 insert end "Deleted text" new1
    .pr.fc.t3 insert end "Added text" new2

    button .pr.b1 -text "Apply" -command applyPref
    button .pr.b2 -text "Test" -command testColor
    button .pr.b3 -text "Close" -command {destroy .pr}

    grid .pr.fc.l1 .pr.fc.l2 x .pr.fc.l3 x -row 0 -sticky ew -padx 1 -pady 1
    grid .pr.fc.t1 .pr.fc.e1 .pr.fc.b1 .pr.fc.e4 .pr.fc.b4 -row 1 -sticky nsew -padx 1 -pady 1
    grid .pr.fc.t2 .pr.fc.e2 .pr.fc.b2 .pr.fc.e5 .pr.fc.b5 -row 2 -sticky nsew -padx 1 -pady 1
    grid .pr.fc.t3 .pr.fc.e3 .pr.fc.b3 .pr.fc.e6 .pr.fc.b6 -row 3 -sticky nsew -padx 1 -pady 1
    grid columnconfigure .pr.fc {1 2} -weight 1

    pack .pr.fc -side top -fill x
    pack .pr.b1 .pr.b2 .pr.b3 -side left -expand 1 -fill x
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
    global Pref
    destroy .he

    toplevel .he
    wm title .he "Diff.tcl Help"
    text .he.t -width 82 -height 35 -wrap word -yscrollcommand ".he.sb set"\
            -font "Courier 8"
    scrollbar .he.sb -orient vert -command ".he.t yview"
    button .he.b -text "Close" -command "destroy .he"
    pack .he.b -side bottom
    pack .he.sb -side right -fill y
    pack .he.t -side left -expand y -fill both
    .he.t tag configure new1 -foreground $Pref(colornew1) -background $Pref(bgnew1)
    .he.t tag configure new2 -foreground $Pref(colornew2) -background $Pref(bgnew2)
    .he.t tag configure change -foreground $Pref(colorchange) -background $Pref(bgchange)
    .he.t tag configure ul -underline 1
    
    .he.t insert end {\

} "" {Commands} ul {

File Menu
  Redo Diff      : Run diff again on the same files.
  Open Both      : Select two files, run diff.
  Open Left File : Select a file for left window, run diff 
  Open Right File: Select a file for right window, run diff
  RCSDiff        : (UNIX only) Select one file and run rcsdiff.
  Print          : (UNIX only) Experimental print function.
                   It currently creates a postscript file ~/tcldiff.ps
  Quit           : Guess

Options Menu
  Fontsize : Select fontsize for the two main text windows
  Ignore   : Diff options for handling whitespace
  Parse    : Additional parsing made by diff.tcl to improve the display.
             See examples below.
             Nothing: No parsing made.
             Lines  : When there is a changed block with the same number
                      of lines in both right and left files, diff.tcl
                      compares corresponding lines and tries to highlight
                      only the part that has been changed.
             Blocks : When the number of lines in a changed block is not
                      the same in both files, diff.tcl tries to find lines
                      that look the same and place them abreast.
             The Char and Word options selects if the line parsing should
             highlight full words only, or check single characters.
             2nd stage  : More thorough parsing of a line.
  Diffs only : Only differing lines will be displayed.
  Colours  : Choose highlight colours.
  Save default: Save current option settings in ~/.diffrc

Diff Options Field: Any text written here will be passed to diff.

Prev Diff Button: Scrolls to the previous differing block, or to the top
                  if there are no more diffs.
Next Diff Button: Scrolls to the next differing block, or to the bottom
                  if there are no more diffs.

Equal sign: Above the vertical scrollbar, a "=" will appear if the files
            are equal.

} "" {Examples of effects of parse options.} ul {

Below are two example files, and five different results when using
different options with those files.

Left file:                       Right file:
NET '/I$1/N$1454' IC2-15 IC5-7   NET '/I$1/N$1454' IC1-4 IC2-15 IC5-2 IC5-7
NET '/I$1/N$1455' IC2-14 IC6-8   NET '/I$1/N$1456' IC2-12            
NET '/I$1/N$1456' IC2-13 IC2-12  NET '/I$1/N$1457' IC2-11 IC6-7      
NET '/I$1/N$1457' IC2-11 IC6-7   NET '/I$1/N$1458' IC2-9            
NET '/I$1/N$1458' IC2-10       

}

.he.t insert end "Example 1. No parsing.\n"
.he.t insert end {1: NET '/I$1/N$1454' IC2-15 IC5-7   1: NET '/I$1/N$1454' IC1-4 IC2-15 IC5-2 IC5-7
} change
.he.t insert end {2: NET '/I$1/N$1455' IC2-14 IC6-8   2: NET '/I$1/N$1456' IC2-12            
} change
.he.t insert end {3: NET '/I$1/N$1456' IC2-13 IC2-12  } change
.he.t insert end {
4: NET '/I$1/N$1457' IC2-11 IC6-7   3: NET '/I$1/N$1457' IC2-11 IC6-7
}
.he.t insert end {5: NET '/I$1/N$1458' IC2-10         4: NET '/I$1/N$1458' IC2-9             
} change 

.he.t insert end "\n"

.he.t insert end "Example 2. Lines and characters\n"
.he.t insert end {1: NET '/I$1/N$1454' IC2-15 IC5-7   1: NET '/I$1/N$1454' IC1-4 IC2-15 IC5-2 IC5-7
} change
.he.t insert end {2: NET '/I$1/N$1455' IC2-14 IC6-8   2: NET '/I$1/N$1456' IC2-12            
} change
.he.t insert end {3: NET '/I$1/N$1456' IC2-13 IC2-12  } change
.he.t insert end {
4: NET '/I$1/N$1457' IC2-11 IC6-7   3: NET '/I$1/N$1457' IC2-11 IC6-7
}
.he.t insert end {5: } change {NET '/I$1/N$1458' IC2-} "" {10} change {         } "" {4: } change {NET '/I$1/N$1458' IC2-} "" {9} change "\n"

.he.t insert end "\n"

.he.t insert end "Example 3. Blocks and characters\n"

.he.t insert end {1: } change {NET '/I$1/N$1454' IC} "" {2-15} change { IC5-7   } "" {1: } change {NET '/I$1/N$1454' IC} "" {1-4 IC2-15 IC5-2} change " IC5-7\n"
.he.t insert end {2: NET '/I$1/N$1455' IC2-14 IC6-8   } change "\n" ""
.he.t insert end {3: } change {NET '/I$1/N$1456' IC2-1} "" {3 IC2-1} new1 {2  } "" {2: } change {NET '/I$1/N$1456' IC2-12
}
.he.t insert end {4: NET '/I$1/N$1457' IC2-11 IC6-7   3: NET '/I$1/N$1457' IC2-11 IC6-7
}
.he.t insert end {5: } change {NET '/I$1/N$1458' IC2-} "" {10} change {         } "" {4: } change {NET '/I$1/N$1458' IC2-} "" {9} change "\n"

.he.t insert end "\n"

.he.t insert end "Example 4. Blocks and words\n"

.he.t insert end {1: } change {NET '/I$1/N$1454' } "" {IC2-15} change { IC5-7   } "" {1: } change {NET '/I$1/N$1454' } "" {IC1-4 IC2-15 IC5-2} change " IC5-7\n"
.he.t insert end {2: NET '/I$1/N$1455' IC2-14 IC6-8   } change "\n" ""
.he.t insert end {3: } change {NET '/I$1/N$1456' } "" {IC2-13 } new1 {IC2-12  } "" {2: } change {NET '/I$1/N$1456' IC2-12
}
.he.t insert end {4: NET '/I$1/N$1457' IC2-11 IC6-7   3: NET '/I$1/N$1457' IC2-11 IC6-7
}
.he.t insert end {5: } change {NET '/I$1/N$1458' } "" {IC2-10} change {         } "" {4: } change {NET '/I$1/N$1458' } "" {IC2-9} change "\n"

.he.t insert end "\n"

.he.t insert end "Example 5. Blocks, words and 2nd stage\n"

.he.t insert end {1: } change {NET '/I$1/N$1454' IC2-15 IC5-7   } "" {1: } change {NET '/I$1/N$1454' } "" {IC1-4 } new2 {IC2-15} "" { IC5-2} new2 " IC5-7\n"
.he.t insert end {2: NET '/I$1/N$1455' IC2-14 IC6-8   } change "\n" ""
.he.t insert end {3: } change {NET '/I$1/N$1456' } "" {IC2-13 } new1 {IC2-12  } "" {2: } change {NET '/I$1/N$1456' IC2-12
}
.he.t insert end {4: NET '/I$1/N$1457' IC2-11 IC6-7   3: NET '/I$1/N$1457' IC2-11 IC6-7
}
.he.t insert end {5: } change {NET '/I$1/N$1458' } "" {IC2-10} change {         } "" {4: } change {NET '/I$1/N$1458' } "" {IC2-9} change "\n"

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
        } elseif {$arg == "-2nd"} {
            set Pref(extralineparse) 1
        } elseif {$arg == "-no2nd"} {
            set Pref(extralineparse) 0
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
                after idle doDiff
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
            after idle doDiff
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
    set Pref(extralineparse) 1
    set Pref(colorchange) red
    set Pref(colornew1) darkgreen
    set Pref(colornew2) blue
    set Pref(bgchange) gray
    set Pref(bgnew1) gray
    set Pref(bgnew2) gray
    set Pref(onlydiffs) 0

    if {[file exists "~/.diffrc"]} {
        source "~/.diffrc"
    }
}

if {![winfo exists .f]} {
    getOptions
    makeDiffWin
    update idletasks
    parseCommandLine
}
