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
#                                    file, and if found, runs in RCS mode.
#
#             Options for diff.tcl:
#
#             -nodiff  : Normally, if there are enough information on the
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
#     1.4     DA-PS    990210   Bug-fix in "Ignore nothing"
#                               Bug-fix in file handling
#                               Improved RCS handling.
#     1.5     DA-PS    990623   Bug-fix and improvement in block parsing
#                               Added font selection
#                               Added "diff server" functionality
#                               Split text windows in lineno/text
#                               Added "Mark last" option
#     1.6     DA-PS    000131   Added scroll-keys
#                               Bug-fixes in scroll map and printing
#     1.7     DA-PS    000427   Restricted parsing of large blocks.
#                               Fixed bug with spaces in file names.
#                               Regular screen updates during processing.
#                               Added CVS support.
#
#-----------------------------------------------
# the next line restarts using wish \
exec wish "$0" "$@"

set debug 1
set diffver "Version 1.8b  000824"
set tmpcnt 0
set tmpfiles {}
set thisscript [file join [pwd] [info script]]
set thisdir [file dirname $thisscript]

if {$tcl_platform(platform) == "windows"} {
    cd $thisdir
    package require dde
}

# Support for FreeWrap. If diff.exe is wrapped, copy it so we can use it.
set diffexe diff
if {[info exists _freewrap_contents] && [file exists diff.exe]} {
    set inch [open diff.exe r]
    if {[info exists env(TEMP)]} {
        set diffexe [file join $env(TEMP) diff.exe]
    } elseif {[info exists env(TMP)]} {
        set diffexe [file join $env(TMP) diff.exe]
    } else {
        set diffexe [file join c:/ diff.exe]
    }
    set outch [open $diffexe w]
    fconfigure $inch -translation binary
    fconfigure $outch -translation binary
    puts -nonewline $outch [read $inch]
    close $inch
    close $outch
    set debug 0
}

proc cleanupAndExit {} {
    if {$::diffexe != "diff"} {
        file delete $::diffexe
    }
    cleartmp
    exit
}

proc myform {lineNo text} {
    return [format "%3d: %s\n" $lineNo $text]
}

proc myforml {lineNo} {
    return [format "%3d: \n" $lineNo]
}

proc maxabs {a b} {
    return [expr {abs($a) > abs($b) ? $a : $b}]
}

proc tmpfile {} {
    global tmpcnt tmpfiles
    incr tmpcnt
    set name "tmpd[pid]a$tmpcnt"
    lappend tmpfiles $name
    return $name
}

proc cleartmp {} {
    global tmpfiles
    foreach f $tmpfiles {
        file delete $f
    }
    set tmpfiles {}
}

# 2nd stage line parsing
# Recursively look for common substrings in strings s1 and s2
##syntax compareMidString x x n n x?
proc compareMidString {s1 s2 res1Name res2Name {test 0}} {
    global Pref
    upvar $res1Name res1
    upvar $res2Name res2

    set len1 [string length $s1]
    set len2 [string length $s2]

    # Is s1 a substring of s2 ?
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

    # Is s2 a substring of s1 ?
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

    # Are they too short to be considered ?
    if {$len1 < 4 || $len2 < 4} {
        set res1 [list $s1]
        set res2 [list $s2]
        return
    }

    set foundlen -1
    set minlen 2 ;# The shortest common substring we detect is 3 chars

    # Find the longest string common to both strings
    for {set t 0 ; set u $minlen} {$u < $len1} {incr t ; incr u} {
        set i [string first [string range $s1 $t $u] $s2]
        if {$i >= 0} {
            for {set p1 [expr {$u + 1}]; set p2 [expr {$i + $minlen + 1}]} \
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
}

# Compare two lines to find inequalities to highlight.
# The return value is, for each line, a list where the first, third etc.
# element is equal between the lines. The second, fourth etc. will be
# highlighted.
##syntax compareLines x x n n x?
proc compareLines {line1 line2 res1Name res2Name {test 0}} {
    global Pref
    upvar $res1Name res1
    upvar $res2Name res2

    if {$Pref(ignore) != " "} {
        # Skip white space in both ends

        set apa1 [string trimleft $line1]
        set leftp1 [expr {[string length $line1] - [string length $apa1]}]
        set mid1 [string trimright $line1]

        set apa2 [string trimleft $line2]
        set leftp2 [expr {[string length $line2] - [string length $apa2]}]
        set mid2 [string trimright $line2]
    } else {
        # If option "ignore nothing" is selected
        set apa1 $line1
        set leftp1 0
        set mid1 $line1
        set apa2 $line2
        set leftp2 0
        set mid2 $line2
    }

    # Check for matching left chars/words.
    # leftp1 and leftp2 will be the indicies of the first difference

    set len1 [string length $apa1]
    set len2 [string length $apa2]
    set len [expr {$len1 < $len2 ? $len1 : $len2}]
    for {set t 0; set s 0; set flag 0} {$t < $len} {incr t} {
        if {[set c [string index $apa1 $t]] != [string index $apa2 $t]} {
            incr flag 2
            break
        }
        if {$c == " "} {
            set s $t
            set flag 1
        }
    }

    if {$Pref(lineparsewords) == 0 || $test != 0} {
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

    # Check for matching right chars/words.
    # t1 and t2 will be the indicies of the last difference

    set len1 [string length $mid1]
    set len2 [string length $mid2]

    set t1 [expr {$len1 - 1}]
    set t2 [expr {$len2 - 1}]
    set s1 $t1
    set s2 $t2
    set flag 0
    for {} {$t1 >= $leftp1 && $t2 >= $leftp2} {incr t1 -1; incr t2 -1} {
        if {[set c [string index $mid1 $t1]] != [string index $mid2 $t2]} {
            incr flag 2
            break
        }
        if {$c == " "} {
            set s1 $t1
            set s2 $t2
            set flag 1
        }
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

    # Make the result
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

# Compare two lines and rate how much they resemble each other.
proc compareLines2 {line1 line2} {
    compareLines $line1 $line2 res1 res2 1

    # Collect identical pieces and different pieces
    set sames {}
    set diffs1 {}
    set diffs2 {}
    foreach {same diff} $res1 {
        lappend sames $same
        if {$diff != ""} {
            lappend diffs1 $diff
        }
    }
    foreach {same diff} $res2 {
        if {$diff != ""} {
            lappend diffs2 $diff
        }
    }
    set sumsame 0
    set sumdiff1 0
    set sumdiff2 0
    foreach same $sames {
        set apa [string length [string trim $same]]
        
        incr sumsame [expr {$apa * $apa}]
    }
    foreach diff $diffs1 {
        set apa [string length $diff]
        incr sumdiff1 $apa
    }
    foreach diff $diffs2 {
        set apa [string length $diff]
        incr sumdiff2 $apa
    }
#    puts "S $sumsame D $sumdiff1 D $sumdiff2"
    return [expr {$sumsame - [maxabs $sumdiff1 $sumdiff2]}]
}

# Decide how to display change blocks
proc oldcompareblocks {block1 block2} {
    set size1 [llength $block1]
    set size2 [llength $block2]

    # Swap if block1 is bigger
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

    # Collect statistics
    set result {}
    set scores {}
    foreach line1 $block1 {
        set bestscore -100000
        set bestline 0
        set i 0
        foreach line2 $block2 {
            set x [compareLines2 $line1 $line2]
            if {$x > $bestscore} {
                set bestscore $x
                set bestline $i
            }
            incr i
        }
        lappend result $bestline
        lappend scores $bestscore
    }

    # If result is in order, no problem.
    # Otherwise, try to adjust result to make it ordered
    if {$size1 > 1} {
        set bad 1
        for {set loop 0} {[llength $bad] != 0 && $loop < 2} {incr loop} {
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

                # Try to move the one with lowest score first
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

# Decide how to display change blocks
# This tries to match the lines that resemble each other and put them
# next to each other. The algorithm for doing it would need some work.
proc compareblocks {block1 block2} {
    set size1 [llength $block1]
    set size2 [llength $block2]

    if {$size1 * $size2 > 1000} {
        puts "Diff warning: Analyzing a large block. ($size1 $size2)"
        update idletasks
    }

    # Swap if block1 is bigger
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

    # Collect statistics
    array set scores {}
    set j 0
    set bestsum 0
    foreach line1 $block1 {
        set bestscore -100000
        set bestline 0
        set i 0
        foreach line2 $block2 {
            set x [compareLines2 $line1 $line2]
            set scores($j,$i) $x
#            puts "Score $j $i : $x"
            if {$x > $bestscore} {
                set bestscore $x
                set bestline $i
            }
            incr i
        }
#        puts "Best for $j is $bestline : $bestscore"
        set origresult($j) $bestline
        set scores(best,$j) $bestscore
        incr bestsum $bestscore
        incr j
    }

    array set bestresult [array get origresult]

    # If result is in order, no problem.
    # Otherwise, try to adjust result to make it ordered
    if {$size1 > 1} {
        set bestscoresum -100000
        while {1} {
            array set result [array get origresult]
            for {set i 0} {$i < $size1} {incr i} {
                set mark($i) 0
            }
            while {1} {
                set besti 0
                set bestscore -100000
                set order 1
                for {set i 0} {$i < $size1} {incr i} {
                    if {$mark($i) == 0} {
                        for {set j [expr {$i + 1}]} {$j < $size1} {incr j} {
                            if {$mark($j) == 0} break
                        }
                        if {$j < $size1 && $result($i) >= $result($j)} {
                            set order 0
                        }
                        set x $scores(best,$i)
                        if {$x > $bestscore} {
                            set bestscore $x
                            set besti $i
                        }
                    }
                }
#                puts "Best $besti order $order sc $bestscore"
                if {$order} break
                set mark($besti) 1
                set bestr $result($besti)
                for {set i 0} {$i < $besti} {incr i} {
                    if {$mark($i) == 0 && $result($i) >= $bestr} {
                        set mark($i) 2
                    }
                }
                for {set i [expr {$besti + 1}]} {$i < $size1} {incr i} {
                    if {$mark($i) == 0 && $result($i) <= $bestr} {
                        set mark($i) 2
                    }
                }
            }
            set prev $size2
            for {set i [expr {$size1 - 1}]} {$i >= 0} {incr i -1} {
                if {$mark($i) != 2} {
                    set prev $result($i)
                } else {
                    set high($i) [expr {$prev - 1}]
                }
            }
            set prev -1
            for {set i 0} {$i < $size1} {incr i} {
                if {$mark($i) != 2} {
                    set prev $result($i)
                } else {
                    if {$high($i) > $prev} {
                        incr prev
                        set result($i) $prev
                    } else {
                        set result($i) -1
                    }
                }
            }
            set scoresum 0
            for {set i 0} {$i < $size1} {incr i} {
                set j $result($i)
                if {[info exists scores($i,$j)]} {
#                    puts "Score: $i $j $scores($i,$j)"
                    incr scoresum $scores($i,$j)
                }
            }
#            puts "Scoresum: $scoresum ($bestsum)"
            if {$scoresum > $bestscoresum} {
                array set bestresult [array get result]
                set bestscoresum $scoresum
                if {$bestscoresum >= (3 * $bestsum / 4)} {
                    break
                }
                # If the result seems too bad, try again but
                # ignore the most awkwardly placed line.
                set mostp -1
                set mosti 0
                for {set i 0} {$i < $size1} {incr i} {
                    if {$mark($i) == 1} {
                        if {abs($result($i) - $i) > $mostp} {
                            set mostp [expr {abs($result($i) - $i)}]
                            set mosti $i
                        }
                    }
                }
#                puts "Most $mosti $mostp"
                set scores(best,$mosti) 0
            } else {
                break
            }
        }
    }

    array set result [array get bestresult]

    set apa {}
    set t1 0
    set t2 0
    while {$t1 < $size1 || $t2 < $size2} {
        if {$t1 < $size1} {
            set r $result($t1)
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

# Insert lineno and text
proc insert {n line text {tag {}}} {
    .ft$n.tl insert end [myforml $line] $tag
    .ft$n.tt insert end "$text\n" $tag
}

proc emptyline {n} {
    .ft$n.tl insert end "\n"
    .ft$n.tt insert end "\n"
}

# Insert one line in each text widget.
# Mark them as changed, and optionally parse them.
proc insertMatchingLines {line1 line2} {
    global doingLine1 doingLine2 Pref

    if {$Pref(parse) != 0} {
        compareLines $line1 $line2 res1 res2
        set dotag 0
        set n [maxabs [llength $res1] [llength $res2]]
        .ft1.tl insert end [myforml $doingLine1] change
        .ft2.tl insert end [myforml $doingLine2] change
        set new1 new1
        set new2 new2
        set change change
        foreach i1 $res1 i2 $res2 {
            incr n -1
            if {$dotag} {
                if {$n == 1 && $Pref(marklast)} {
                    lappend new1 last
                    lappend new2 last
                    lappend change last
                }
                if {$i1 == ""} {
                    .ft2.tt insert end $i2 $new2
                } elseif {$i2 == ""} {
                    .ft1.tt insert end $i1 $new1
                } else {
                    .ft1.tt insert end $i1 $change
                    .ft2.tt insert end $i2 $change
                }
                set dotag 0
            } else {
                .ft1.tt insert end $i1
                .ft2.tt insert end $i2
                set dotag 1
            }
        }
        .ft1.tt insert end "\n"
        .ft2.tt insert end "\n"
    } else {
        insert 1 $doingLine1 $line1 change
        insert 2 $doingLine2 $line2 change
    }
    incr doingLine1
    incr doingLine2
}

# Process one of the change/add/delete blocks reported by diff.
# ch1 is a file channel for the left file
# ch2 is a file channel for the right file
# n1/n2 is the number of lines involved
# line1/line2 says on what lines this block starts
proc dotext {ch1 ch2 n1 n2 line1 line2} {
    global doingLine1 doingLine2 Pref mapList mapMax

    if {$n1 == 0 && $n2 == 0} {
        # All blocks have been processed. Continue until end of file.
        if {$Pref(onlydiffs) == 1} return
        while {[gets $ch2 apa] != -1} {
            insert 2 $doingLine2 $apa
            incr doingLine2
            incr mapMax
        }
        while {[gets $ch1 apa] != -1} {
            insert 1 $doingLine1 $apa
            incr doingLine1
        }
        return
    }

    if {$n1 == 0} {set tag2 new2} else {set tag2 change}
    if {$n2 == 0} {set tag1 new1} else {set tag1 change}

    # Display all equal lines before next diff
    if {$Pref(onlydiffs) == 1 && $doingLine1 < $line1} {
        emptyline 1
        emptyline 2
        incr mapMax
    }
    while {$doingLine1 < $line1} {
        gets $ch1 apa
        gets $ch2 bepa
        if {$Pref(onlydiffs) == 0} {
            insert 1 $doingLine1 $apa
            insert 2 $doingLine2 $bepa
            incr mapMax
        }
        incr doingLine1
        incr doingLine2
    }
    if {$doingLine2 != $line2} {
        .ft1.tt insert end "**Bad alignment here!! $doingLine2 $line2**\n"
        .ft2.tt insert end "**Bad alignment here!! $doingLine2 $line2**\n"
        .ft1.tl insert end "\n"
        .ft2.tl insert end "\n"
    }

    # Process the block

    if {$n1 == $n2 && ($n1 == 1 || $Pref(parse) < 2)} {
        for {set t 0} {$t < $n1} {incr t} {
            gets $ch1 line1
            gets $ch2 line2
            insertMatchingLines $line1 $line2
        }
        lappend mapList $mapMax
        incr mapMax $n1
        lappend mapList $mapMax change
    } else {
        if {$n1 != 0 && $n2 != 0 && $Pref(parse) >= 2 && \
                ($n1 * $n2 < 1000 || $Pref(parse) == 3)} {
            set block1 {}
            for {set t 0} {$t < $n1} {incr t} {
                gets $ch1 apa
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
                    .ft1.tl insert end [myforml $doingLine1] change
                    .ft1.tt insert end "$bepa\n" new1
                    emptyline 2
                    incr doingLine1
                    incr t1
                }
                if {$c == "a"} {
                    set bepa [lindex $block2 $t2]
                    .ft2.tl insert end [myforml $doingLine2] change
                    .ft2.tt insert end "$bepa\n" new2
                    emptyline 1
                    incr doingLine2
                    incr t2
                }
            }
            lappend mapList $mapMax
            incr mapMax [llength $apa]
            lappend mapList $mapMax change
        } else {
            for {set t 0} {$t < $n1} {incr t} {
                gets $ch1 apa
                insert 1 $doingLine1 $apa $tag1
                incr doingLine1
            }
            for {set t 0} {$t < $n2} {incr t} {
                gets $ch2 apa
                insert 2 $doingLine2 $apa $tag2
                incr doingLine2
            }
            if {$n1 < $n2} {
                for {set t $n1} {$t < $n2} {incr t} {
                    emptyline 1
                }
                lappend mapList $mapMax
                incr mapMax $n2
                lappend mapList $mapMax $tag2
            } elseif {$n2 < $n1} {
                for {set t $n2} {$t < $n1} {incr t} {
                    emptyline 2
                }
                lappend mapList $mapMax
                incr mapMax $n1
                lappend mapList $mapMax $tag1
            }
        }
    }
}

# Scroll windows to next diff
proc findNext {} {
    set i [.ft1.tt index @0,0+1line]
    set n1 [.ft1.tt tag nextrange new1 $i]
    set c1 [.ft1.tt tag nextrange change $i]
    set i [.ft2.tt index @0,0+1line]
    set n2 [.ft2.tt tag nextrange new2 $i]
    set c2 [.ft2.tt tag nextrange change $i]

    # Below, the 'list' command is not used because I want the list
    # "flattened" before the sort.
    set apa [lsort -dictionary "$n1 $c1 $n2 $c2"]

    if {[llength $apa] != 0} {
        set apa [lindex $apa 0]
    } else {
        set apa end
    }

    foreach w {.ft1.tl .ft1.tt .ft2.tl .ft2.tt} {
        $w yview $apa
    }
}

# Scroll windows to previous diff
proc findPrev {} {
    set i [.ft1.tt index @0,0]
    set n1 [.ft1.tt tag prevrange new1 $i]
    set c1 [.ft1.tt tag prevrange change $i]
    set i [.ft2.tt index @0,0]
    set n2 [.ft2.tt tag prevrange new2 $i]
    set c2 [.ft2.tt tag prevrange change $i]

    # Below, the 'list' command is not used because I want the list
    # "flattened" before the sort.
    set apa [lsort -decreasing -dictionary "$n1 $c1 $n2 $c2"]
    if {[llength $apa] != 0} {
        set apa [lindex $apa 1]
    } else {
        set apa 1.0
    }

    foreach w {.ft1.tl .ft1.tt .ft2.tl .ft2.tt} {
        $w yview $apa
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
        set oldcursor2 [.ft1.tt cget -cursor]
    }
    . config -cursor watch
    foreach w {.ft1.tl .ft1.tt .ft2.tl .ft2.tt} {
        $w config -cursor watch
    }
}

proc normalCursor {} {
    global oldcursor oldcursor2
    . config -cursor $oldcursor
    foreach w {.ft1.tl .ft1.tt .ft2.tl .ft2.tt} {
        $w config -cursor $oldcursor2
    }
}

proc prepareRCS {} {
    global leftFile rightFile RCSFile leftLabel rightLabel Pref RCSmode

    set revs {}
    set opts {}
    set Pref(old_dopt) $Pref(dopt)

    set nextIsRev 0
    foreach opt $Pref(dopt) {
        if {$nextIsRev} {
            lappend revs $opt
            set nextIsRev 0
        } elseif {[string equal "-r" $opt]} {
            set nextIsRev 1
        } elseif {[string match "-r*" $opt]} {
            lappend revs [string range $opt 2 end]
        } else {
            lappend opts $opt
        }
    }

    switch [llength $revs] {
        0 {
            set leftFile [tmpfile]
            set rightLabel $RCSFile
            set rightFile $RCSFile

            if {$RCSmode == 2} {
                set leftLabel "$RCSFile (CVS)"
                catch {exec cvs update -p \
                        [file nativename $RCSFile] > $leftFile}
            } else {
                set leftLabel "$RCSFile (RCS)"
                catch {exec co -p [file nativename $RCSFile] > $leftFile}
            }
        }
        1 {
            set r [lindex $revs 0]
            set leftFile [tmpfile]
            set rightLabel $RCSFile
            set rightFile $RCSFile

            if {$RCSmode == 2} {
                set leftLabel "$RCSFile (CVS $r)"
                catch {exec cvs update -p -r $r \
                        [file nativename $RCSFile] > $leftFile}
            } else {
                set leftLabel "$RCSFile (RCS $r)"
                catch {exec co -p$r [file nativename $RCSFile] > $leftFile}
            }
        }
        default {
            set r1 [lindex $revs 0]
            set r2 [lindex $revs 1]
            set leftFile [tmpfile]
            set rightFile [tmpfile]

            if {$RCSmode == 2} {
                set leftLabel "$RCSFile (CVS $r1)"
                set rightLabel "$RCSFile (CVS $r2)"
                catch {exec cvs update -p -r $r1 \
                        [file nativename $RCSFile] > $leftFile}
                catch {exec cvs update -p -r $r2 \
                        [file nativename $RCSFile] > $rightFile}
            } else {
                set leftLabel "$RCSFile (RCS $r1)"
                set rightLabel "$RCSFile (RCS $r2)"
                catch {exec co -p$r1 [file nativename $RCSFile] > $leftFile}
                catch {exec co -p$r2 [file nativename $RCSFile] > $rightFile}
            }
        }
    }
    update idletasks
    set Pref(dopt) $opts
}

proc cleanupRCS {} {
    global RCSFile rightFile leftFile Pref

    cleartmp
    set rightFile $RCSFile
    set leftFile $RCSFile
    set Pref(dopt) $Pref(old_dopt)
    unset Pref(old_dopt)
}

proc doDiff {} {
    global leftFile rightFile leftOK rightOK
    global eqLabel RCSmode Pref doingLine1 doingLine2
    global mapList mapMax

    if {$RCSmode == 0 && ($leftOK == 0 || $rightOK == 0)} {
        disableRedo
        return
    } else {
        enableRedo
    }

    busyCursor

    foreach w {.ft1.tl .ft1.tt .ft2.tl .ft2.tt} {
        $w configure -state normal
        $w delete 1.0 end
    }
    set mapList {}
    set mapMax 0

    update idletasks

    if {$RCSmode} {
        prepareRCS
    }

    set differr [catch {eval exec \$::diffexe $Pref(dopt) $Pref(ignore) \
            \$leftFile \$rightFile} diffres]

    set apa [split $diffres "\n"]
    set result {}
    foreach i $apa {
        if {[string match {[0-9]*} $i]} {
            lappend result $i
        }
    }

    if {[llength $result] == 0} {
        if {$differr == 1} {
            .ft1.tt insert end $diffres
            normalCursor
            return
        } else {
            set eqLabel "="
        }
    } else {
        set eqLabel " "
    }

    set ch1 [open $leftFile]
    set ch2 [open $rightFile]
    if {$::tcl_platform(platform) == "windows" && $Pref(crlf)} {
        fconfigure $ch1 -translation crlf
        fconfigure $ch2 -translation crlf
    }
    set doingLine1 1
    set doingLine2 1
    set t 0
    foreach i $result {
        if {![regexp {(.*)([acd])(.*)} $i apa l c r]} {
            .ft1.tt insert 1.0 "No regexp match for $i\n"
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
                    dotext $ch1 $ch2 0 $n2 [expr {$line1 + 1}] $line2
                }
                c {
                    dotext $ch1 $ch2 $n1 $n2 $line1 $line2
                }
                d {
                    # lucka i right, new i left
                    dotext $ch1 $ch2 $n1 0 $line1 [expr {$line2 + 1}]
                }
            }
        }
        if {[incr t] >= 10} {
            update idletasks
            .ft2.tl see end
            update idletasks
            set t 0
        }
    }

    dotext $ch1 $ch2 0 0 0 0

    # Make sure all text widgets have the same number of lines.
    # The common y scroll doesn't work well if not.
    set max 0.0
    foreach w {.ft1.tl .ft1.tt .ft2.tl .ft2.tt} {
        if {[$w index end] > $max} {
            set max [$w index end]
        }
    }
    foreach w {.ft1.tl .ft1.tt .ft2.tl .ft2.tt} {
        set d [expr {int($max - [$w index end])}]
        for {set t 0} {$t < $d} {incr t} {
            $w insert end \n
        }
    }

    close $ch1
    close $ch2

    if {$RCSmode} {
        cleanupRCS
    }

    drawMap -1
    foreach w {.ft1.tl .ft2.tl} {
        $w configure -state disabled
    }
    update idletasks
    .ft2.tl see 1.0
    normalCursor
}

# This is the entrypoint to do a diff via DDE or Send
proc remoteDiff {file1 file2} {
    global leftFile rightFile leftOK rightOK
    global leftDir rightDir leftLabel rightLabel

    set leftDir [file dirname $file1]
    set leftFile $file1
    set leftLabel $file1
    set leftOK 1
    set rightDir [file dirname $file2]
    set rightFile $file2
    set rightLabel $file2
    set rightOK 1
    set RCSmode 0
    wm deiconify .
    raise .
    update
    doDiff
}

proc doOpenLeft {{forget 0}} {
    global leftFile leftDir rightDir leftOK leftLabel

    if {!$forget && [info exists leftDir]} {
        set initDir $leftDir
    } elseif {[info exists rightDir]} {
        set initDir $rightDir
    } else {
        set initDir [pwd]
    }

    set apa [tk_getOpenFile -title "Select left file" -initialdir $initDir]
    if {$apa != ""} {
        set leftDir [file dirname $apa]
        set leftFile $apa
        set leftLabel $apa
        set leftOK 1
        return 1
    }
    return 0
}

proc doOpenRight {{forget 0}} {
    global rightFile rightDir leftDir rightOK rightLabel
    if {!$forget && [info exists rightDir]} {
        set initDir $rightDir
    } elseif {[info exists leftDir]} {
            set initDir $leftDir
    } else {
        set initDir [pwd]
    }

    set apa [tk_getOpenFile -title "Select right file" -initialdir $initDir]
    if {$apa != ""} {
        set rightDir [file dirname $apa]
        set rightFile $apa
        set rightLabel $apa
        set rightOK 1
        return 1
    }
    return 0
}

proc openLeft {} {
    global RCSmode
    if {[doOpenLeft]} {
        set RCSmode 0
        doDiff
    }
}

proc openRight {} {
    global RCSmode
    if {[doOpenRight]} {
        set RCSmode 0
        doDiff
    }
}

proc openRCS {} {
    global RCSmode leftFile rightFile leftOK RCSFile
    if {[doOpenRight]} {
        set RCSmode 1
        set RCSFile $rightFile
        set leftLabel "RCS"
        set leftOK 0
        doDiff
    }
}

proc openCVS {} {
    global RCSmode leftFile rightFile leftOK RCSFile
    if {[doOpenRight]} {
        set RCSmode 2
        set RCSFile $rightFile
        set leftLabel "CVS"
        set leftOK 0
        doDiff
    }
}

proc openBoth {forget} {
    global RCSmode
    if {[doOpenLeft]} {
        if {[doOpenRight $forget]} {
            set RCSmode 0
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
    incr h -1
    foreach {start stop type} $mapList {
        set y1 [expr {$start * $h / $mapMax + 1}]
        if {$y1 < 1} {set y1 1}
        if {$y1 > $h} {set y1 $h}
        set y2 [expr {$stop * $h / $mapMax + 1}]
        if {$y2 < 1} {set y2 1}
        if {$y2 <= $y1} {set y2 [expr {$y1 + 1}]}
        if {$y2 > $h} {set y2 $h}
        incr y2
        map put $Pref(color$type) -to 1 $y1 $x2 $y2
    }
}

# Format a line number for printing
proc formatLineno {lineno gray} {
    set res [format "%3d: " $lineno]
    if {[string length $res] > 5} {
        set res [string range $res end-5 end-1]
    }
    if {$gray == "1.0"} {
        return $res
    } else {
        return "\0bggray\{$gray\}$res\0bggray\{1.0\}"
    }
}

# Process the line numbers from the line number widget into a list
# of "linestarters"
proc processLineno {w} {
    set tdump [$w dump -tag -text 1.0 end]
    set gray 1.0
    set line ""
    set lines {}
    foreach {key value index} $tdump {
        if {$key == "tagon"} {
            if {$value == "change"} {
                set gray 0.6
            } else {
                set gray 0.8
            }
        } elseif {$key == "tagoff"} {
            set gray 1.0
        } elseif {$key == "text"} {
            append line $value
            if {[string index $value end] == "\n"} {
                set line [string trim [string trim $line] :]
                if {$line == ""} {
                    lappend lines ""
                } else {
                    lappend lines [formatLineno $line $gray]
                }
                set line ""
            }
        }
    }
    return $lines
}

# Handle wrapping of a too long line for printing
proc linewrap {gray} {
    if {$gray == "1.0"} {
        return "\n     "
    } else {
        return "\0bggray\{1.0\}\n     \0bggray\{$gray\}"
    }
}

# Prepare a text block for printing
proc fixTextBlock {text index} {
    # Remove any form feed
    if {[regsub -all "\f" $text {} apa]} {
        set text $apa
    }
    regexp {\d+\.(\d+)} $index -> index

    # Expand tabs to 8 chars
    while 1 {
        set i [string first \t $text]
        if {$i == -1} break
        set n [expr {(- $i - $index) % 8}]
        set text [string replace $text $i $i [format %${n}s ""]]
    }
    return $text
}

proc printDiffs {} {
    busyCursor
    update idletasks
    set tmpFile [file nativename ~/tcldiff.enscript]
    set tmpFile2 [file nativename ~/tcldifftmp.ps]
    set tmpFile3 [file nativename ~/tcldiff.ps]

    set lines1 {}
    set lines2 {}

    set tdump1 [.ft1.tt dump -tag -text 1.0 end]
    set tdump2 [.ft2.tt dump -tag -text 1.0 end]
    set lineNo1 [processLineno .ft1.tl]
    set lineNo2 [processLineno .ft2.tl]

    foreach tdump [list $tdump1 $tdump2] \
            lineName {lines1 lines2} wrapName {wrap1 wrap2} \
            lineNo [list $lineNo1 $lineNo2] {
        set lines {}
        set wraps {}
        set line [lindex $lineNo 0]
        set newline 0
        set gray 1.0
        set chars 0
        set wrapc 0
        foreach {key value index} $tdump {
            if {$key != "tagoff" && $newline == 1} {
                lappend lines $line
                lappend wraps $wrapc
                set newline 0
                set line [lindex $lineNo [llength $lines]]
                append line "\0bggray\{$gray\}"
                set chars 0
                set wrapc 0
            }
            switch $key {
                text {
                    set value [fixTextBlock $value $index]
                    if {[string index $value end] == "\n"} {
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
                    } elseif {$value != "last"} {
                        append line "\0bggray\{.8\}"
                        set gray 0.8
                    }
                }
                tagoff {
                    if {$value != "last"} {
                        append line "\0bggray\{1.0\}"
                        set gray 1.0
                    }
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

    set ch [open $tmpFile "w"]

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
    catch {exec mpage -bA4 -a2 $tmpFile2 > $tmpFile3}

    normalCursor

    destroy .dp
    toplevel .dp
    wm title .dp "Diff Print"
    button .dp.b -text Close -command {destroy .dp}
    label .dp.l -anchor w -justify left -text "The following files have\
            been created:\n\n$tmpFile\nInput file to enscript.\
            \n\n$tmpFile2\nCreated with 'enscript -c -B -e -p $tmpFile2\
            $tmpFile'\n\n$tmpFile3\nCreated with 'mpage -bA4 -a2 $tmpFile2 >\
            $tmpFile3'" -font "Courier 8"
    pack .dp.b -side bottom
    pack .dp.l -side top
}

proc my_yview args {
    foreach w {.ft1.tl .ft1.tt .ft2.tl .ft2.tt} {
        eval $w yview $args
    }
}

proc my_yscroll args {
    eval .sby set $args
    my_yview moveto [lindex $args 0]
}

proc chFont {} {
    global Pref

    font configure myfont -size $Pref(fontsize) -family $Pref(fontfamily)
}

proc applyColor {} {
    global Pref

    foreach w {.tl .tt} {
        .ft1$w tag configure new1 -foreground $Pref(colornew1) \
                -background $Pref(bgnew1)
        .ft1$w tag configure change -foreground $Pref(colorchange) \
                -background $Pref(bgchange)
        .ft2$w tag configure new2 -foreground $Pref(colornew2) \
                -background $Pref(bgnew2)
        .ft2$w tag configure change -foreground $Pref(colorchange) \
                -background $Pref(bgchange)
    }
}

proc scroll {n what} {
    if {![string match ".ft?.tt" [focus]]} {
        .ft1.tt yview scroll $n $what
    }
}

# Build the main window
proc makeDiffWin {} {
    global Pref tcl_platform debug
    eval destroy [winfo children .]

    wm protocol . WM_DELETE_WINDOW cleanupAndExit

    frame .f
    grid .f - - - -row 0 -sticky news

    menubutton .mf -text File -underline 0 -menu .mf.m
    menu .mf.m
    if {$debug == 1} {
        .mf.m add command -label "Redo Diff" -underline 5 -command doDiff
    } else {
        .mf.m add command -label "Redo Diff" -underline 5 -command doDiff \
                -state disabled
    }
    .mf.m add separator
    .mf.m add command -label "Open Both" -underline 0 -command {openBoth 0}
    .mf.m add command -label "Open Both (forget)" -command {openBoth 1}
    .mf.m add command -label "Open Left File" -command openLeft
    .mf.m add command -label "Open Right File" -command openRight
    if {$tcl_platform(platform) == "unix"} {
        .mf.m add command -label "RCSDiff" -underline 0 -command openRCS
        .mf.m add command -label "CVSDiff" -underline 0 -command openCVS
        .mf.m add separator
        .mf.m add command -label "Print" -underline 0 -command printDiffs
    }
    .mf.m add separator
    .mf.m add command -label "Quit" -command cleanupAndExit

    menubutton .mo -text Options -underline 0 -menu .mo.m
    menu .mo.m
    .mo.m add cascade -label Font -underline 0 -menu .mo.mf
    .mo.m add cascade -label Ignore -underline 0 -menu .mo.mi
    .mo.m add cascade -label Parse -underline 0 -menu .mo.mp
    .mo.m add command -label Colours -underline 0 -command makePrefWin
    .mo.m add checkbutton -label "Diffs only" -variable Pref(onlydiffs)
    if {$tcl_platform(platform) == "windows"} {
        .mo.m add checkbutton -label "Force crlf translation" \
                -variable Pref(crlf)
    }
    .mo.m add separator
    .mo.m add command -label "Save default" -command saveOptions

    menu .mo.mf
    .mo.mf add command -label "Select" -command makeFontWin
    .mo.mf add radiobutton -label 6 -variable Pref(fontsize) -value 6 \
            -command chFont
    .mo.mf add radiobutton -label 7 -variable Pref(fontsize) -value 7 \
            -command chFont
    .mo.mf add radiobutton -label 8 -variable Pref(fontsize) -value 8 \
            -command chFont
    .mo.mf add radiobutton -label 9 -variable Pref(fontsize) -value 9 \
            -command chFont
    .mo.mf add radiobutton -label 10 -variable Pref(fontsize) -value 10 \
            -command chFont

    menu .mo.mi
    .mo.mi add radiobutton -label "Nothing" -variable Pref(ignore) -value " "
    .mo.mi add radiobutton -label "Space changes (-b)" -variable Pref(ignore) \
            -value "-b"
    .mo.mi add radiobutton -label "All spaces (-w)" -variable Pref(ignore) \
            -value "-w"

    menu .mo.mp
    .mo.mp add radiobutton -label "Nothing" -variable Pref(parse) -value 0
    .mo.mp add radiobutton -label "Lines" -variable Pref(parse) -value 1
    .mo.mp add radiobutton -label "Blocks (small)" -variable Pref(parse) \
            -value 2
    .mo.mp add radiobutton -label "Blocks" -variable Pref(parse) -value 3
    .mo.mp add separator
    .mo.mp add radiobutton -label "Characters" -variable Pref(lineparsewords) \
            -value "0"
    .mo.mp add radiobutton -label "Words" -variable Pref(lineparsewords) \
            -value "1"
    .mo.mp add separator
    .mo.mp add checkbutton -label "Use 2nd stage" \
            -variable Pref(extralineparse)
    .mo.mp add checkbutton -label "Mark last" -variable Pref(marklast)

    menubutton .mh -text Help -underline 0 -menu .mh.m
    menu .mh.m
    .mh.m add command -label "Help" -command makeHelpWin
    .mh.m add command -label "About" -command makeAboutWin

    button .bfn -text "Next Diff" -relief raised -command findNext
    button .bfp -text "Prev Diff" -relief raised -command findPrev
    entry .eo -width 10 -textvariable Pref(dopt)
    label .lo -text "Diff Options"

    catch {font delete myfont}
    font create myfont -family $Pref(fontfamily) -size $Pref(fontsize)

    label .l1 -textvariable leftLabel -anchor e -width 10
    label .l2 -textvariable rightLabel -anchor e -width 10

    frame .ft1 -borderwidth 2 -relief sunken
    text .ft1.tl -height 40 -width 5 -wrap none -yscrollcommand my_yscroll \
            -font myfont -borderwidth 0 -padx 0 -highlightthickness 0
    text .ft1.tt -height 40 -width 80 -wrap none -yscrollcommand my_yscroll \
            -xscrollcommand ".sbx1 set" -font myfont -borderwidth 0 -padx 0 \
            -highlightthickness 0
    pack .ft1.tl -side left -fill y
    pack .ft1.tt -side right -fill both -expand 1
    scrollbar .sby -orient vertical -command "my_yview"
    scrollbar .sbx1 -orient horizontal -command ".ft1.tt xview"

    frame .ft2 -borderwidth 2 -relief sunken
    text .ft2.tl -height 60 -width 5 -wrap none -yscrollcommand my_yscroll \
            -font myfont -borderwidth 0 -padx 0 -highlightthickness 0
    text .ft2.tt -height 60 -width 80 -wrap none -yscrollcommand my_yscroll \
            -xscrollcommand ".sbx2 set" -font myfont -borderwidth 0 -padx 0 \
            -highlightthickness 0
    pack .ft2.tl -side left -fill y
    pack .ft2.tt -side right -fill both -expand 1
    scrollbar .sbx2 -orient horizontal -command ".ft2.tt xview"
    label .le -textvariable eqLabel -width 1
    canvas .c -width 6 -bd 0 -selectborderwidth 0 -highlightthickness 0

    applyColor
    .ft1.tt tag configure last -underline 1
    .ft2.tt tag configure last -underline 1

    grid .l1 .le - .l2 -row 1 -sticky news
    grid .ft1 .c .sby .ft2 -row 2 -sticky news
    grid .sbx1 x x .sbx2 -row 3 -sticky news
    grid columnconfigure . {0 3} -weight 1
    grid rowconfigure . 2 -weight 1
    grid .c -pady [expr {[.sby cget -width] + 2}]

    image create photo map
    .c create image 0 0 -anchor nw -image map
    bind .c <Configure> {drawMap %h}

    bind . <Key-Up> {scroll -1 u}
    bind . <Key-Down> {scroll 1 u}
    bind . <Key-Prior> {scroll -1 p}
    bind . <Key-Next> {scroll 1 p}
    bind . <Key-Escape> {focus .}

    if {$debug == 1} {
        menubutton .md -text Debug -menu .md.m -relief ridge
        menu .md.m
        if {$tcl_platform(platform) == "windows"} {
            .md.m add checkbutton -label Console -variable consolestate \
                    -onvalue show -offvalue hide \
                    -command {console $consolestate}
            .md.m add separator
        }
        .md.m add command -label "Stack trace" -command {bgerror Debug}
        .md.m add separator
        .md.m add command -label "Reread Source" -command {source $thisscript}
        .md.m add separator
        .md.m add command -label "Redraw Window" -command {makeDiffWin}
        .md.m add separator
        .md.m add command -label "Normal Cursor" -command {normalCursor}
        .md.m add separator
        .md.m add command -label "Evalstats" -command {evalstats}
        .md.m add command -label "_stats" -command {parray _stats}

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

    .pr.fc.t1 tag configure change -foreground $TmpPref(colorchange) \
            -background $TmpPref(bgchange)
    .pr.fc.t2 tag configure new1 -foreground $TmpPref(colornew1) \
            -background $TmpPref(bgnew1)
    .pr.fc.t3 tag configure new2 -foreground $TmpPref(colornew2) \
            -background $TmpPref(bgnew2)
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
    .pr.fc.t1 tag configure change -foreground $TmpPref(colorchange) \
            -background $TmpPref(bgchange)
    .pr.fc.t2 tag configure new1 -foreground $TmpPref(colornew1) \
            -background $TmpPref(bgnew1)
    .pr.fc.t3 tag configure new2 -foreground $TmpPref(colornew2) \
            -background $TmpPref(bgnew2)
    .pr.fc.t1 insert end "Changed text" change
    .pr.fc.t2 insert end "Deleted text" new1
    .pr.fc.t3 insert end "Added text" new2

    button .pr.b1 -text "Apply" -command applyPref
    button .pr.b2 -text "Test" -command testColor
    button .pr.b3 -text "Close" -command {destroy .pr}

    grid .pr.fc.l1 .pr.fc.l2 x .pr.fc.l3 x -row 0 -sticky ew -padx 1 -pady 1
    grid .pr.fc.t1 .pr.fc.e1 .pr.fc.b1 .pr.fc.e4 .pr.fc.b4 -row 1 \
            -sticky nsew -padx 1 -pady 1
    grid .pr.fc.t2 .pr.fc.e2 .pr.fc.b2 .pr.fc.e5 .pr.fc.b5 -row 2 \
            -sticky nsew -padx 1 -pady 1
    grid .pr.fc.t3 .pr.fc.e3 .pr.fc.b3 .pr.fc.e6 .pr.fc.b6 -row 3 \
            -sticky nsew -padx 1 -pady 1
    grid columnconfigure .pr.fc {1 3} -weight 1

    pack .pr.fc -side top -fill x
    pack .pr.b1 .pr.b2 .pr.b3 -side left -expand 1 -fill x
}

proc applyFont {} {
    global Pref TmpPref

    set Pref(fontsize) $TmpPref(fontsize)

    set i [lindex [.fo.lb curselection] 0]
    set Pref(fontfamily) [.fo.lb get $i]

    chFont
}

proc exampleFont {} {
    global TmpPref
    set i [lindex [.fo.lb curselection] 0]
    set TmpPref(fontfamily) [.fo.lb get $i]

    font configure tmpfont -family $TmpPref(fontfamily)
    if {[regexp {^[0-9]+$} $TmpPref(fontsize)]} {
        font configure tmpfont -size $TmpPref(fontsize)
    }
}

proc makeFontWin {} {
    global Pref TmpPref FontCache

    destroy .fo
    toplevel .fo
    wm title .fo "Select Font"

    label .fo.ltmp -text "Searching for fonts..."
    pack .fo.ltmp
    update idletasks

    catch {font delete tmpfont}
    font create tmpfont

    array set TmpPref [array get Pref]
    label .fo.lf -text Family -anchor w
    listbox .fo.lb -width 15 -height 10 -yscrollcommand ".fo.sb set" \
            -exportselection no -selectmode single
    bind .fo.lb <ButtonPress-1> {after idle exampleFont}
    scrollbar .fo.sb -orient vertical -command ".fo.lb yview"

    label .fo.ls -text Size -anchor w
    button .fo.bm -text - -padx 0 -pady 0 -highlightthickness 0 \
            -command {incr TmpPref(fontsize) -1 ; exampleFont}
    button .fo.bp -text + -padx 0 -pady 0 -highlightthickness 0 \
            -command {incr TmpPref(fontsize) ; exampleFont}
    entry .fo.es -textvariable TmpPref(fontsize) -width 3
    bind .fo.es <KeyPress> {after idle exampleFont}
    label .fo.le -text Example -anchor w -font tmpfont -width 1
    button .fo.bo -text Ok -command "applyFont; destroy .fo"
    button .fo.ba -text Apply -command "applyFont"
    button .fo.bc -text Cancel -command "destroy .fo"

    if {![info exists FontCache]} {
        set fam [lsort -dictionary [font families]]
        font create testfont
        foreach f $fam {
            if {[string compare $f ""]} {
                font configure testfont -family $f
                if {[font metrics testfont -fixed]} {
                    lappend FontCache $f
                }
            }
        }
        font delete testfont
    }
    foreach f $FontCache {
        .fo.lb insert end $f
        if {![string compare $f $Pref(fontfamily)]} {
            .fo.lb selection set end
            .fo.lb see end
        }
    }

    destroy .fo.ltmp

    grid .fo.lf -      .fo.ls -      - -sticky w
    grid .fo.lb .fo.sb .fo.es .fo.bm .fo.bp
    grid x      x      .fo.le -      - -sticky we
    grid x      x      .fo.bo -      - -sticky we
    grid x      x      .fo.ba -      - -sticky we
    grid x      x      .fo.bc -      - -sticky we
    grid .fo.lb -sticky news -rowspan 5
    grid .fo.sb -sticky ns   -rowspan 5
    grid .fo.es .fo.bm .fo.bp -sticky new
    grid columnconfigure .fo 0 -weight 1
    grid rowconfigure .fo 1 -weight 1

    exampleFont
}

# Help and startup functions

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
            -font "Courier 10"
    scrollbar .he.sb -orient vert -command ".he.t yview"
    button .he.b -text "Close" -command "destroy .he"
    pack .he.b -side bottom
    pack .he.sb -side right -fill y
    pack .he.t -side left -expand y -fill both
    .he.t tag configure new1 -foreground $Pref(colornew1) \
            -background $Pref(bgnew1)
    .he.t tag configure new2 -foreground $Pref(colornew2) \
            -background $Pref(bgnew2)
    .he.t tag configure change -foreground $Pref(colorchange) \
            -background $Pref(bgchange)
    .he.t tag configure ul -underline 1

    .he.t insert end {\

} "" {Commands} ul {

File Menu
  Redo Diff      : Run diff again on the same files.
  Open Both      : Select two files, run diff.
  Open Left File : Select a file for left window, run diff
  Open Right File: Select a file for right window, run diff
  RCSDiff        : (UNIX only) Select one file and diff like rcsdiff.
  Print          : (UNIX only) Experimental print function.
                   It currently creates a postscript file ~/tcldiff.ps
  Quit           : Guess

Options Menu
  Font     : Select font ant fontsize for the two main text windows
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
             Mark last  : Last change of a line is underlined
  Diffs only : Only differing lines will be displayed.
  Colours  : Choose highlight colours.
  Save default: Save current option settings in ~/.diffrc

Diff Options Field: Any text written here will be passed to diff.
                    In RCS mode, any -r options will be used to select versions.

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
.he.t insert end {2: } change {NET '/I$1/N$1455' IC2-14 IC6-8   } new1 "\n" ""
.he.t insert end {3: } change {NET '/I$1/N$1456' IC2-1} "" {3 IC2-1} new1 {2  } "" {2: } change {NET '/I$1/N$1456' IC2-12
}
.he.t insert end {4: NET '/I$1/N$1457' IC2-11 IC6-7   3: NET '/I$1/N$1457' IC2-11 IC6-7
}
.he.t insert end {5: } change {NET '/I$1/N$1458' IC2-} "" {10} change {         } "" {4: } change {NET '/I$1/N$1458' IC2-} "" {9} change "\n"

.he.t insert end "\n"

.he.t insert end "Example 4. Blocks and words\n"

.he.t insert end {1: } change {NET '/I$1/N$1454' } "" {IC2-15} change { IC5-7   } "" {1: } change {NET '/I$1/N$1454' } "" {IC1-4 IC2-15 IC5-2} change " IC5-7\n"
.he.t insert end {2: } change {NET '/I$1/N$1455' IC2-14 IC6-8   } new1 "\n" ""
.he.t insert end {3: } change {NET '/I$1/N$1456' } "" {IC2-13 } new1 {IC2-12  } "" {2: } change {NET '/I$1/N$1456' IC2-12
}
.he.t insert end {4: NET '/I$1/N$1457' IC2-11 IC6-7   3: NET '/I$1/N$1457' IC2-11 IC6-7
}
.he.t insert end {5: } change {NET '/I$1/N$1458' } "" {IC2-10} change {         } "" {4: } change {NET '/I$1/N$1458' } "" {IC2-9} change "\n"

.he.t insert end "\n"

.he.t insert end "Example 5. Blocks, words and 2nd stage\n"

.he.t insert end {1: } change {NET '/I$1/N$1454' IC2-15 IC5-7   } "" {1: } change {NET '/I$1/N$1454' } "" {IC1-4 } new2 {IC2-15} "" { IC5-2} new2 " IC5-7\n"
.he.t insert end {2: } change {NET '/I$1/N$1455' IC2-14 IC6-8   } new1 "\n" ""
.he.t insert end {3: } change {NET '/I$1/N$1456' } "" {IC2-13 } new1 {IC2-12  } "" {2: } change {NET '/I$1/N$1456' IC2-12
}
.he.t insert end {4: NET '/I$1/N$1457' IC2-11 IC6-7   3: NET '/I$1/N$1457' IC2-11 IC6-7
}
.he.t insert end {5: } change {NET '/I$1/N$1458' } "" {IC2-10} change {         } "" {4: } change {NET '/I$1/N$1458' } "" {IC2-9} change "\n"

}

proc parseCommandLine {} {
    global argv argc Pref RCSmode RCSFile tcl_platform
    global rightDir rightFile rightOK rightLabel
    global leftDir leftFile leftOK leftLabel

    set leftOK 0
    set rightOK 0
    set RCSmode 0
    set noautodiff 0
    set autobrowse 0

    if {$argc == 0} return

    set files ""
    foreach arg $argv {
        if {$arg == "-w"} {
            set Pref(ignore) "-w"
        } elseif {$arg == "-b"} {
            set Pref(ignore) "-b"
        } elseif {$arg == "-noignore"} {
            set Pref(ignore) " "
        } elseif {$arg == "-noparse"} {
            set Pref(parse) 0
        } elseif {$arg == "-line"} {
            set Pref(parse) 1
        } elseif {$arg == "-block"} {
            set Pref(parse) 3
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
        } elseif {$arg == "-browse"} {
            set autobrowse 1
        } elseif {$arg == "-server"} {
            if {$tcl_platform(platform) == "unix"} {
                tk appname Diff
            } else {
                dde servername Diff
            }
        } elseif {[string range $arg 0 0] == "-"} {
            set Pref(dopt) "$Pref(dopt) $arg"
        } else {
            set apa [glob -nocomplain [file join [pwd] $arg]]
            if {$apa == ""} {
                puts "Ignoring argument: $arg"
            } else {
                lappend files [lindex $apa 0]
            }
        }
    }

    set len [llength $files]
    if {$len == 1} {
        set fullname [file join [pwd] $files]
        set fulldir [file dirname $fullname]
        if {[llength [glob -nocomplain [file join $fulldir RCS]]]} {
            set RCSmode 1
            set rightDir $fulldir
            set RCSFile $fullname
            set rightLabel $fullname
            set rightFile $fullname
            set rightOK 1
            set leftLabel "RCS"
            if {$noautodiff} {
                enableRedo
            } else {
                after idle doDiff
            }
        } elseif {[llength [glob -nocomplain [file join $fulldir CVS]]]} {
            set RCSmode 2
            set rightDir $fulldir
            set RCSFile $fullname
            set rightLabel $fullname
            set rightFile $fullname
            set rightOK 1
            set leftLabel "CVS"
            if {$noautodiff} {
                enableRedo
            } else {
                after idle doDiff
            }
        } else {
            set leftDir $fulldir
            set leftFile $fullname
            set leftLabel $fullname
            set leftOK 1
        }
    } elseif {$len >= 2} {
        set fullname [file join [pwd] [lindex $files 0]]
        set fulldir [file dirname $fullname]
        set leftDir $fulldir
        set leftFile $fullname
        set leftLabel $fullname
        set leftOK 1
        set fullname [file join [pwd] [lindex $files 1]]
        set fulldir [file dirname $fullname]
        set rightDir $fulldir
        set rightFile $fullname
        set rightLabel $fullname
        set rightOK 1
        if {$noautodiff} {
            enableRedo
        } else {
            after idle doDiff
        }
    }
    if {$autobrowse && (!$leftOK || !$rightOK)} {
        if {!$leftOK && !$rightOK} {
            openBoth 0
        } elseif {!$leftOK} {
            openLeft
        } elseif {!$rightOK} {
            openRight
        }
    }
}

proc saveOptions {} {
    global Pref

    set ch [open "~/.diffrc" w]

    foreach i [array names Pref] {
        if {$i != "dopt"} {
            puts $ch [list set Pref($i) $Pref($i)]
        }
    }
    close $ch
}

proc getOptions {} {
    global Pref

    set Pref(fontsize) 9
    set Pref(fontfamily) courier
    set Pref(ignore) "-b"
    set Pref(dopt) ""
    set Pref(parse) 2
    set Pref(lineparsewords) "0"
    set Pref(extralineparse) 1
    set Pref(colorchange) red
    set Pref(colornew1) darkgreen
    set Pref(colornew2) blue
    set Pref(bgchange) gray
    set Pref(bgnew1) gray
    set Pref(bgnew2) gray
    set Pref(onlydiffs) 0
    set Pref(crlf) 0
    set Pref(marklast) 1

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
