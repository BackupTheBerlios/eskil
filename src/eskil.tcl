#!/bin/sh
#
#   diff.tcl
#
#   Purpose
#             Graphical frontend to diff
#
#   Usage
#             Do 'diff.tcl' for interactive mode
#             Do 'diff.tcl -h' for command line usage
#
#   Author    Peter Spjuth  980612
#
#   Revised   Date     Remark
#             
#     1.0     980612   New Version.
#     1.1     980807   Parsing of change blocks added
#                      Options menu and variables changed
#                      Command line options added
#     1.2     980818   Improved yscroll
#                      Added map next to y-scrollbar
#     1.3     980921   Added Prev Diff button
#                      Added colour options, and Only diffs option
#                      Added 2nd stage line parsing
#                      Improved block parsing
#                      Added print
#     1.4     990210   Bug-fix in "Ignore nothing"
#                      Bug-fix in file handling
#                      Improved RCS handling.
#     1.5     990623   Bug-fix and improvement in block parsing
#                      Added font selection
#                      Added "diff server" functionality
#                      Split text windows in lineno/text
#                      Added "Mark last" option
#     1.6     000131   Added scroll-keys
#                      Bug-fixes in scroll map and printing
#     1.7     000427   Restricted parsing of large blocks.
#                      Fixed bug with spaces in file names.
#                      Regular screen updates during processing.
#                      Added CVS support.
#     1.8     001115   Highlight current diff.
#                      New -conflict flag to handle merge conflicts.
#     1.9     011105   Added right-click "zoom".
#                      Added -print option.
#                      Improved printing, allow print on Windows.
#                      Display patch mode.
#                      Added search and incremental search.
#                      Added context around a 'diffs only' output.
#
#-----------------------------------------------
# $Revision$
#-----------------------------------------------
# the next line restarts using wish \
exec wish "$0" "$@"

set debug 0
set diffver "Version 1.9.3  2002-03-11"
set tmpcnt 0
set tmpfiles {}
set thisscript [file join [pwd] [info script]]
set thisdir [file dirname $thisscript]

set diffexe diff

# Support for FreeWrap.
if {[info exists ::freewrap::contents]} {
    set debug 0
    set thisdir [pwd]
    set thisscript ""
    # If diff.exe is wrapped, copy it so we can use it.
    if {[info exists ::freewrap::pkgInfo(diff.exe)]} {
        if {[info exists env(TEMP)]} {
            set diffexe [file join $env(TEMP) diff.exe]
        } elseif {[info exists env(TMP)]} {
            set diffexe [file join $env(TMP) diff.exe]
        } else {
            set diffexe [file join c:/ diff.exe]
        }
        ::freewrap::pkgfilecopy diff.exe $diffexe force
    }
}

if {$tcl_platform(platform) == "windows"} {
    cd $thisdir
    catch {package require dde}
}

proc cleanupAndExit {} {
    if {$::diffexe != "diff"} {
        file delete $::diffexe
    }
    cleartmp
    exit
}

# Format a line number
proc myforml {lineNo} {
    if {![string is integer -strict $lineNo]} {return "$lineNo\n"}
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
        # Replace middle element in res* with list elements from mid*
        #set res1 [eval lreplace \$res1 1 1 $mid1]
        #set res2 [eval lreplace \$res2 1 1 $mid2]
        # This makes use of pure-list optimisation in eval
        set res1 [eval [linsert $mid1 0 lreplace $res1 1 1]]
        set res2 [eval [linsert $mid2 0 lreplace $res2 1 1]]
    }
}

# Compare two lines and rate how much they resemble each other.
# This has never worked well. Some day I'll sit down, think this through,
# and come up with a better algorithm.
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
# This tries to match the lines that resemble each other and put them
# next to each other.
# As the previous procedure, this would need a complete rework and a
# better algorithm.
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
    .ft$n.tt insert end "$text\n" $tag
    if {$tag != ""} {
        set tag "hl$::HighLightCount $tag"
    }
    .ft$n.tl insert end [myforml $line] $tag
}

proc emptyline {n {highlight 1}} {
    if {$highlight} {
        .ft$n.tl insert end "\n" hl$::HighLightCount
    } else {
        .ft$n.tl insert end "\n"
    }
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
        .ft1.tl insert end [myforml $doingLine1] "hl$::HighLightCount change"
        .ft2.tl insert end [myforml $doingLine2] "hl$::HighLightCount change"
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

# Returns number of lines used to display the blocks
proc insertMatchingBlocks {block1 block2} {
    global doingLine1 doingLine2

    set apa [compareblocks $block1 $block2]

    set t1 0
    set t2 0
    foreach c $apa {
        if {$c == "c"} {
            set textline1 [lindex $block1 $t1]
            set textline2 [lindex $block2 $t2]
            insertMatchingLines $textline1 $textline2
            incr t1
            incr t2
        }
        if {$c == "d"} {
            set bepa [lindex $block1 $t1]
            .ft1.tl insert end [myforml $doingLine1] \
                    "hl$::HighLightCount change"
            .ft1.tt insert end "$bepa\n" new1
            emptyline 2
            incr doingLine1
            incr t1
        }
        if {$c == "a"} {
            set bepa [lindex $block2 $t2]
            .ft2.tl insert end [myforml $doingLine2] \
                    "hl$::HighLightCount change"
            .ft2.tt insert end "$bepa\n" new2
            emptyline 1
            incr doingLine2
            incr t2
        }
    }
    return [llength $apa]
}

# Process one of the change/add/delete blocks reported by diff.
# ch1 is a file channel for the left file
# ch2 is a file channel for the right file
# n1/n2 is the number of lines involved
# line1/line2 says on what lines this block starts
proc dotext {ch1 ch2 n1 n2 line1 line2} {
    global doingLine1 doingLine2 Pref mapMax changesList

    if {$n1 == 0 && $n2 == 0} {
        # All blocks have been processed. Continue until end of file.
        # If "only diffs" is on, just display a couple of context lines.
        set limit -1
        if {$Pref(onlydiffs) == 1} {
            set limit $Pref(context)
        }
        if {$::diff(limitlines)} {
            set limit [expr {$::diff(limitlines) - $mapMax}]
            if {$limit < 0} {
                set limit 0
            }
        }
        set t 0
        while {[gets $ch2 apa] != -1} {
            insert 2 $doingLine2 $apa
            incr doingLine2
            incr mapMax
            incr t
            if {$limit >= 0 && $t >= $limit} break
        }
        set t 0
        while {[gets $ch1 apa] != -1} {
            insert 1 $doingLine1 $apa
            incr doingLine1
            incr t
            if {$limit >= 0 && $t >= $limit} break
        }
        return
    }

    if {$n1 == 0} {set tag2 new2} else {set tag2 change}
    if {$n2 == 0} {set tag1 new1} else {set tag1 change}

    # Display all equal lines before next diff
    # If only diff is on, only skip a section if the blank
    # line replaces at least 3 lines.
    set limit -1
    if {$Pref(onlydiffs) == 1 && \
            ($line1 - $doingLine1 > (2 * $Pref(context) + 2))} {
        set limit $Pref(context)
    }
    set t 0
    while {$doingLine1 < $line1} {
        gets $ch1 apa
        gets $ch2 bepa
        if {$limit < 0 || ($t < $limit && $doingLine1 > $limit) || \
                ($line1 - $doingLine1) <= $limit} {
            insert 1 $doingLine1 $apa
            insert 2 $doingLine2 $bepa
            incr mapMax
        } elseif {$t == $limit} {
            emptyline 1 0
            emptyline 2 0
            incr mapMax
        }
        incr doingLine1
        incr doingLine2
        incr t
        if {$::diff(limitlines) && $mapMax > $::diff(limitlines)} {
            return
        }
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
            gets $ch1 textline1
            gets $ch2 textline2
            insertMatchingLines $textline1 $textline2
        }
        lappend changesList $mapMax $n1 change $line1 $n1 $line2 $n2
        incr mapMax $n1
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
            set apa [insertMatchingBlocks $block1 $block2]

            lappend changesList $mapMax $apa change \
                    $line1 $n1 $line2 $n2
            incr mapMax $apa
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
            if {$n1 <= $n2} {
                for {set t $n1} {$t < $n2} {incr t} {
                    emptyline 1
                }
                lappend changesList $mapMax $n2 $tag2 \
                        $line1 $n1 $line2 $n2
                incr mapMax $n2
            } elseif {$n2 < $n1} {
                for {set t $n2} {$t < $n1} {incr t} {
                    emptyline 2
                }
                lappend changesList $mapMax $n1 $tag1 \
                        $line1 $n1 $line2 $n2
                incr mapMax $n1
            }
        }
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

# Read a conflict file and extract the two versions.
proc prepareConflict {} {
    global diff Pref

    set diff(leftFile) [tmpfile]
    set diff(rightFile) [tmpfile]

    set ch1 [open $diff(leftFile) w]
    set ch2 [open $diff(rightFile) w]
    set ch [open $diff(conflictFile) r]

    set diff(conflictDiff) {}
    set leftLine 1
    set rightLine 1
    set state both
    set rightName ""
    set leftName ""
    while {[gets $ch line] != -1} {
        if {[string match <<<<<<* $line]} {
            set state right
            regexp {<*\s*(.*)} $line -> rightName
            set start2 $rightLine
        } elseif {[string match ======* $line] && $state == "right"} {
            set state left
            set end2 [expr {$rightLine - 1}]
            set start1 $leftLine
        } elseif {[string match >>>>>>* $line] && $state == "left"} {
            set state both
            regexp {>*\s*(.*)} $line -> leftName
            set end1 [expr {$leftLine - 1}]
            lappend diff(conflictDiff) $start1,${end1}c$start2,$end2
        } elseif {$state == "both"} {
            puts $ch1 $line
            puts $ch2 $line
            incr leftLine
            incr rightLine
        } elseif {$state == "left"} {
            puts $ch1 $line
            incr leftLine
        } else {
            puts $ch2 $line
            incr rightLine
        }
    }
    close $ch
    close $ch1
    close $ch2

    if {$leftName == "" && $rightName == ""} {
        set leftName "No Conflict: [file tail $diff(conflictFile)]"
        set rightName $leftName
    }
    set diff(leftLabel) $leftName
    set diff(rightLabel) $rightName
    update idletasks
}

# Clean up after a conflict diff.
proc cleanupConflict {} {
    global diff Pref

    cleartmp
    set diff(rightFile) $diff(conflictFile)
    set diff(leftFile) $diff(conflictFile)
}

proc displayOnePatch {leftLines rightLines leftLine rightLine} {
    emptyline 1
    emptyline 2

    set leftlen [llength $leftLines]
    set rightlen [llength $rightLines]

    set leftc 0
    set rightc 0
    set lblock {}
    set lblockl 0
    set rblock {}
    set rblockl 0

    while {$leftc < $leftlen || $rightc < $rightlen} {
        foreach {lline lmode lstr} [lindex $leftLines $leftc] break
        foreach {rline rmode rstr} [lindex $rightLines $rightc] break

        # Fix the case where one side's block is empty.
        # That means that each line not marked should show up on both sides.
        if {$leftc >= $leftlen} {
            set lline $leftLine
            incr leftLine
            set lmode ""
            set lstr $rstr
        }
        if {$rightc >= $rightlen} {
            set rline $rightLine
            incr rightLine
            set rmode ""
            set rstr $lstr
        }

        # Treat the combination "-" and "+" as a "!"
        if {$lmode == "-" && $rmode == "+"} {
            set lmode "!"
            set rmode "!"
        }
        if {$lmode == "-" && [llength $lblock] > 0} {
            set lmode "!"
        }
        if {$rmode == "+" && [llength $rblock] > 0} {
            set rmode "!"
        }

        # If we are in a change block, gather up all lines
        if {$lmode == "!" || $rmode == "!"} {
            if {$lmode == "!"} {
                if {[llength $lblock] == 0} {
                    set lblockl $lline
                }
                lappend lblock $lstr
                incr leftc
            }
            if {$rmode == "!"} {
                if {[llength $rblock] == 0} {
                    set rblockl $rline
                }
                lappend rblock $rstr
                incr rightc
            }
            continue
        }
        # No change block anymore. If one just ended, display it.
        if {[llength $lblock] > 0 || [llength $rblock] > 0} {
            set ::doingLine1 $lblockl
            set ::doingLine2 $rblockl
            insertMatchingBlocks $lblock $rblock
            set lblock {}
            set rblock {}
        }
        if {$lmode == "" && $rmode == ""} {
            insert 1 $lline $lstr
            insert 2 $rline $rstr
            incr leftc
            incr rightc
            continue
        }
        if {$lmode == "-"} {
            insert 1 $lline $lstr new1
            emptyline 2
            incr leftc
            continue
        }
        if {$rmode == "+"} {
            insert 2 $rline $rstr new2
            emptyline 1
            incr rightc
            continue
        }
    }
}

# Read a patch file and display it
proc displayPatch {} {
    global diff Pref

    set diff(leftLabel) "Patch $diff(patchFile): old"
    set diff(rightLabel) "Patch $diff(patchFile): new"
    update idletasks

    set ch [open $diff(patchFile) r]

    set style ""
    set divider "-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-"

    set leftLine 1
    set rightLine 1
    set leftLines {}
    set rightLines {}
    set state none
    while {[gets $ch line] != -1} {
        if {[string match ======* $line]} {
            if {$state != "none"} {
                displayOnePatch $leftLines $rightLines $leftLine $rightLine
            }
            set leftLines {}
            set rightLines {}
            set state none
            continue
        }
        # Detect the first line in a -c style diff
        if {$state == "none" && [regexp {^\*\*\*} $line]} {
            set state newfile
            set style c
            set leftRE {^\*\*\*\s+(.*)$}
            set rightRE {^---\s+(.*)$}
        }
        # Detect the first line in a -u style diff
        if {$state == "none" && [regexp {^---} $line]} {
            set state newfile
            set style u
            set leftRE {^---\s+(.*)$}
            set rightRE {^\+\+\+\s+(.*)$}
        }
        if {$state == "newfile" && [regexp $leftRE $line -> sub]} {
            emptyline 1
            insert 1 "" $divider
            insert 1 "" $sub
            insert 1 "" $divider
            continue
        }
        if {$state == "newfile" && [regexp $rightRE $line -> sub]} {
            emptyline 2
            insert 2 "" $divider
            insert 2 "" $sub
            insert 2 "" $divider
            continue
        }
        # A new section in a -u style diff
        if {[regexp {^@@\s+-(\d+),\d+\s+\+(\d+),} $line -> sub1 sub2]} {
            if {$state == "both"} {
                displayOnePatch $leftLines $rightLines $leftLine $rightLine
            }
            set state both
            set leftLine $sub1
            set rightLine $sub2
            set leftLines {}
            set rightLines {}
            continue
        }
        # A new section in a -c style diff
        if {[regexp {^\*\*\*\*\*} $line]} {
            if {$state == "right"} {
                displayOnePatch $leftLines $rightLines $leftLine $rightLine
            }
            set leftLines {}
            set rightLines {}
            set state left
            continue
        }
        # We are in the left part of a -c style diff
        if {$state == "left"} {
            if {[regexp {^\*\*\*\s*(\d*)} $line -> sub]} {
                if {$sub != ""} {
                    set leftLine $sub
                }
                continue
            }
            if {[regexp {^---\s*(\d*)} $line -> sub]} {
                if {$sub != ""} {
                    set rightLine $sub
                }
                set state right
                continue
            }
            if {![regexp {^[\s!+-]} $line]} continue
            lappend leftLines [list $leftLine \
                    [string trim [string range $line 0 1]] \
                    [string range $line 2 end]]
            incr leftLine
            continue
        }
        # We are in the right part of a -c style diff
        if {$state == "right"} {
            if {![regexp {^[\s!+-]} $line]} continue
            lappend rightLines [list $rightLine \
                    [string trim [string range $line 0 1]] \
                    [string range $line 2 end]]
            incr rightLine
            continue
        }
        # We are in a -u style diff
        if {$state == "both"} {
            if {![regexp {^[\s+-]} $line]} continue
            set sig [string trim [string index $line 0]]
            set str [string range $line 1 end]
            if {$sig == ""} {
                lappend leftLines [list $leftLine "" $str]
                lappend rightLines [list $leftLine "" $str]
                incr leftLine
                incr rightLine
            } elseif {$sig == "-"} {
                lappend leftLines [list $leftLine "-" $str]
                incr leftLine
            } else {
                lappend rightLines [list $leftLine "+" $str]
                incr rightLine
            }
            continue
        }
    }
    if {$state != "none"} {
        displayOnePatch $leftLines $rightLines $leftLine $rightLine
    }

    close $ch

}

# Prepare for RCS/CVS diff. Checkout copies of the versions needed.
proc prepareRCS {} {
    global diff Pref

    set revs {}
    set opts {}
    set Pref(old_dopt) $Pref(dopt)

    # Search for revision options
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
            # Compare local file with latest version.
            set diff(leftFile) [tmpfile]
            set diff(rightLabel) $diff(RCSFile)
            set diff(rightFile) $diff(RCSFile)

            if {$diff(mode) == "CVS"} {
                set diff(leftLabel) "$diff(RCSFile) (CVS)"
                catch {exec cvs update -p \
                        [file nativename $diff(RCSFile)] > $diff(leftFile)}
            } else {
                set diff(leftLabel) "$diff(RCSFile) (RCS)"
                catch {exec co -p [file nativename $diff(RCSFile)] > $diff(leftFile)}
            }
        }
        1 {
            # Compare local file with specified version.
            set r [lindex $revs 0]
            set diff(leftFile) [tmpfile]
            set diff(rightLabel) $diff(RCSFile)
            set diff(rightFile) $diff(RCSFile)

            if {$diff(mode) == "CVS"} {
                set diff(leftLabel) "$diff(RCSFile) (CVS $r)"
                catch {exec cvs update -p -r $r \
                        [file nativename $diff(RCSFile)] > $diff(leftFile)}
            } else {
                set diff(leftLabel) "$diff(RCSFile) (RCS $r)"
                catch {exec co -p$r [file nativename $diff(RCSFile)] > $diff(leftFile)}
            }
        }
        default {
            # Compare the two specified versions.
            set r1 [lindex $revs 0]
            set r2 [lindex $revs 1]
            set diff(leftFile) [tmpfile]
            set diff(rightFile) [tmpfile]

            if {$diff(mode) == "CVS"} {
                set diff(leftLabel) "$diff(RCSFile) (CVS $r1)"
                set diff(rightLabel) "$diff(RCSFile) (CVS $r2)"
                catch {exec cvs update -p -r $r1 \
                        [file nativename $diff(RCSFile)] > $diff(leftFile)}
                catch {exec cvs update -p -r $r2 \
                        [file nativename $diff(RCSFile)] > $diff(rightFile)}
            } else {
                set diff(leftLabel) "$diff(RCSFile) (RCS $r1)"
                set diff(rightLabel) "$diff(RCSFile) (RCS $r2)"
                catch {exec co -p$r1 [file nativename $diff(RCSFile)] > $diff(leftFile)}
                catch {exec co -p$r2 [file nativename $diff(RCSFile)] > $diff(rightFile)}
            }
        }
    }
    # Make sure labels are updated before processing starts
    update idletasks
    set Pref(dopt) $opts
}

# Clean up after a RCS/CVS diff.
proc cleanupRCS {} {
    global diff Pref

    cleartmp
    set diff(rightFile) $diff(RCSFile)
    set diff(leftFile) $diff(RCSFile)
    set Pref(dopt) $Pref(old_dopt)
    unset Pref(old_dopt)
}

# Main diff function.
proc doDiff {} {
    global diff Pref
    global doingLine1 doingLine2
    global mapMax changesList

    if {$diff(mode) == "" && ($diff(leftOK) == 0 || $diff(rightOK) == 0)} {
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
    set changesList {}
    set mapMax 0
    set ::HighLightCount 0
    highLightChange -1

    update idletasks

    if {$diff(mode) == "patch"} {
        displayPatch
        drawMap -1
        foreach w {.ft1.tl .ft2.tl} {
            $w configure -state disabled
        }
        update idletasks
        .ft2.tl see 1.0
        normalCursor
        return
    } elseif {$diff(mode) == "RCS" || $diff(mode) == "CVS"} {
        prepareRCS
    } elseif {[string match "conflict*" $diff(mode)]} {
        prepareConflict
    }

    # Run diff and parse the result.
    set differr [catch {eval exec \$::diffexe $Pref(dopt) $Pref(ignore) \
            \$diff(leftFile) \$diff(rightFile)} diffres]

    set apa [split $diffres "\n"]
    set result {}
    foreach i $apa {
        if {[string match {[0-9]*} $i]} {
            lappend result $i
        }
    }

    # In conflict mode we can use the diff information collected when
    # parsing the conflict file. This makes sure the blocks in the conflict
    # file become change-blocks during merge.
    if {$diff(mode) == "conflictPure"} {
        set result $diff(conflictDiff)
    }

    if {[llength $result] == 0} {
        if {$differr == 1} {
            .ft1.tt insert end $diffres
            normalCursor
            return
        } else {
            set ::diff(eqLabel) "="
        }
    } else {
        set ::diff(eqLabel) " "
    }
    update idletasks

    set ch1 [open $diff(leftFile)]
    set ch2 [open $diff(rightFile)]
    if {$::tcl_platform(platform) == "windows" && $Pref(crlf)} {
        fconfigure $ch1 -translation crlf
        fconfigure $ch2 -translation crlf
    }
    set doingLine1 1
    set doingLine2 1
    set t 0
    foreach i $result {
        if {![regexp {(.*)([acd])(.*)} $i -> l c r]} {
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
        incr ::HighLightCount
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

    drawMap -1
    foreach w {.ft1.tl .ft2.tl} {
        $w configure -state disabled
    }
    update idletasks
    .ft2.tl see 1.0
    normalCursor
    showDiff 0

    if {$diff(mode) == "RCS" || $diff(mode) == "CVS"} {
        cleanupRCS
    } elseif {[string match "conflict*" $diff(mode)]} {
        cleanupConflict
        if {$::diff(eqLabel) != "="} {
            after idle makeMergeWin
        }
    }
    if {$diff(printFile) != ""} {
        after idle {doPrint 1 ; exit}
    }
}

# This is the entrypoint to do a diff via DDE or Send
proc remoteDiff {file1 file2} {
    global diff

    set diff(leftDir) [file dirname $file1]
    set diff(leftFile) $file1
    set diff(leftLabel) $file1
    set diff(leftOK) 1
    set diff(rightDir) [file dirname $file2]
    set diff(rightFile) $file2
    set diff(rightLabel) $file2
    set diff(rightOK) 1
    set diff(mode) ""
    wm deiconify .
    raise .
    update
    doDiff
}

#####################################
# Highlight and navigation stuff
#####################################

# Scroll windows to next/previous diff
proc findDiff {delta} {
    global CurrentHighLight

    showDiff [expr {$CurrentHighLight + $delta}]
}

# Scroll a text window to view a certain range, and possibly some
# lines before and after.
proc seeText {w si ei} {
    $w see $ei
    $w see $si
    $w see $si-5lines
    $w see $ei+5lines
    if {[llength [$w bbox $si]] == 0} {
        $w yview $si-5lines
    }
    if {[llength [$w bbox $ei]] == 0} {
        $w yview $si
    }
}

# Highlight a diff
proc highLightChange {n} {
    global CurrentHighLight changesList
    if {[info exists CurrentHighLight] && $CurrentHighLight >= 0} {
        .ft1.tl tag configure hl$CurrentHighLight -background {}
        .ft2.tl tag configure hl$CurrentHighLight -background {}
    }
    set CurrentHighLight $n
    if {$CurrentHighLight < 0} {
        set CurrentHighLight -1
    } elseif {$CurrentHighLight * 7 >= [llength $changesList]} {
        set CurrentHighLight [expr {[llength $changesList] / 7}]
    } else {
        .ft1.tl tag configure hl$CurrentHighLight -background yellow
        .ft2.tl tag configure hl$CurrentHighLight -background yellow
    }
}

# Highlight a diff and scroll windows to it.
proc showDiff {num} {
    global CurrentHighLight changesList

    highLightChange $num

    set line1 [lindex $changesList [expr {$CurrentHighLight * 7}]]
    
    if {$CurrentHighLight < 0} {
        set line1 1.0
        set line2 1.0
    } elseif {$line1 == ""} {
        set line1 end
        set line2 end
    } else {
        set line2 [expr {$line1 + \
                [lindex $changesList [expr {$CurrentHighLight * 7 + 1}]]}]
        incr line1
        set line1 $line1.0
        set line2 $line2.0
    }

    foreach w {.ft1.tl .ft1.tt .ft2.tl .ft2.tt} {
        seeText $w $line1 $line2
    }
}

#####################################
# File dialog stuff
#####################################

proc doOpenLeft {{forget 0}} {
    global diff

    if {!$forget && [info exists diff(leftDir)]} {
        set initDir $diff(leftDir)
    } elseif {[info exists diff(rightDir)]} {
        set initDir $diff(rightDir)
    } else {
        set initDir [pwd]
    }

    set apa [tk_getOpenFile -title "Select left file" -initialdir $initDir]
    if {$apa != ""} {
        set diff(leftDir) [file dirname $apa]
        set diff(leftFile) $apa
        set diff(leftLabel) $apa
        set diff(leftOK) 1
        return 1
    }
    return 0
}

proc doOpenRight {{forget 0}} {
    global diff
    if {!$forget && [info exists diff(rightDir)]} {
        set initDir $diff(rightDir)
    } elseif {[info exists diff(leftDir)]} {
        set initDir $diff(leftDir)
    } else {
        set initDir [pwd]
    }

    set apa [tk_getOpenFile -title "Select right file" -initialdir $initDir]
    if {$apa != ""} {
        set diff(rightDir) [file dirname $apa]
        set diff(rightFile) $apa
        set diff(rightLabel) $apa
        set diff(rightOK) 1
        return 1
    }
    return 0
}

proc openLeft {} {
    global diff
    if {[doOpenLeft]} {
        set diff(mode) ""
        doDiff
    }
}

proc openRight {} {
    global diff
    if {[doOpenRight]} {
        set diff(mode) ""
        doDiff
    }
}

proc openConflict {} {
    global diff
    if {[doOpenRight]} {
        set diff(mode) "conflict"
        set Pref(ignore) " "
        set diff(conflictFile) $diff(rightFile)
        set diff(mergeFile) ""
        doDiff
    }
}

proc openPatch {} {
    global diff
    if {[doOpenLeft]} {
        set diff(mode) "patch"
        set Pref(ignore) " "
        set diff(patchFile) $diff(leftFile)
        doDiff
    }
}

proc openRCS {} {
    global diff
    if {[doOpenRight]} {
        set diff(mode) "RCS"
        set diff(RCSFile) $diff(rightFile)
        set diff(leftLabel) "RCS"
        set diff(leftOK) 0
        doDiff
    }
}

proc openCVS {} {
    global diff
    if {[doOpenRight]} {
        set diff(mode) "CVS"
        set diff(RCSFile) $diff(rightFile)
        set diff(leftLabel) "CVS"
        set diff(leftOK) 0
        doDiff
    }
}

proc openBoth {forget} {
    global diff
    if {[doOpenLeft]} {
        if {[doOpenRight $forget]} {
            set diff(mode) ""
            doDiff
        }
    }
}

#####################################
# Map stuff
#####################################

proc drawMap {newh} {
    global mapMax Pref changesList

    set oldh [map cget -height]
    if {$oldh == $newh} return

    map blank
    if {![info exists changesList] || [llength $changesList] == 0} return

    set w [winfo width .c]
    set h [winfo height .c]
    set x2 [expr {$w - 1}]
    map configure -width $w -height $h
    incr h -1
    foreach {start length type dum1 dum2 dum3 dum4} $changesList {
        set y1 [expr {$start * $h / $mapMax + 1}]
        if {$y1 < 1} {set y1 1}
        if {$y1 > $h} {set y1 $h}
        set y2 [expr {($start + $length) * $h / $mapMax + 1}]
        if {$y2 < 1} {set y2 1}
        if {$y2 <= $y1} {set y2 [expr {$y1 + 1}]}
        if {$y2 > $h} {set y2 $h}
        incr y2
        map put $Pref(color$type) -to 1 $y1 $x2 $y2
    }
}

######################################
# Merge stuff
#####################################

# Get all data from the files to merge
proc collectMergeData {} {
    global diff
    global changesList mergeSelection
    global leftMergeData rightMergeData

    set leftMergeData {}
    set rightMergeData {}

    if {![info exists changesList]} {
        set changesList {}
    }

    if {$diff(mode) == "RCS" || $diff(mode) == "CVS"} {
        prepareRCS
    } elseif {[string match "conflict*" $diff(mode)]} {
        prepareConflict
    }

    set ch1 [open $diff(leftFile) r]
    set ch2 [open $diff(rightFile) r]
    set doingLine1 1
    set doingLine2 1
    set changeNo 0
    foreach {start length type line1 n1 line2 n2} $changesList {
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
        lappend leftMergeData $data1
        lappend rightMergeData $data2

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
        lappend leftMergeData $data1
        lappend rightMergeData $data2
        set mergeSelection($changeNo) 2
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
    lappend leftMergeData $data1
    lappend rightMergeData $data2

    close $ch1
    close $ch2

    if {$diff(mode) == "RCS" || $diff(mode) == "CVS"} {
        cleanupRCS
    } elseif {[string match "conflict*" $diff(mode)]} {
        cleanupConflict
    }
}

# Fill up the merge window with the initial version of merged files.
proc fillMergeWindow {} {
    global mergeSelection leftMergeData rightMergeData curMergeSel curMerge

    set w .merge.t
    $w delete 1.0 end
    set marks {}
    set t 0
    foreach {commLeft diffLeft} $leftMergeData \
            {commRight diffRight} $rightMergeData {
        $w insert end $commRight
        if {![info exists mergeSelection($t)]} continue
        $w mark set merges$t insert
        $w mark gravity merges$t left
        $w insert end $diffRight merge$t
        lappend marks mergee$t [$w index insert]
        set mergeSelection($t) 2
        incr t
    }
    foreach {mark index} $marks {
        $w mark set $mark $index
    }
    set curMerge 0
    set curMergeSel 2
    $w tag configure merge0 -foreground red
    showDiff 0
    update
    seeText $w merges0 mergee0
}

# Move to and highlight another diff.
proc nextMerge {delta} {
    global mergeSelection curMergeSel curMerge leftMergeData

    set w .merge.t
    $w tag configure merge$curMerge -foreground ""

    set curMerge [expr {$curMerge + $delta}]
    if {$curMerge < 0} {set curMerge 0}
    if {$curMerge >= ([llength $leftMergeData] / 2)} {
        set curMerge [expr {[llength $leftMergeData] / 2 - 1}]
    }
    set curMergeSel $mergeSelection($curMerge)
    $w tag configure merge$curMerge -foreground red
    showDiff $curMerge
    seeText $w merges$curMerge mergee$curMerge
}

# Select a merge setting for all diffs.
proc selectMergeAll {new} {
    global leftMergeData curMerge curMergeSel
    set end [expr {[llength $leftMergeData] / 2}]
    for {set t 0} {$t < $end} {incr t} {
        selectMerge2 $t $new
    }
    set curMergeSel $new
    set w .merge.t
    seeText $w merges$curMerge mergee$curMerge
}

# Change merge setting fo current diff.
proc selectMerge {} {
    global curMergeSel curMerge

    set w .merge.t
    selectMerge2 $curMerge $curMergeSel
    seeText $w merges$curMerge mergee$curMerge
}

# Change merge setting for a diff.
proc selectMerge2 {no new} {
    global mergeSelection
    global leftMergeData rightMergeData

    set w .merge.t
    # Delete current string
    $w delete merges$no mergee$no

    set mergeSelection($no) $new
    
    set i [expr {$no * 2 + 1}]
    set diffLeft [lindex $leftMergeData $i]
    set diffRight [lindex $rightMergeData $i]

    if {$mergeSelection($no) == 12} {
        $w insert merges$no $diffLeft$diffRight merge$no
    } elseif {$mergeSelection($no) == 21} {
        $w insert merges$no $diffRight$diffLeft merge$no
    } elseif {$mergeSelection($no) == 1} {
        $w insert merges$no $diffLeft merge$no
    } elseif {$mergeSelection($no) == 2} {
        $w insert merges$no $diffRight merge$no
    }
}

# Save the merge result.
proc saveMerge {} {
    global diff

    set w .merge.t

    if {$diff(mergeFile) == ""} {
        if {[info exists diff(rightDir)]} {
            set initDir $diff(rightDir)
        } elseif {[info exists diff(leftDir)]} {
            set initDir $diff(leftDir)
        } else {
            set initDir [pwd]
        }

        set apa [tk_getSaveFile -title "Save merge file" -initialdir $initDir]
        if {$apa == ""} return
        set diff(mergeFile) $apa
    }

    set ch [open $diff(mergeFile) w]
    puts -nonewline $ch [$w get 1.0 end-1char]
    close $ch
    tk_messageBox -parent .merge -icon info -type ok -title "Diff" \
            -message "Saved merge to file $diff(mergeFile)."
}

# Close merge window and clean up.
proc closeMerge {} {
    global mergeSelection leftMergeData rightMergeData

    destroy .merge
    set leftMergeData {}
    set rightMergeData {}
    array unset mergeSelection
}

# Create a window to display merge result.
proc makeMergeWin {} {
    set w .merge
    set geometry ""
    if {![winfo exists $w]} {
        toplevel $w
    } else {
        eval destroy [winfo children $w]
    }

    wm title $w "Merge result"

    frame $w.f

    radiobutton $w.f.rb1 -text "LR" -value 12 -variable curMergeSel \
            -command selectMerge
    radiobutton $w.f.rb2 -text "L"  -value 1  -variable curMergeSel \
            -command selectMerge
    radiobutton $w.f.rb3 -text "R"  -value 2  -variable curMergeSel \
            -command selectMerge
    radiobutton $w.f.rb4 -text "RL" -value 21 -variable curMergeSel \
            -command selectMerge
    bind $w <Key-Left>  {focus .merge ; set curMergeSel 1 ; selectMerge}
    bind $w <Key-Right> {focus .merge ; set curMergeSel 2 ; selectMerge}

    button $w.f.bl -text "All L" -command {selectMergeAll 1}
    button $w.f.br -text "All R" -command {selectMergeAll 2}
    checkbutton $w.f.bm -text "Pure" -variable diff(mode) \
            -onvalue "conflictPure" -offvalue "conflict" -command {doDiff}

    button $w.f.b1 -text "Prev" -command {nextMerge -1}
    button $w.f.b2 -text "Next" -command {nextMerge 1}
    bind $w <Key-Down> {focus .merge ; nextMerge 1}
    bind $w <Key-Up>   {focus .merge ; nextMerge -1}

    button $w.f.bs -text "Save" -command saveMerge
    button $w.f.bq -text "Close" -command closeMerge
    wm protocol $w WM_CLOSE_WINDOW closeMerge

    grid $w.f.rb1 $w.f.rb2 $w.f.rb3 $w.f.rb4 x $w.f.b1 $w.f.b2 x \
            $w.f.bl $w.f.br x $w.f.bm x $w.f.bs $w.f.bq
    grid columnconfigure $w.f {4 7 10 12} -minsize 10
    grid columnconfigure $w.f 10 -weight 1

    text $w.t -width 80 -height 20 -xscrollcommand "$w.sbx set" \
            -yscrollcommand "$w.sby set" -font myfont
    scrollbar $w.sbx -orient horizontal -command "$w.t xview"
    scrollbar $w.sby -orient vertical   -command "$w.t yview"

    grid $w.f   -      -sticky news -row 0
    grid $w.t   $w.sby -sticky news
    grid $w.sbx x      -sticky we
    grid columnconfigure $w 0 -weight 1
    grid rowconfigure $w 1 -weight 1

    collectMergeData
    fillMergeWindow
}

#####################################
# Searching
#####################################

proc startIncrementalSearch {w} {

    if {![info exists ::diff(isearch)]} {
        set ::diff(isearch) ""
        set ::diff(isearchlast) ""
    }

    # This shouldn't happen
    if {$::diff(isearch) != ""} {
        endIncrementalSearch $w
    }

    set ::diff(isearch) $w
    
    bind MyText <Control-Key-s> "isearchAgain %W ; break"
    bind MyText <FocusOut> "endIncrementalSearch %W"
    bind MyText <Key> "isearchKey %W %A %s %K"
    bind MyText <Key-Escape> "endIncrementalSearch %W ; break"
    bind MyText <Control-Key-g> "endIncrementalSearch %W ; break"
    bind MyText <Key-Delete> "isearchBack %W ; break"
    bind MyText <Key-BackSpace> "isearchBack %W ; break"
    
    set ::diff(isearchstring) ""
    set ::diff(isearchhistory) {}
    set ::diff(isearchindex) [$w index insert]
    set ::diff(statusLabel) "i"
}

proc isearchShow {w index string} {
    $w tag remove sel 1.0 end
    $w tag add sel $index "$index + [string length $string] chars"
    $w mark set insert $index
    $w see $index

    set ::diff(isearchindex) $index
    set ::diff(isearchstring) $string
    set ::diff(isearchlast) $string
}

proc isearchSearch {w str ix} {
    if {[string equal [string tolower $str] $str]} {
        set found [$w search -nocase $str $ix]
    } else {
        set found [$w search $str $ix]
    }
    return $found
}

proc isearchAgain {w} {
    if {$w != $::diff(isearch)} {
        bell
        endIncrementalSearch $::diff(isearch)
        return
    }

    set str $::diff(isearchstring)
    if {$str == ""} {
        set str $::diff(isearchlast)
    }
    set found [isearchSearch $w $str "$::diff(isearchindex) + 1 char"]
    if {$found == ""} {
        bell
        return
    }
    lappend ::diff(isearchhistory) $::diff(isearchindex) \
            $::diff(isearchstring)
    isearchShow $w $found $str
}

proc isearchKey {w key state sym} {
    if {$w != $::diff(isearch)} {
        bell
        endIncrementalSearch $::diff(isearch)
        return -code break
    }

    if {$key == ""} {
        # Ignore the Control and Shift keys
        if {[string match Contr* $sym]} {return -code break}
        if {[string match Shift* $sym]} {return -code break}
        # Ignore any Control-ed key
        if {$state == 4} {return -code break}
        # Break isearch on other non-ascii keys, and let it through
        bell
        endIncrementalSearch $::diff(isearch)
        return
    }

    set str $::diff(isearchstring)
    append str $key

    set found [isearchSearch $w $str $::diff(isearchindex)]
    if {$found == ""} {
        bell
        return -code break
    }
    lappend ::diff(isearchhistory) $::diff(isearchindex) \
            $::diff(isearchstring)
    isearchShow $w $found $str
    return -code break
}

proc isearchBack {w} {
    if {$w != $::diff(isearch)} {
        bell
        endIncrementalSearch $::diff(isearch)
        return
    }
    if {[llength $::diff(isearchhistory)] < 2} {
        bell
        return
    }
    
    set str [lindex $::diff(isearchhistory) end]
    set found [lindex $::diff(isearchhistory) end-1]
    set ::diff(isearchhistory) [lrange $::diff(isearchhistory) 0 end-2]
    
    isearchShow $w $found $str
}

proc endIncrementalSearch {w} {

    set ::diff(isearch) ""
    set ::diff(statusLabel) ""

    # Remove all bindings from MyText
    foreach b [bind MyText] {
        bind MyText $b ""
    }
    
    bind MyText <Control-Key-s> "startIncrementalSearch %W"
}

#####################################
# Printing stuff
#####################################

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
                set gray $::grayLevel1
            } elseif {[string match "new*" $value]} {
                set gray $::grayLevel2
            }
        } elseif {$key == "tagoff"} {
            if {$value == "change" || [string match "new*" $value]} {
                set gray 1.0
            }
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
        set n [expr {(- $i - $index - 1) % 8 + 1}]
        set text [string replace $text $i $i [format %${n}s ""]]
    }
    return $text
}

# Main print function
proc printDiffs {{quiet 0}} {
    busyCursor
    update idletasks
    set tmpFile [file nativename ~/tcldiff.enscript]
    set tmpFile2 [file nativename ~/tcldifftmp.ps]
    if {$::diff(printFile) != ""} {
        set tmpFile3 [file nativename $::diff(printFile)]
    } else {
        set tmpFile3 [file nativename ~/tcldiff.ps]
    }

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
                    while {$chars + $len > 84} {
                        set wrap [expr {84 - $chars}]
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
                        set gray $::grayLevel1
                        append line "\0bggray\{$gray\}"
                    } elseif {$value != "last"} {
                        set gray $::grayLevel2
                        append line "\0bggray\{$gray\}"
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
        if {$i < 66} {puts -nonewline $ch "\f"}
        for {set i 0} {$i < 66 && $i2 < $len2} {incr i ; incr i2} {
            puts $ch [lindex $wraplines2 $i2]
        }
        if {$i < 66} {puts -nonewline $ch "\f"}
    }

    close $ch

    if {$::tcl_platform(platform) == "windows" &&\
            ![info exists env(ENSCRIPT_LIBRARY)]} {
        set ::env(ENSCRIPT_LIBRARY) [pwd]
    }
    set enscriptCmd [list enscript -2jcre]
    if {![regexp {^(.*)( \(.*?\))$} $::diff(leftLabel) -> lfile lrest]} {
        set lfile $::diff(leftLabel)
        set lrest ""
    }
    set lfile [file tail $lfile]$lrest
    if {![regexp {^(.*)( \(.*?\))$} $::diff(rightLabel) -> rfile rrest]} {
        set rfile $::diff(rightLabel)
        set rrest ""
    }
    set rfile [file tail $rfile]$rrest

    lappend enscriptCmd "--header=$lfile|Page \$% of \$=|$rfile"
    if {$::prettyPrint != ""} {
        lappend enscriptCmd -E$::prettyPrint
    }
    lappend enscriptCmd -p $tmpFile3 $tmpFile

    if {[catch {eval exec $enscriptCmd} result]} {
        if {[string index $result 0] != "\["} {
            tk_messageBox -message "Enscript error: $result"
            return
        }
    }

    normalCursor
    if {!$quiet} {
        destroy .dp
        toplevel .dp
        wm title .dp "Diff Print"
        button .dp.b -text Close -command {destroy .dp}
        label .dp.l -anchor w -justify left -text "The following files have\
                been created:\n\n$tmpFile\nInput file to enscript.\
                \n\n$tmpFile3\nCreated with\
                '[lrange $enscriptCmd 0 end-3] \\\n             \
                [lrange $enscriptCmd end-2 end]'" \
                -font "Courier 8"
        pack .dp.b -side bottom
        pack .dp.l -side top
    }
}

# Create a print dialog.
proc doPrint {{quiet 0}} {
    
    if {![info exists ::grayLevel1]} {
        set ::grayLevel1 0.6
        set ::grayLevel2 0.8
    }
    if {$quiet} {
        printDiffs 1
        return
    }

    destroy .pr
    toplevel .pr
    wm title .pr "Print diffs"

    label .pr.l1 -justify left -anchor w \
            -text "The print function is just on an\
            experimental level. It will use 'enscript' to write a postcript\
            file \"tcldiff.ps\" in your home directory."
    label .pr.l2 -justify left -anchor w \
            -text "Below you can adjust the gray scale\
            levels that are used on the background to mark changes.\
            The first value is used for changed text. The second for\
            new/deleted text."
    .pr.l1 configure -wraplength 400
    .pr.l2 configure -wraplength 400

    scale .pr.s1 -orient horizontal -resolution 0.1 -showvalue 1 -from 0.0 \
            -to 1.0 -variable grayLevel1
    scale .pr.s2 -orient horizontal -resolution 0.1 -showvalue 1 -from 0.0 \
            -to 1.0 -variable grayLevel2
    frame .pr.f
    radiobutton .pr.r1 -text "No Syntax" -variable prettyPrint -value ""
    radiobutton .pr.r2 -text "VHDL" -variable prettyPrint -value "vhdl"
    radiobutton .pr.r3 -text "Tcl"  -variable prettyPrint -value "tcl"
    radiobutton .pr.r4 -text "C"    -variable prettyPrint -value "c"

    button .pr.b1 -text Print -width 7 \
            -command {destroy .pr; update; printDiffs}
    button .pr.b2 -text Cancel -width 7 \
            -command {destroy .pr}

    grid .pr.l1 - -sticky we
    grid .pr.l2 - -sticky we
    grid .pr.s1 - -sticky we
    grid .pr.s2 - -sticky we
    grid .pr.f  - -sticky we
    grid .pr.b1 .pr.b2 -sticky w -padx 5 -pady 5
    grid .pr.b2 -sticky e
    pack .pr.r1 .pr.r2 .pr.r3 .pr.r4 -in .pr.f -side left -fill x -expand 1

}

#####################################
# GUI stuff
#####################################

proc zoomRow {w X Y x y} {
    global Pref
    # Get the row that was clicked
    set index [$w index @$x,$y]
    set row [lindex [split $index "."] 0]

    # Extract the data
    set data1 [.ft1.tt dump -tag -text $row.0 $row.end]
    set data2 [.ft2.tt dump -tag -text $row.0 $row.end]
    if {[llength $data1] == 0 && [llength $data2] == 0} return

    set font [.ft1.tt cget -font]
    set wx $X
    set wy [expr {$Y + 4}]

    destroy .balloon
    toplevel .balloon -bg black
    wm withdraw .balloon
    wm overrideredirect .balloon 1
    
    set wid 0
    foreach x {1 2} {
        text .balloon.t$x -relief flat -font $font -bg #ffffaa -fg black \
                -padx 2 -pady 0 -height 1 -wrap word
        .balloon.t$x tag configure new1 -foreground $Pref(colornew1) \
                -background $Pref(bgnew1)
        .balloon.t$x tag configure change -foreground $Pref(colorchange) \
                -background $Pref(bgchange)
        .balloon.t$x tag configure new2 -foreground $Pref(colornew2) \
                -background $Pref(bgnew2)
        pack .balloon.t$x -side top -padx 1 -pady 1 -fill both -expand 1
        
        set data [set data$x]
        set tags {}
        foreach {key value index} $data {
            if {$key == "tagon"} {
                lappend tags $value
                set tags [lsort -unique $tags]
            } elseif {$key == "tagoff"} {
                set i [lsearch $tags $value]
                if {$i >= 0} {
                    set tags [lreplace $tags $i $i]
                }
            } else {
                .balloon.t$x insert end $value $tags
            }
        }
        set text [.balloon.t$x get 1.0 1.end]
        regsub -all "\t" $text "        " text
        .balloon.t$x configure -width [string length $text]
    }
    
    # Let geometry requests propagate
    update idletasks

    # Is the balloon within the diff window?
    set wid [winfo reqwidth .balloon]
    if {$wid + $wx > [winfo rootx .] + [winfo width .]} {
        # No.
        # Center on diff window
        set wx [expr {([winfo width .] - $wid) / 2 + [winfo rootx .]}]
        if {$wx < 0} {set wx 0}
        # Is the balloon not within the screen?
        if {$wx + $wid > [winfo screenwidth .]} {
            # Center in screen
            set wx [expr {([winfo screenwidth .] - $wid) / 2}]
            if {$wx < 0} {set wx 0}
        }
    }
            
    # Does the balloon fit within the screen?
    if {$wid > [winfo screenwidth .]} {
        # How many rows does it take?
        set rows [expr {ceil(double($wid) / [winfo screenwidth .])}]
        # Add rows and fill screen width
        .balloon.t1 configure -height $rows
        .balloon.t2 configure -height $rows
        # Let geometry requests propagate
        update idletasks
        wm geometry .balloon \
                [winfo screenwidth .]x[winfo reqheight .balloon]
        set wx 0
    }
    wm geometry .balloon +$wx+$wy
    wm deiconify .balloon
}

proc unzoomRow {} {
    destroy .balloon
}

# Procedures for common y-scroll
proc my_yview args {
    foreach w {.ft1.tl .ft1.tt .ft2.tl .ft2.tt} {
        eval $w yview $args
    }
}

proc my_yscroll args {
    eval .sby set $args
    my_yview moveto [lindex $args 0]
}

# Reconfigure font
proc chFont {} {
    global Pref

    font configure myfont -size $Pref(fontsize) -family $Pref(fontfamily)
}

# Change color settings
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

# Scroll text windows
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
    .mf.m add command -label "Open Conflict File" -command openConflict
    .mf.m add command -label "Open Patch File" -command openPatch
    if {$tcl_platform(platform) == "unix"} {
        .mf.m add command -label "RCSDiff" -underline 0 -command openRCS
        .mf.m add command -label "CVSDiff" -underline 0 -command openCVS
    }
    .mf.m add separator
    .mf.m add command -label "Print" -underline 0 -command doPrint
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

    menubutton .ms -text Search -underline 0 -menu .ms.m 
    menu .ms.m
    .ms.m add command -label "Find"      -accelerator "Ctrl+f" -command Search
    .ms.m add command -label "Find Next" -accelerator "F3" \
            -command SearchNext
    .ms.m add command -label "Find Prev" -accelerator "Ctrl+F3" \
            -command SearchPrev

    menubutton .mh -text Help -underline 0 -menu .mh.m
    menu .mh.m
    .mh.m add command -label "Help" -command makeHelpWin
    .mh.m add command -label "About" -command makeAboutWin

    button .bfn -text "Next Diff" -relief raised -command {findDiff 1}
    button .bfp -text "Prev Diff" -relief raised -command {findDiff -1}
    entry .eo -width 10 -textvariable Pref(dopt)
    label .lo -text "Diff Options"

    catch {font delete myfont}
    font create myfont -family $Pref(fontfamily) -size $Pref(fontsize)

    label .l1 -textvariable diff(leftLabel) -anchor e -width 10
    label .l2 -textvariable diff(rightLabel) -anchor e -width 10

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

    bind MyText <Control-Key-s> "startIncrementalSearch %W"
    bindtags .ft1.tt "MyText [bindtags .ft1.tt]"
    bindtags .ft2.tt "MyText [bindtags .ft2.tt]"

    label .le -textvariable ::diff(eqLabel) -width 1
    label .ls -textvariable ::diff(statusLabel) -width 1 -pady 0 -padx 0
    canvas .c -width 6 -bd 0 -selectborderwidth 0 -highlightthickness 0

    applyColor
    .ft1.tt tag configure last -underline 1
    .ft2.tt tag configure last -underline 1
    foreach w {.ft1.tt .ft1.tl .ft2.tt .ft2.tl} {
        bind $w <ButtonPress-3> "zoomRow %W %X %Y %x %y"
        bind $w <ButtonRelease-3> "unzoomRow"
    }

    grid .l1   .le -    .l2   -row 1 -sticky news
    grid .ft1  .c  .sby .ft2  -row 2 -sticky news
    grid .sbx1 .ls -    .sbx2 -row 3 -sticky news
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
    bind . <Control-Key-f> {Search}
    bind . <Key-F3> {SearchNext}
    bind . <Control-Key-F3> {SearchPrev}

    pack .mf .mo .ms .mh -in .f -side left
    pack .bfn .bfp .eo .lo -in .f -side right
    if {$debug == 1} {
        menubutton .md -text Debug -menu .md.m -relief ridge
        menu .md.m
        if {$tcl_platform(platform) == "windows"} {
            .md.m add checkbutton -label Console -variable consolestate \
                    -onvalue show -offvalue hide \
                    -command {console $consolestate}
            .md.m add separator
        }
        .md.m add checkbutton -label Wrap -variable wrapstate -onvalue char\
                -offvalue none -command {.ft1.tt configure -wrap $wrapstate ;\
                .ft2.tt configure -wrap $wrapstate}
        .md.m add command -label "Merge" -command {makeMergeWin}
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
        .md.m add command -label "Nuisance" -command {makeNuisance \
                "It looks like you are trying out the debug menu."}
        pack .md -in .f -side left -padx 20
    }
}

# Set new preferences.
proc applyPref {} {
    global Pref TmpPref

    array set Pref [array get TmpPref]
    applyColor
}

# Update test color fields.
proc testColor {} {
    global TmpPref

    .pr.fc.t1 tag configure change -foreground $TmpPref(colorchange) \
            -background $TmpPref(bgchange)
    .pr.fc.t2 tag configure new1 -foreground $TmpPref(colornew1) \
            -background $TmpPref(bgnew1)
    .pr.fc.t3 tag configure new2 -foreground $TmpPref(colornew2) \
            -background $TmpPref(bgnew2)
}

# Color dialog.
proc selColor {name} {
    global TmpPref

    set t [tk_chooseColor -parent .pr -initialcolor $TmpPref($name)]
    if {$t != ""} {
        set TmpPref($name) $t
    }
}

# Create a windoe for changing preferences.
# Currently only colors are changed in this dialog.
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

# Change font preference
proc applyFont {} {
    global Pref TmpPref

    set Pref(fontsize) $TmpPref(fontsize)

    set i [lindex [.fo.lb curselection] 0]
    set Pref(fontfamily) [.fo.lb get $i]

    chFont
}

# Update example font
proc exampleFont {} {
    global TmpPref
    set i [lindex [.fo.lb curselection] 0]
    if {$i == ""} return
    set TmpPref(fontfamily) [.fo.lb get $i]

    font configure tmpfont -family $TmpPref(fontfamily)
    if {[string is integer -strict $TmpPref(fontsize)]} {
        font configure tmpfont -size $TmpPref(fontsize)
    }
}

# Font dialog
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

#####################################
# Help and startup functions
#####################################

proc makeNuisance {{str {Hi there!}}} {
    global thisdir

    if {[lsearch [image names] nuisance] < 0} {
        set file [file join $thisdir Nuisance.gif]
        if {![file exists $file]} return
        image create photo nuisance -file $file
    }

    destroy .nui
    toplevel .nui
    wm transient .nui .
    wm geometry .nui +400+400
    wm title .nui ""
    label .nui.l -image nuisance
    pack .nui.l
    wm protocol .nui WM_DELETE_WINDOW {destroy .nui2 .nui}
    update
    
    destroy .nui2
    toplevel .nui2 -bg yellow
    wm transient .nui2 .nui
    wm overrideredirect .nui2 1
    wm title .nui2 ""
    label .nui2.l -text "$str\nDo you want help?" -justify left -bg yellow
    button .nui2.b -text "No, get out of my face!" -command {destroy .nui2 .nui} -bg yellow
    pack .nui2.l .nui2.b -side top -fill x
    wm geometry .nui2 +[expr {405 + [winfo width .nui]}]+400
}

proc makeAboutWin {} {
    global diffver
    destroy .ab

    toplevel .ab
    wm title .ab "About Diff.tcl"
    text .ab.t -width 45 -height 11 -wrap word
    button .ab.b -text "Close" -command "destroy .ab"
    pack .ab.b -side bottom
    pack .ab.t -side top -expand y -fill both

    .ab.t insert end "A Tcl/Tk frontend to diff\n\n"
    .ab.t insert end "$diffver\n"
    .ab.t insert end "Made by Peter Spjuth\n"
    .ab.t insert end "E-Mail: peter.spjuth@space.se\n\n"
    .ab.t insert end "Credits:\n"
    .ab.t insert end "Ideas for scrollbar map and merge function\n"
    .ab.t insert end "taken from TkDiff\n"
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
  Redo Diff         : Run diff again on the same files.
  Open Both         : Select two files, run diff.
  Open Left File    : Select a file for left window, run diff
  Open Right File   : Select a file for right window, run diff
  Open Conflict File: Select a file containing conflicts such as from
                      a CVS merge.
  Open Patch File   : Display a patch file created by diff -c or diff -u.
  RCSDiff           : (UNIX only) Select one file and diff like rcsdiff.
  CVSDiff           : (UNIX only) Select one file and diff like cvs diff.
  Print             : Experimental print function.
                      It currently creates a postscript file ~/tcldiff.ps
  Quit              : Guess

Options Menu
  Font     : Select font and fontsize for the two main text windows
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
                      that look the same and place them abreast. The "small"
                      version do not parse big blocks to avoid long runs.
             The Char and Word options selects if the line parsing should
             highlight full words only, or check single characters.
             2nd stage  : More thorough parsing of a line.
             Mark last  : Last change of a line is underlined
  Colours  : Choose highlight colours.
  Diffs only : Only differing lines will be displayed.
  Force crlf translation : (Windows only) Use crlf mode when reading files.
  Save default: Save current option settings in ~/.diffrc

Diff Options Field: Any text written here will be passed to diff.
                    In RCS/CVS mode, any -r options will be used to select
                    versions.

Prev Diff Button: Scrolls to the previous differing block, or to the top
                  if there are no more diffs.
Next Diff Button: Scrolls to the next differing block, or to the bottom
                  if there are no more diffs.

Equal sign: Above the vertical scrollbar, a "=" will appear if the files
            are equal.

} "" {Bindings} ul {

Up, Down, Page Up and Page Down scrolls main windows.

Escape takes focus out of text windows.

Right mouse button "zooms" a line of text.

Ctrl-s starts incremental search. Incremental search is stopped by Escape
  or Ctrl-g.

Ctrl-f brings up search dialog. F3 is "search again".

} "" {Merge Window (Appears in conflict mode)} ul {

You can, for each difference, select which version you want to appear
in the output file. The buttons "LR", "L", "R" and "RL" select the
lines from the left file, right file or both files.
"Prev" and "Next" buttons moves between differences.
"All L" and "All R" buttons select "L" or "R" for all differences.
"Pure" ...
"Save" saves the merge result to a file.

On the keyboard, Up and Down keys means the same as "Prev" and "Next".
Left and Right keys selects "L" and "R".

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

proc printUsage {} {
    puts {Usage: diff.tcl [options] [file1] [file2]
  [options]              All options but the ones listed below
                         are passed to diff.
  [file1],[file2]        Files to be compared
                         If no files are given, the program is
                         started anyway and you can select files
                         from within.
                         If only one file is given, the program
                         looks for an RCS/CVS directory next to the
                         file, and if found, runs in RCS/CVS mode.
  Options:

  -nodiff     : Normally, if there are enough information on the
                command line to run diff, diff.tcl will do so unless
                this option is specified.

  -noparse    : Diff.tcl can perform analysis of changed blocks to
  -line       : improve display. See online help for details.
  -smallblock : The default. Do block analysis on small blocks.
  -block      : Full block analysis. This can be slow if there
                are large change blocks.

  -char       : The analysis of changes can be done on either
  -word       : character or word basis. -char is the default.

  -2nd        : Turn on or off second stage parsing.
  -no2nd      : It is on by default.

  -noignore   : Don't ignore any whitespace.
  -b          : Ignore space changes. Default.
  -w          : Ignore all spaces.

  -conflict   : Treat file as a merge conflict file and enter merge
                mode.
  -o <file>   : Specify merge result output file. 

  -browse     : Automatically bring up file dialog after starting.
  -server     : Set up diff to be controllable from the outside.

  -print <file> : Generate postscript and exit.

  -limit <lines> : Do not process more than <lines> lines.
}
}

proc parseCommandLine {} {
    global diff Pref
    global argv argc tcl_platform

    set diff(leftOK) 0
    set diff(rightOK) 0
    set diff(mode) ""
    set diff(printFile) ""
    set noautodiff 0
    set autobrowse 0
    set diff(mergeFile) ""
    set diff(conflictFile) ""
    set diff(limitlines) 0

    if {$argc == 0} return

    set files ""
    set nextArg ""
    foreach arg $argv {
        if {$nextArg != ""} {
            if {$nextArg == "mergeFile"} {
                set diff(mergeFile) [file join [pwd] $arg]
            } elseif {$nextArg == "printFile"} {
                set diff(printFile) [file join [pwd] $arg]
            } elseif {$nextArg == "revision"} {
                set Pref(dopt) "$Pref(dopt) -r$arg"
            } elseif {$nextArg == "limitlines"} {
                set diff(limitlines) $arg
            }
            set nextArg ""
            continue
        }
        if {$arg == "-w"} {
            set Pref(ignore) "-w"
        } elseif {$arg == "-h"} {
            printUsage
            exit
        } elseif {$arg == "-b"} {
            set Pref(ignore) "-b"
        } elseif {$arg == "-noignore"} {
            set Pref(ignore) " "
        } elseif {$arg == "-noparse"} {
            set Pref(parse) 0
        } elseif {$arg == "-line"} {
            set Pref(parse) 1
        } elseif {$arg == "-smallblock"} {
            set Pref(parse) 2
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
        } elseif {$arg == "-limit"} {
            set nextArg limitlines
        } elseif {$arg == "-nodiff"} {
            set noautodiff 1
        } elseif {$arg == "-browse"} {
            set autobrowse 1
        } elseif {$arg == "-conflict"} {
            set diff(mode) "conflict"
            set Pref(ignore) " "
        } elseif {$arg == "-print"} {
            set nextArg printFile
        } elseif {$arg == "-server"} {
            if {$tcl_platform(platform) == "windows"} {
                dde servername Diff
            } else {
                tk appname Diff
            }
        } elseif {$arg == "-o"} {
            set nextArg mergeFile
        } elseif {$arg == "-r"} {
            set nextArg revision
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
        set fullname [file join [pwd] [lindex $files 0]]
        set fulldir [file dirname $fullname]
        if {$diff(mode) == "conflict"} {
            set diff(conflictFile) $fullname
            set diff(rightDir) $fulldir
            set diff(rightOK) 1
            set diff(rightLabel) $fullname
            set diff(leftLabel) $fullname
            after idle doDiff
            return
        } elseif {!$autobrowse && \
                [llength [glob -nocomplain [file join $fulldir RCS]]]} {
            set diff(mode) "RCS"
            set diff(rightDir) $fulldir
            set diff(RCSFile) $fullname
            set diff(rightLabel) $fullname
            set diff(rightFile) $fullname
            set diff(rightOK) 1
            set diff(leftLabel) "RCS"
            if {$noautodiff} {
                enableRedo
            } else {
                after idle doDiff
            }
        } elseif {!$autobrowse && \
                [llength [glob -nocomplain [file join $fulldir CVS]]]} {
            set diff(mode) "CVS"
            set diff(rightDir) $fulldir
            set diff(RCSFile) $fullname
            set diff(rightLabel) $fullname
            set diff(rightFile) $fullname
            set diff(rightOK) 1
            set diff(leftLabel) "CVS"
            if {$noautodiff} {
                enableRedo
            } else {
                after idle doDiff
            }
        } else {
            set diff(leftDir) $fulldir
            set diff(leftFile) $fullname
            set diff(leftLabel) $fullname
            set diff(leftOK) 1
            if {[string match "*.diff" $fullname]} {
                set diff(mode) "patch"
                set diff(patchFile) $fullname
                set autobrowse 0
                if {$noautodiff} {
                    enableRedo
                } else {
                    after idle doDiff
                }
            }   
        }
    } elseif {$len >= 2} {
        set fullname [file join [pwd] [lindex $files 0]]
        set fulldir [file dirname $fullname]
        set diff(leftDir) $fulldir
        set diff(leftFile) $fullname
        set diff(leftLabel) $fullname
        set diff(leftOK) 1
        set fullname [file join [pwd] [lindex $files 1]]
        set fulldir [file dirname $fullname]
        set diff(rightDir) $fulldir
        set diff(rightFile) $fullname
        set diff(rightLabel) $fullname
        set diff(rightOK) 1
        if {$noautodiff} {
            enableRedo
        } else {
            after idle doDiff
        }
    }
    if {$autobrowse && (!$diff(leftOK) || !$diff(rightOK))} {
        if {!$diff(leftOK) && !$diff(rightOK)} {
            openBoth 0
        } elseif {!$diff(leftOK)} {
            openLeft
        } elseif {!$diff(rightOK)} {
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
    set Pref(context) 2
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

# Searching contributed by Ulf Nilsson
# FIXA, test this properly and incorporate into the rest of the file

# Dialog functions from "Practical Programming in Tcl And Tk" by Welch.
proc Dialog_Create {top title args} {
    global dialog
    if {[winfo exists $top]} {
        switch -- [wm state $top] {
            normal {
                # Raise a buried window
                raise $top
            }
            withdrawn -
            iconified {
                # Open and restore geometry
                wm deiconify $top
                catch {wm geometry $top $dialog(geo,$top)}
            }
        }
        return 0
    } else {
        eval {toplevel $top} $args
        wm title $top $title
        return 1
    }
}

proc Dialog_Wait {top varName {focus {}}} {
    upvar $varName var
    
    # Poke the variable if the user nukes the window
    bind $top <Destroy> [list set $varName $var]
    
    # Grab focus for the dialog
    if {[string length $focus] == 0} {
        set focus $top
    }
    set old [focus -displayof $top]
    focus $focus
    catch {tkwait visibility $top}
    catch {grab $top}
    
    # Wait for the dialog to complete
    tkwait variable $varName
    catch {grab release $top}
    focus $old
}

proc Dialog_Dismiss {top} {
    global dialog
    # Save current size and position
    catch {
        # window may have been deleted
        set dialog(geo,$top) [wm geometry $top]
        wm withdraw $top
    }
}

proc Find_Dialog { string } {
    global prompt CaseSensitive
    set f .prompt
    if {[Dialog_Create $f "Find" -borderwidth 10]} {
        message $f.msg -text $string -aspect 1000
        entry $f.entry -textvariable prompt(result)
        
        checkbutton $f.case -text "Match Case" -variable ::diff(searchcase)
        pack $f.case -side right
        
        set b [frame $f.buttons]
        pack $f.msg $f.entry $f.buttons -side top -fill x
        pack $f.entry -pady 5
        button $b.ok -text OK -command {set prompt(ok) 1}
        button $b.cancel -text Cancel \
                -command {set prompt(ok) 0}
		
		
        pack $b.ok -side left
        pack $b.cancel -side right
        bind $f.entry <Return> {set prompt(ok) 1 ; break}
        bind $f.entry <Key-Escape> {set prompt(ok) 0 ; break}
        
    }
    set prompt(ok) 0
    Dialog_Wait $f prompt(ok) $f.entry
    Dialog_Dismiss $f
    if {$prompt(ok)} {
        return $prompt(result)
    } else {
        return {}
    }
}

proc Search {} {
    if {![info exists diff(searchcase)]} {
        set ::diff(searchcase) 0
        set ::diff(searchindex) 1.0
        set ::diff(searchstring) ""
    }

    set ::diff(searchwin) .ft1.tt
    if {[focus -displayof .] == ".ft2.tt"} {
        set ::diff(searchwin) .ft2.tt
    }
    set ::diff(searchstring) [Find_Dialog "Please enter string to find"]

    if {$::diff(searchcase)} {
        set searchpos [$::diff(searchwin) search -count cnt \
                $::diff(searchstring) @0,0]
    } else {
        set searchpos [$::diff(searchwin) search -count cnt -nocase \
                $::diff(searchstring) @0,0]
    }
    if {$searchpos == ""} {
        tk_messageBox -message "Search string not found!" -type ok -title Diff
        return
    }

    $::diff(searchwin) see $searchpos
    $::diff(searchwin) tag remove sel 1.0 end
    $::diff(searchwin) tag add sel $searchpos "$searchpos + $cnt chars"
    set ::diff(searchindex) $searchpos
}      

proc SearchNext {} {
    if {$::diff(searchcase)} {
        set searchpos [$::diff(searchwin) search -count cnt \
                $::diff(searchstring) "$::diff(searchindex) + 1 chars"]
    } else {
        set searchpos [$::diff(searchwin) search -count cnt -nocase \
                $::diff(searchstring) "$::diff(searchindex) + 1 chars"]
    }
    
    if {$searchpos == "" || $searchpos == $::diff(searchindex)} {
        tk_messageBox -message "String not found!" -type ok -title Diff
        return
    }

    $::diff(searchwin) see $searchpos
    $::diff(searchwin) tag remove sel 1.0 end
    $::diff(searchwin) tag add sel $searchpos "$searchpos + $cnt chars"
    set ::diff(searchindex) $searchpos
}

proc SearchPrev {} {
    if {$::diff(searchcase)} {
        set searchpos [$::diff(searchwin) search -count cnt -backwards \
                $::diff(searchstring) "$::diff(searchindex) - 1 chars"]
    } else {
        set searchpos [$::diff(searchwin) search -count cnt -backwards \
                -nocase \
                $::diff(searchstring) "$::diff(searchindex) - 1 chars"]
    }
    
    if {$searchpos == "" || $searchpos == $::diff(searchindex)} {
        tk_messageBox -message "String not found!" -type ok -title Diff
        return
    }

    $::diff(searchwin) see $searchpos
    $::diff(searchwin) tag remove sel 1.0 end
    $::diff(searchwin) tag add sel $searchpos "$searchpos + $cnt chars"
    set ::diff(searchindex) $searchpos
}
