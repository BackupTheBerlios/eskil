#!/bin/sh
#----------------------------------------------------------------------
#
#  diff.tcl, a Graphical frontend to diff
#
#  Copyright (c) 1998-2003, Peter Spjuth  (peter.spjuth@space.se)
#
#  Usage
#             Do 'diff.tcl' for interactive mode
#             Do 'diff.tcl --help' for command line usage
#
#----------------------------------------------------------------------
# $Revision$
#----------------------------------------------------------------------
# the next line restarts using wish \
exec wish "$0" "$@"

package provide app-diff 1.0
package require Tk
catch {package require textSearch}
if {[catch {package require psballoon}]} {
    proc addBalloon {args} {}
} else {
    namespace import -force psballoon::addBalloon
}

set debug 0
set diffver "Version 1.9.8  2003-08-13"
set tmpcnt 0
set tmpfiles {}
set thisscript [file join [pwd] [info script]]
set thisdir [file dirname $thisscript]
set ::diff(cvsExists) [expr {![string equal [auto_execok cvs] ""]}]
set ::diff(diffexe) diff
set ::diff(thisexe) [list [info nameofexecutable] $thisscript]

# Experimenting with DiffUtil package
set ::diff(diffutil) [expr {![catch {package require DiffUtil}]}]
set ::diff(diffutil) 0

if {[info exists env(TEMP)]} {
    set ::diff(tmpdir) $env(TEMP)
} elseif {[info exists env(TMP)]} {
    set ::diff(tmpdir) $env(TMP)
} else {
    if {$tcl_platform(platform) == "windows"} {
        set ::diff(tmpdir) c:/
    } else {
        set ::diff(tmpdir) .
    }
}

# Support for FreeWrap.
if {[info proc ::freewrap::unpack] != ""} {
    set debug 0
    set thisdir [pwd]
    set thisscript ""
    set ::diff(thisexe) [list [info nameofexecutable]]
    # If diff.exe is wrapped, copy it so we can use it.
    set apa [::freewrap::unpack /diff.exe]
    if {$apa != ""} {
        set ::diff(diffexe) $apa
    }
}

if {$tcl_platform(platform) == "windows"} {
    cd $thisdir
    catch {package require dde}
    if {!$::diff(cvsExists) && [file exists "c:/bin/cvs.exe"]} {
        set env(PATH) "$env(PATH);c:\\bin"
        auto_reset
        set ::diff(cvsExists) [expr {![string equal [auto_execok cvs] ""]}]
    }
}

proc cleanupAndExit {} {
    if {$::diff(diffexe) != "diff"} {
        catch {file delete $::diff(diffexe)}
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
    incr ::tmpcnt
    set name [file join $::diff(tmpdir) "tmpd[pid]a$::tmpcnt"]
    lappend ::tmpfiles $name
    return $name
}

proc cleartmp {} {
    foreach f $::tmpfiles {
        catch {file delete $f}
    }
    set ::tmpfiles {}
}

# 2nd stage line parsing
# Recursively look for common substrings in strings s1 and s2
# The strings are known to not have anything in common at start or end.
# The return value is, for each string, a list where the second, fourth etc.
# element is equal between the strings.
# This is sort of a Longest Common Subsequence algorithm but with
# a preference for long consecutive substrings, and it does not look
# for really small substrings.
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

# Experiment using DiffUtil
proc compareLinesX {line1 line2 res1Name res2Name {test 0}} {
    global Pref
    upvar $res1Name res1
    upvar $res2Name res2

    set args "$Pref(ignore)\
            [expr {($Pref(lineparsewords) && !$test) ? "-word" : ""}]"
    eval DiffUtil::compareLines $args \$line1 \$line2 res1 res2
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

    # This processes the lines from both ends first.
    # A typical line has few changes thus this gets rid of most
    # equalities. The middle part is then optionally parsed further.

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
    if {$leftp1 > $t1 && $leftp2 > $t2} {
        set res1 [list $line1]
        set res2 [list $line2]
    } else {
        set right1 [string range $line1 [expr {$t1 + 1}] end]
        set mid1 [string range $line1 $leftp1 $t1]
        set left1 [string range $line1 0 [expr {$leftp1 - 1}]]
        set res1 [list $left1 $mid1 $right1]

        set right2 [string range $line2 [expr {$t2 + 1}] end]
        set mid2 [string range $line2 $leftp2 $t2]
        set left2 [string range $line2 0 [expr {$leftp2 - 1}]]
        set res2 [list $left2 $mid2 $right2]

        if {$Pref(extralineparse) != 0 && $mid1 != "" && $mid2 != ""} {
            compareMidString $mid1 $mid2 mid1 mid2 $test

            # Replace middle element in res* with list elements from mid*
            #set res1 [eval lreplace \$res1 1 1 $mid1]
            #set res2 [eval lreplace \$res2 1 1 $mid2]
            # This makes use of pure-list optimisation in eval
            set res1 [eval [linsert $mid1 0 lreplace $res1 1 1]]
            set res2 [eval [linsert $mid2 0 lreplace $res2 1 1]]
        }
    }
}

# Compare two lines and rate how much they resemble each other.
# This has never worked well. Some day I'll sit down, think this through,
# and come up with a better algorithm.
proc compareLines2 {line1 line2} {
    compareLines $line1 $line2 res1 res2 1
    if {$::diff(diffutil)} {
        compareLinesX $line1 $line2 xres1 xres2 1
        if {$res1 != $xres1 || $res2 != $xres2} {
            tk_messageBox -title "Rate Mismatch!" \
                    -message ":$res1:\n:$res2:\n:$xres1:\n:$xres2:"
        }
    }
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
#    puts "Same ($sames)"
#    puts "D1   ($diffs1)"
#    puts "D2   ($diffs2)"
#    puts "S $sumsame D $sumdiff1 D $sumdiff2"
    return [expr {$sumsame - [maxabs $sumdiff1 $sumdiff2]}]
}

# Decide how to display change blocks
# This tries to match the lines that resemble each other and put them
# next to each other.
# As the previous procedure, this would need a complete rework and a
# better algorithm.
proc compareBlocks {block1 block2} {
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
#    puts "Bestsum: $bestsum"

    # origresult holds a mapping between blocks where each row
    # is paired with its best match. This may not be a possible
    # result since it has to be in order.

    array set bestresult [array get origresult]
    set bestscoresum -100000

    # If the size is 1, it is automatically in order so we
    # don't need further processing.
    if {$size1 > 1} {

	# If both blocks are the same size, try first with the
	# simple row to row match, as a base score
	if {$size1 == $size2} {
	    set sum 0
	    array unset result
	    for {set i 0} {$i < $size1} {incr i} {
		set result($i) $i
		incr sum $scores($i,$i)
	    }
#	    puts "Simple map sum: $sum"
	    array set bestresult [array get result]
	    set bestscoresum $sum
	}

	# If result is in order, no problem.
	# Otherwise, try to adjust result to make it ordered
        while {1} {
	    # The outer loop restarts from the "best mapping"
            array unset result
            array set result [array get origresult]
            for {set i 0} {$i < $size1} {incr i} {
                set mark($i) 0
            }

            while {1} {
		# The inner loop tries to get the result in order
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
#            puts "Scoresum: $scoresum ($bestscoresum)"

	    # If it was not an improvement over previous iteration, quit
            if {$scoresum <= $bestscoresum} {
                break
	    }

	    array set bestresult [array get result]
	    set bestscoresum $scoresum
	    # If it is close enough to the theoretical max, take it
	    if {$bestscoresum >= (3 * $bestsum / 4)} {
		break
	    }

	    # We are redoing from start, but try to improve by
	    # ignoring the most awkwardly placed line.
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
#	    puts "Most $mosti $mostp"
	    set scores(best,$mosti) 0
        }
    }

    array set result [array get bestresult]

    # Collect the result into diff-like codes to use as display info.

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
                #if {[string match Wm* [lindex $block2 $t2]]} {
                #    puts "Left : [lindex $block1 $t1]"
                #    puts "Right: [lindex $block2 $t2]"
                #    puts "Score: $scores($t1,$t2)"
                #}

                # If the score is too bad, don't do line parsing.
                if {$scores($t1,$t2) < 0} {
                    lappend apa "C"
                } else {
                    lappend apa "c"
                }
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

    if {$::diff(filter) != ""} {
        if {[regexp $::diff(filter) $line1]} {
            insert 1 $doingLine1 $line1
            insert 2 $doingLine2 $line2
            incr doingLine1
            incr doingLine2
            set ::diff(filterflag) 1
            return
        }
        set ::diff(filterflag) 0
    }

    if {$Pref(parse) != 0} {
        compareLines $line1 $line2 res1 res2
        if {$::diff(diffutil)} {
            compareLinesX $line1 $line2 xres1 xres2
            if {$res1 != $xres1 || $res2 != $xres2} {
                tk_messageBox -title Mismatch! \
                        -message ":$res1:\n:$res2:\n:$xres1:\n:$xres2:"
            }
        }
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

# Insert two blocks of lines in the compare windows.
# Returns number of lines used to display the blocks
proc insertMatchingBlocks {block1 block2} {
    global doingLine1 doingLine2

    set apa [compareBlocks $block1 $block2]

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
        if {$c == "C"} {
	    # This is two lines that the block matching considered
	    # too different to use line parsing on them.
	    # Marked the whole line as deleted/inserted
            set textline1 [lindex $block1 $t1]
            set textline2 [lindex $block2 $t2]
            .ft1.tl insert end [myforml $doingLine1] \
                    "hl$::HighLightCount change"
            .ft1.tt insert end "$textline1\n" new1
            .ft2.tl insert end [myforml $doingLine2] \
                    "hl$::HighLightCount change"
            .ft2.tt insert end "$textline2\n" new2
            incr doingLine1
            incr doingLine2
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
	# Consider any total limit on displayed lines.
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

    # Is this a change block, a delete block or a insert block?
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
    # This should not happen unless something is wrong...
    if {$doingLine2 != $line2} {
        .ft1.tt insert end "**Bad alignment here!! $doingLine2 $line2**\n"
        .ft2.tt insert end "**Bad alignment here!! $doingLine2 $line2**\n"
        .ft1.tl insert end "\n"
        .ft2.tl insert end "\n"
    }

    # Process the block

    if {$n1 == $n2 && ($n1 == 1 || $Pref(parse) < 2)} {
        # Never do block parsing for one line blocks.
        # If block parsing is turned off, only do line parsing for
        # blocks of equal size.
        for {set t 0} {$t < $n1} {incr t} {
            gets $ch1 textline1
            gets $ch2 textline2
            insertMatchingLines $textline1 $textline2
        }
        if {$::diff(filter) != "" &&  $::diff(filterflag)} {

        } else {
            lappend changesList $mapMax $n1 change $line1 $n1 $line2 $n2
        }
        incr mapMax $n1
    } else {
        if {$n1 != 0 && $n2 != 0 && $Pref(parse) >= 2 && \
                ($n1 * $n2 < 1000 || $Pref(parse) == 3)} {
            # Full block parsing
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
            # No extra parsing at all.
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
    .mf.m entryconfigure 0 -state normal
}

proc disableRedo {} {
    .mf.m entryconfigure 0 -state disabled
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

# Display one chunk from a patch file
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
            incr ::mapMax [insertMatchingBlocks $lblock $rblock]
            set lblock {}
            set rblock {}
        }
        if {$lmode == "" && $rmode == ""} {
            insert 1 $lline $lstr
            insert 2 $rline $rstr
            incr leftc
            incr rightc
            incr ::mapMax
            continue
        }
        if {$lmode == "-"} {
            insert 1 $lline $lstr new1
            emptyline 2
            incr leftc
            incr ::mapMax
            continue
        }
        if {$rmode == "+"} {
            insert 2 $rline $rstr new2
            emptyline 1
            incr rightc
            incr ::mapMax
            continue
        }
    }
}

# Read a patch file and display it
proc displayPatch {} {
    global diff Pref changesList mapMax

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
            lappend ::changesList $mapMax 4 change 0 0 0 0
            incr mapMax 4
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

# Get a CVS revision
proc execCvsUpdate {filename outfile args} {
    set old ""
    set dir [file dirname $filename]
    if {$dir != "."} {
        set old [pwd]
        set outfile [file join [pwd] $outfile]
        cd $dir
        set filename [file tail $filename]
    }

    set cmd $args
    set cmd [linsert $args 0 exec cvs -z3 update -p]
    lappend cmd [file nativename $filename] > $outfile
    if {[catch {eval $cmd} res]} {
        if {![string match "*Checking out*" $res]} {
            tk_messageBox -icon error -title "CVS error" -message $res
        }
    }

    if {$old != ""} {
        cd $old
    }
}

# Prepare for RCS/CVS diff. Checkout copies of the versions needed.
proc prepareRCS {} {
    global diff Pref

    set revs {}

    # Search for revision options
    if {$Pref(doptrev1) != ""} {
        lappend revs $Pref(doptrev1)
    }
    if {$Pref(doptrev2) != ""} {
        lappend revs $Pref(doptrev2)
    }

    switch [llength $revs] {
        0 {
            # Compare local file with latest version.
            set diff(leftFile) [tmpfile]
            set diff(rightLabel) $diff(RCSFile)
            set diff(rightFile) $diff(RCSFile)

            if {$diff(mode) == "CVS"} {
                set diff(leftLabel) "$diff(RCSFile) (CVS)"
                execCvsUpdate $diff(RCSFile) $diff(leftFile)
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
                execCvsUpdate $diff(RCSFile) $diff(leftFile) -r $r
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
                execCvsUpdate $diff(RCSFile) $diff(leftFile) -r $r1
                execCvsUpdate $diff(RCSFile) $diff(rightFile) -r $r2
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
}

# Clean up after a RCS/CVS diff.
proc cleanupRCS {} {
    global diff Pref

    cleartmp
    set diff(rightFile) $diff(RCSFile)
    set diff(leftFile) $diff(RCSFile)
}

# Prepare for a diff by creating needed temporary files
proc prepareFiles {} {
    if {$::diff(mode) == "RCS" || $::diff(mode) == "CVS"} {
        prepareRCS
    } elseif {[string match "conflict*" $::diff(mode)]} {
        prepareConflict
    }
}

# Clean up after a diff
proc cleanupFiles {} {
    if {$::diff(mode) == "RCS" || $::diff(mode) == "CVS"} {
        cleanupRCS
    } elseif {[string match "conflict*" $::diff(mode)]} {
        cleanupConflict
    }
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

    # Clear up everything before starting processing
    foreach w {.ft1.tl .ft1.tt .ft2.tl .ft2.tt} {
        $w configure -state normal
        $w delete 1.0 end
    }
    set changesList {}
    set mapMax 0
    set ::HighLightCount 0
    highLightChange -1
    drawMap -1
    # Display a star during diff execution, to know when the internal
    # processing starts, and when the label is "valid".
    set ::diff(eqLabel) "*"

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
    } else {
        prepareFiles
    }

    # Run diff and parse the result.
    if {$::diff(diffutil)} {
        set differr [catch {eval DiffUtil::diffFiles $Pref(ignore) \
                \$diff(leftFile) \$diff(rightFile)} diffres]
    } else {
        set differr [catch {eval exec \$::diff(diffexe) \
                $Pref(dopt) $Pref(ignore) \
                \$diff(leftFile) \$diff(rightFile)} diffres]
    }
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
    # Update the equal label immediately for better feedback
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
        if {$::diff(limitlines) && $::mapMax > $::diff(limitlines)} {
            break
        }
        if {[incr t] >= 10} {
	    update idletasks
	    .ft2.tl see end
	    update idletasks
            set t 0
        }
        bindHighlight
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
        set d [expr {int($max) - int([$w index end])}]
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

    cleanupFiles
    if {[string match "conflict*" $diff(mode)]} {
        if {$::diff(eqLabel) != "="} {
            makeMergeWin
        }
    }
    if {$diff(printFile) != ""} {
        after idle {doPrint 1 ; cleanupAndExit}
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

    button $w.f.b1 -text "Prev" -command {nextMerge -1}
    button $w.f.b2 -text "Next" -command {nextMerge 1}
    bind $w <Key-Down> {focus .merge ; nextMerge 1}
    bind $w <Key-Up>   {focus .merge ; nextMerge -1}
    bind $w <Shift-Key-Down> {focus .merge ; nextMerge 10}
    bind $w <Shift-Key-Up>   {focus .merge ; nextMerge -10}

    button $w.f.bs -text "Save" -command saveMerge
    button $w.f.bq -text "Close" -command closeMerge
    wm protocol $w WM_CLOSE_WINDOW closeMerge

    grid $w.f.rb1 $w.f.rb2 $w.f.rb3 $w.f.rb4 x $w.f.b1 $w.f.b2 x \
            $w.f.bl $w.f.br x x x $w.f.bs $w.f.bq
    grid columnconfigure $w.f {4 7 10 12} -minsize 10
    grid columnconfigure $w.f 10 -weight 1

    if {[string match conflict* $::diff(mode)]} {
        checkbutton $w.f.bm -text "Pure" -variable diff(mode) \
                -onvalue "conflictPure" -offvalue "conflict" -command {doDiff}
        grid $w.f.bm -row 0 -column 11
    }

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
    if {$::diff(printFile) != ""} {
        set tmpFile2 [file nativename $::diff(printFile)]
    } else {
        set tmpFile2 [file nativename ~/tcldiff.ps]
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
            ![info exists ::env(ENSCRIPT_LIBRARY)]} {
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
    lappend enscriptCmd -p $tmpFile2 $tmpFile

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
                \n\n$tmpFile2\nCreated with\
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

proc formatAlignPattern {p} {
    set raw [binary format I $p]
    binary scan $raw B* bin
    set bin [string trimleft [string range $bin 0 end-8] 0][string range $bin end-7 end]
    set pat [string map {0 . 1 ,} $bin]
    return $pat
}

proc runAlign {} {
    if {![info exists ::diff(aligns)] || [llength $::diff(aligns)] == 0} {
        return
    }

    set pattern 0
    foreach align $::diff(aligns) {
        foreach {lline rline level} $align break

        set pre {}
        set post {}
        for {set t 1} {$t <= $level} {incr t} {
            lappend pre [formatAlignPattern $pattern]
            incr pattern
            lappend post [formatAlignPattern $pattern]
            incr pattern
        }

        set fix1($lline) [list [join $pre \n] [join $post \n]]
        set fix2($rline) [list [join $pre \n] [join $post \n]]
    }

    prepareFiles
    foreach n {1 2} src {leftFile rightFile} {
        set tmp [tmpfile]
        set f$n $tmp
        set cho [open $tmp w]
        #puts $cho hej
        set chi [open $::diff($src) r]
        set lineNo 1
        while {[gets $chi line] >= 0} {
            if {[info exists fix${n}($lineNo)]} {
                foreach {pre post} [set fix${n}($lineNo)] break
                puts $cho $pre
                puts $cho $line
                puts $cho $post
            } else {
                puts $cho $line
            }
            incr lineNo
        }
        close $cho
        close $chi
    }
    # FIXA : detta tar bort tmpfiles
    cleanupFiles

    catch {eval exec $::diff(thisexe) \$f1 \$f2 &}

    set ::diff(aligns) ""
}

# Mark a line as aligned.
proc markAlign {n line text} {
    set ::diff(align$n) $line
    set ::diff(aligntext$n) $text

    if {[info exists ::diff(align1)] && [info exists ::diff(align2)]} {
        set level 1
        if {![string equal $::diff(aligntext1) $::diff(aligntext2)]} {
            set apa [tk_messageBox -icon question -title "Align" -type yesno \
                    -message "Those lines are not equal.\nReally align them?"]
            if {$apa != "yes"} {
                return
            }
            set level 3
        }

        lappend ::diff(aligns) [list $::diff(align1) $::diff(align2) $level]

        unset ::diff(align1)
        unset ::diff(align2)
    }
}

# Called by popup menus over row numbers to add command for alignment.
# Returns 1 of nothing added.
proc alignMenu {m n x y} {
    # Get the row that was clicked
    set index [.ft$n.tl index @$x,$y]
    set row [lindex [split $index "."] 0]

    set data [.ft$n.tl get $row.0 $row.end]
    if {![regexp {\d+} $data line]} {
        return 1
    }
    set text [.ft$n.tt get $row.0 $row.end]

    set other [expr {$n == 1 ? 2 : 1}]
    if {![info exists ::diff(align$other)]} {
        set label "Mark line for alignment"
    } else {
        set label "Align with line $::diff(align$other) on other side"
    }

    .lpm add command -label $label -command [list markAlign $n $line $text]
    return 0
}


proc hlSelect {hl} {
    highLightChange $hl
}

proc hlSeparate {n hl} {
    set ::diff(separate$n) $hl
    if {$hl == ""} {
        set range [.ft$n.tt tag ranges sel]
    } else {
        set range [.ft$n.tl tag ranges hl$::diff(separate$n)]
    }
    set text [eval .ft$n.tt get $range]
    set ::diff(separatetext$n) $text

    if {[info exists ::diff(separate1)] && [info exists ::diff(separate2)]} {
        set f1 [tmpfile]
        set f2 [tmpfile]
        set ch [open $f1 w]
        puts $ch $::diff(separatetext1)
        close $ch
        set ch [open $f2 w]
        puts $ch $::diff(separatetext2)
        close $ch

        catch {eval exec $::diff(thisexe) \$f1 \$f2 &}

        unset ::diff(separate1)
        unset ::diff(separate2)
    }
}

proc hlPopup {n hl X Y x y} {
    if {[info exists ::diff(nopopup)] && $::diff(nopopup)} return
    destroy .lpm
    menu .lpm -tearoff 0

    if {$hl != ""} {
        .lpm add command -label "Select" \
                -command [list hlSelect $hl]
    }

    set other [expr {$n == 1 ? 2 : 1}]
    if {![info exists ::diff(separate$other)]} {
        set label "Mark for Separate Diff"
    } else {
        set label "Separate Diff"
    }

    .lpm add command -label $label -command [list hlSeparate $n $hl]
    alignMenu .lpm $n $x $y

    set ::diff(nopopup) 1
    tk_popup .lpm $X $Y
    after idle {after 1 {set ::diff(nopopup) 0}}

    return
}

proc rowPopup {w X Y x y} {
    if {[info exists ::diff(nopopup)] && $::diff(nopopup)} return
    destroy .lpm
    menu .lpm -tearoff 0

    regexp {\d+} $w n
    if {[alignMenu .lpm $n $x $y]} {
        return
    }

    set ::diff(nopopup) 1
    tk_popup .lpm $X $Y
    after idle {after 1 {set ::diff(nopopup) 0}}
}

proc bindHighlight {} {
    set tag hl$::HighLightCount
    foreach n {1 2} {
        .ft$n.tl tag bind $tag <ButtonPress-3> \
                "hlPopup $n $::HighLightCount %X %Y %x %y ; break"
        .ft$n.tl tag bind $tag <ButtonPress-1> \
                "hlSelect $::HighLightCount"
    }
}

proc zoomRow {w X Y x y} {
    global Pref
    # Get the row that was clicked
    set index [$w index @$x,$y]
    set row [lindex [split $index "."] 0]

    # Check if it is selected
    if {[lsearch [$w tag names $index] sel] >= 0} {
        regexp {\d+} $w n
        hlPopup $n "" $X $Y $x $y
        return
    }

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

# Emulate a label that:
# 1 : Displays the right part of the text if there isn't enough room
# 2 : Justfify text to the left if there is enough room.
# 3 : Does not try to allocate space according to its contents
proc fileLabel {w args} {
    eval label $w $args
    set fg [$w cget -foreground]
    set bg [$w cget -background]
    set font [$w cget -font]
    destroy $w

    entry $w -relief flat -bd 0 -highlightthickness 0 \
            -foreground $fg -background $bg -font $font
    eval $w configure $args

    $w configure -takefocus 0 -state disabled
    if {[info tclversion] >= 8.4} {
        $w configure -state readonly -readonlybackground $bg
    }

    set i [lsearch $args -textvariable]
    if {$i >= 0} {
	set var [lindex $args [expr {$i + 1}]]
	uplevel #0 "trace variable $var w \
		{after idle {$w xview end} ;#}"
    }
}

# Build the main window
proc makeDiffWin {} {
    global Pref tcl_platform debug
    eval destroy [winfo children .]

    option add *Menu.tearOff 0
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
    .mf.m add command -label "Open Both..." -underline 0 -command {openBoth 0}
    .mf.m add command -label "Open Both (forget)..." -command {openBoth 1}
    .mf.m add command -label "Open Left File..." -command openLeft
    .mf.m add command -label "Open Right File..." -command openRight
    .mf.m add command -label "Open Conflict File..." -command openConflict
    .mf.m add command -label "Open Patch File..." -command openPatch
    if {$tcl_platform(platform) == "unix"} {
        .mf.m add command -label "RCSDiff..." -underline 0 -command openRCS
    }
    if {$::diff(cvsExists)} {
        .mf.m add command -label "CVSDiff..." -underline 0 -command openCVS
    }
    .mf.m add separator
    .mf.m add command -label "Print..." -underline 0 -command doPrint
    .mf.m add separator
    .mf.m add command -label "Quit" -underline 0 -command cleanupAndExit

    menubutton .mo -text "Options" -underline 0 -menu .mo.m
    menu .mo.m
    .mo.m add cascade -label "Font" -underline 0 -menu .mo.mf
    .mo.m add cascade -label "Ignore" -underline 0 -menu .mo.mi
    .mo.m add cascade -label "Parse" -underline 0 -menu .mo.mp
    .mo.m add command -label "Colours..." -underline 0 -command makePrefWin
    .mo.m add checkbutton -label "Diffs only" -variable Pref(onlydiffs)
    if {$tcl_platform(platform) == "windows"} {
        .mo.m add checkbutton -label "Force crlf translation" \
                -variable Pref(crlf)
    }
    .mo.m add separator
    .mo.m add command -label "Save default" -command saveOptions

    menu .mo.mf
    .mo.mf add command -label "Select..." -command makeFontWin
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
    if {[info proc textSearch::searchMenu] != ""} {
        textSearch::searchMenu .ms.m
    } else {
        .ms.m add command -label "Text search not available" -state disabled
    }

    menubutton .mh -text Help -underline 0 -menu .mh.m
    menu .mh.m
    .mh.m add command -label "Help" -command makeHelpWin
    .mh.m add command -label "About" -command makeAboutWin

    label .lo -text "Diff Options"
    addBalloon .lo "Options passed to the external diff.\nNote\
            that options for ignoring whitespace are available in\
            the Options menu."
    entry .eo -width 6 -textvariable Pref(dopt)
    label .lr1 -text "Rev 1"
    addBalloon .lr1 "Revision number for CVS/RCS diff."
    entry .er1 -width 6 -textvariable Pref(doptrev1)
    label .lr2 -text "Rev 2"
    addBalloon .lr2 "Revision number for CVS/RCS diff."
    entry .er2 -width 6 -textvariable Pref(doptrev2)
    button .bfp -text "Prev Diff" -relief raised -command {findDiff -1} \
            -underline 0 -padx 15
    button .bfn -text "Next Diff" -relief raised -command {findDiff 1} \
            -underline 0 -padx 15
    bind . <Alt-n> {findDiff 1}
    bind . <Alt-p> {findDiff -1}

    catch {font delete myfont}
    font create myfont -family $Pref(fontfamily) -size $Pref(fontsize)

    #label .l1 -textvariable diff(leftLabel) -anchor e -width 10
    #label .l2 -textvariable diff(rightLabel) -anchor e -width 10
    fileLabel .l1 -textvariable diff(leftLabel)
    fileLabel .l2 -textvariable diff(rightLabel)

    frame .ft1 -borderwidth 2 -relief sunken
    text .ft1.tl -height 40 -width 5 -wrap none -yscrollcommand my_yscroll \
            -font myfont -borderwidth 0 -padx 0 -highlightthickness 0 \
            -takefocus 0
    text .ft1.tt -height 40 -width 80 -wrap none -yscrollcommand my_yscroll \
            -xscrollcommand ".sbx1 set" -font myfont -borderwidth 0 -padx 1 \
            -highlightthickness 0
    frame .ft1.f -width 2 -height 2 -bg lightgray
    pack .ft1.tl -side left -fill y
    pack .ft1.f -side left -fill y
    pack .ft1.tt -side right -fill both -expand 1
    scrollbar .sby -orient vertical -command "my_yview"
    scrollbar .sbx1 -orient horizontal -command ".ft1.tt xview"

    frame .ft2 -borderwidth 2 -relief sunken
    text .ft2.tl -height 60 -width 5 -wrap none -yscrollcommand my_yscroll \
            -font myfont -borderwidth 0 -padx 0 -highlightthickness 0 \
            -takefocus 0
    text .ft2.tt -height 60 -width 80 -wrap none -yscrollcommand my_yscroll \
            -xscrollcommand ".sbx2 set" -font myfont -borderwidth 0 -padx 1 \
            -highlightthickness 0
    frame .ft2.f -width 2 -height 2 -bg lightgray
    pack .ft2.tl -side left -fill y
    pack .ft2.f -side left -fill y
    pack .ft2.tt -side right -fill both -expand 1
    scrollbar .sbx2 -orient horizontal -command ".ft2.tt xview"

    # Set up a tag for incremental search bindings
    if {[info proc textSearch::enableSearch] != ""} {
        textSearch::enableSearch .ft1.tt -label ::diff(isearchLabel)
        textSearch::enableSearch .ft2.tt -label ::diff(isearchLabel)
    }

    label .le -textvariable ::diff(eqLabel) -width 1
    addBalloon .le "* means external diff is running.\n= means files do\
            not differ.\nBlank means files differ."
    label .ls -width 1 -pady 0 -padx 0 -textvariable ::diff(isearchLabel)
    addBalloon .ls "Incremental search indicator"
    canvas .c -width 6 -bd 0 -selectborderwidth 0 -highlightthickness 0


    applyColor
    .ft1.tt tag configure last -underline 1
    .ft2.tt tag configure last -underline 1
    foreach w {.ft1.tt .ft2.tt} {
        $w tag raise sel
        bind $w <ButtonPress-3> "zoomRow %W %X %Y %x %y"
        bind $w <ButtonRelease-3> "unzoomRow"
    }
    foreach w {.ft1.tl .ft2.tl} {
        bind $w <ButtonPress-3> "rowPopup %W %X %Y %x %y"
    }

    grid .l1   .le -    .l2   -row 1 -sticky news
    grid .ft1  .c  .sby .ft2  -row 2 -sticky news
    grid .sbx1 .ls -    .sbx2 -row 3 -sticky news
    grid columnconfigure . {0 3} -weight 1
    grid rowconfigure . 2 -weight 1
    grid .c -pady [expr {[.sby cget -width] + 2}]
    grid .ls -sticky ""
    
    image create photo map
    .c create image 0 0 -anchor nw -image map
    bind .c <Configure> {drawMap %h}

    bind . <Key-Up> {scroll -1 u}
    bind . <Key-Down> {scroll 1 u}
    bind . <Key-Prior> {scroll -1 p}
    bind . <Key-Next> {scroll 1 p}
    bind . <Key-Escape> {focus .}

    pack .mf .mo .ms .mh -in .f -side left
    pack .bfn -in .f -side right -padx {3 6}
    pack .bfp .er2 .lr2 .er1 .lr1 .eo .lo -in .f -side right -padx 3
    if {$debug == 1} {
        menubutton .md -text Debug -menu .md.m
        menu .md.m
        if {$tcl_platform(platform) == "windows"} {
            .md.m add checkbutton -label Console -variable consolestate \
                    -onvalue show -offvalue hide \
                    -command {console $consolestate}
            .md.m add separator
        }
        .md.m add radiobutton -label "Context 2" -variable ::Pref(context) -value 2
        .md.m add radiobutton -label "Context 5" -variable ::Pref(context) -value 5
        .md.m add radiobutton -label "Context 10" -variable ::Pref(context) -value 10
        .md.m add radiobutton -label "Context 20" -variable ::Pref(context) -value 20
        .md.m add separator
        .md.m add checkbutton -label Wrap -variable wrapstate -onvalue char\
                -offvalue none -command {.ft1.tt configure -wrap $wrapstate ;\
                .ft2.tt configure -wrap $wrapstate}
        .md.m add command -label "Merge" -command {makeMergeWin}
        .md.m add command -label "Date Filter" -command {set ::diff(filter) {^Date}}
        .md.m add command -label "Align" -command {runAlign}
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
    bind .ab <Key-Return> "destroy .ab"
    bind .ab <Key-Escape> "destroy .ab"
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
    bind .he <Key-Return> "destroy .he"
    bind .he <Key-Escape> "destroy .he"
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
                    In RCS/CVS mode, any -r options will be used internally
                    to select versions.

Prev Diff Button: Scrolls to the previous differing block, or to the top
                  if there are no more diffs.
Next Diff Button: Scrolls to the next differing block, or to the bottom
                  if there are no more diffs.

Equal sign: Above the vertical scrollbar, a "=" will appear if the files
            are equal. While the external diff executes a "*" is shown
            and is replaced with "=" or "" before the files are displayed
            to give that information early.

} "" {Bindings} ul {

Up, Down, Page Up and Page Down scrolls main windows.

Escape takes focus out of text windows.

Right mouse button "zooms" a line of text. If the text under the cursor
is selected, a menu appears where the selected text can be used for a
separate diff.

Ctrl-s starts incremental search. Incremental search is stopped by Escape
or Ctrl-g.

Ctrl-f brings up search dialog. F3 is "search again".

Left mouse click on the line number of a diff highlights it.

Right mouse click on the line number of a diff gives a menu where it can
be selected for separate diff. This can be used to check a block that has
been moved.

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

When saved it is the contents of the text widget that is saved which
means you can hand edit there if you are careful. E.g. you can't use
cursor keys, but you can type text, delete text and copy/paste with
mouse assistance.

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
    set revNo 1
    foreach arg $argv {
        if {$nextArg != ""} {
            if {$nextArg == "mergeFile"} {
                set diff(mergeFile) [file join [pwd] $arg]
            } elseif {$nextArg == "printFile"} {
                set diff(printFile) [file join [pwd] $arg]
            } elseif {$nextArg == "revision"} {
                set Pref(doptrev$revNo) $arg
                incr revNo
            } elseif {$nextArg == "limitlines"} {
                set diff(limitlines) $arg
            }
            set nextArg ""
            continue
        }
        if {$arg == "-w"} {
            set Pref(ignore) "-w"
        } elseif {$arg == "--help"} {
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
        } elseif {[string range $arg 0 1] == "-r"} {
            set Pref(doptrev$revNo) [string range $arg 2 end]
            incr revNo
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
        # If we cancel the second file and detect CVS, ask about it.
        if {$diff(leftOK) && !$diff(rightOK) && \
                [llength [glob -nocomplain [file join $fulldir CVS]]]} {

            if {[tk_messageBox -title Diff -icon question \
                    -message "Do CVS diff?" -type yesno] == "yes"} {
                set fulldir $diff(leftDir)
                set fullname $diff(leftFile)
                set diff(leftOK) 0
                set diff(mode) "CVS"
                set diff(rightDir) $fulldir
                set diff(RCSFile) $fullname
                set diff(rightLabel) $fullname
                set diff(rightFile) $fullname
                set diff(rightOK) 1
                set diff(leftLabel) "CVS"
                after idle doDiff
            }
        }
    }
}

proc saveOptions {} {
    global Pref

    if {[catch {set ch [open "~/.diffrc" w]} err]} {
        tk_messageBox -icon error -title "File error" -message \
                "Error when trying to save preferences:\n$err"
        return
    }

    foreach i [array names Pref] {
        if {![string match "dopt*" $i]} {
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
    set Pref(doptrev1) ""
    set Pref(doptrev2) ""
    set Pref(parse) 2
    set Pref(lineparsewords) "0"
    set Pref(extralineparse) 1
    set Pref(colorchange) red
    set Pref(colornew1) darkgreen
    set Pref(colornew2) blue
    set Pref(bgchange) #ffe0e0
    set Pref(bgnew1) #a0ffa0
    set Pref(bgnew2) #e0e0ff
    set Pref(onlydiffs) 0
    set Pref(context) 2
    set Pref(crlf) 0
    set Pref(marklast) 1
    set ::diff(filter) ""

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
