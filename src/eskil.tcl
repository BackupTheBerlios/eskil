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
package require Tcl 8.3
package require Tk
catch {package require textSearch}

if {[catch {package require psballoon}]} {
    # Add a dummy if it does not exists.
    proc addBalloon {args} {}
} else {
    namespace import -force psballoon::addBalloon
}

set debug 1
set diffver "Version 1.9.8+  2003-08-20"
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
if {[info procs ::freewrap::unpack] != ""} {
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

proc cleanupAndExit {top} {
    # A security thing to make sure we can exit.
    set cont 0
    if {[catch {
        if {$top != "all"} {
            set i [lsearch $::diff(diffWindows) $top]
            if {$i >= 0} {
                set ::diff(diffWindows) [lreplace $::diff(diffWindows) $i $i]
            }
            destroy $top
            array unset ::diff $top,*
            
            # Any windows remaining?
            if {[llength $::diff(diffWindows)] > 0} {
                set cont 1
            }
        }
    } errMsg]} {
        tk_messageBox -icon error -title "Diff Error" -message \
                "An error occured in the close process.\n$errMsg\n\
                (This is a bug)\nTerminating application." -type ok
    }
    if {$cont} return

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
    if {[info exists ::tmpcnt]} {
        incr ::tmpcnt
    } else {
        set ::tmpcnt 0
    }
    set name [file join $::diff(tmpdir) "tmpd[pid]a$::tmpcnt"]
    lappend ::tmpfiles $name
    return $name
}

proc cleartmp {} {
    if {[info exists ::tmpfiles]} {
        foreach f $::tmpfiles {
            catch {file delete $f}
        }
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
##syntax compareLinesX x x n n x?
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
proc insert {top n line text {tag {}}} {
    $::diff($top,wDiff$n) insert end "$text\n" $tag
    if {$tag != ""} {
        set tag "hl$::HighLightCount $tag"
    }
    $::diff($top,wLine$n) insert end [myforml $line] $tag
}

proc emptyline {top n {highlight 1}} {
    if {$highlight} {
        $::diff($top,wLine$n) insert end "\n" hl$::HighLightCount
    } else {
        $::diff($top,wLine$n) insert end "\n"
    }
    $::diff($top,wDiff$n) insert end "\n"
}

# Insert one line in each text widget.
# Mark them as changed, and optionally parse them.
proc insertMatchingLines {top line1 line2} {
    global doingLine1 doingLine2 Pref

    if {$::diff(filter) != ""} {
        if {[regexp $::diff(filter) $line1]} {
            insert $top 1 $doingLine1 $line1
            insert $top 2 $doingLine2 $line2
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
        $::diff($top,wLine1) insert end [myforml $doingLine1] "hl$::HighLightCount change"
        $::diff($top,wLine2) insert end [myforml $doingLine2] "hl$::HighLightCount change"
        set new1 "new1"
        set new2 "new2"
        set change "change"
        foreach i1 $res1 i2 $res2 {
            incr n -1
            if {$dotag} {
                if {$n == 1 && $Pref(marklast)} {
                    lappend new1 last
                    lappend new2 last
                    lappend change last
                }
                if {$i1 == ""} {
                    $::diff($top,wDiff2) insert end $i2 $new2
                } elseif {$i2 == ""} {
                    $::diff($top,wDiff1) insert end $i1 $new1
                } else {
                    $::diff($top,wDiff1) insert end $i1 $change
                    $::diff($top,wDiff2) insert end $i2 $change
                }
                set dotag 0
            } else {
                $::diff($top,wDiff1) insert end $i1
                $::diff($top,wDiff2) insert end $i2
                set dotag 1
            }
        }
        $::diff($top,wDiff1) insert end "\n"
        $::diff($top,wDiff2) insert end "\n"
    } else {
        insert $top 1 $doingLine1 $line1 "change"
        insert $top 2 $doingLine2 $line2 "change"
    }
    incr doingLine1
    incr doingLine2
}

# Insert two blocks of lines in the compare windows.
# Returns number of lines used to display the blocks
proc insertMatchingBlocks {top block1 block2} {
    global doingLine1 doingLine2

    set apa [compareBlocks $block1 $block2]

    set t1 0
    set t2 0
    foreach c $apa {
        if {$c == "c"} {
            set textline1 [lindex $block1 $t1]
            set textline2 [lindex $block2 $t2]
            insertMatchingLines $top $textline1 $textline2
            incr t1
            incr t2
        }
        if {$c == "C"} {
	    # This is two lines that the block matching considered
	    # too different to use line parsing on them.
	    # Marked the whole line as deleted/inserted
            set textline1 [lindex $block1 $t1]
            set textline2 [lindex $block2 $t2]
            $::diff($top,wLine1) insert end [myforml $doingLine1] \
                    "hl$::HighLightCount change"
            $::diff($top,wDiff1) insert end "$textline1\n" new1
            $::diff($top,wLine2) insert end [myforml $doingLine2] \
                    "hl$::HighLightCount change"
            $::diff($top,wDiff2) insert end "$textline2\n" new2
            incr doingLine1
            incr doingLine2
            incr t1
            incr t2
        }
        if {$c == "d"} {
            set bepa [lindex $block1 $t1]
            $::diff($top,wLine1) insert end [myforml $doingLine1] \
                    "hl$::HighLightCount change"
            $::diff($top,wDiff1) insert end "$bepa\n" new1
            emptyline $top 2
            incr doingLine1
            incr t1
        }
        if {$c == "a"} {
            set bepa [lindex $block2 $t2]
            $::diff($top,wLine2) insert end [myforml $doingLine2] \
                    "hl$::HighLightCount change"
            $::diff($top,wDiff2) insert end "$bepa\n" new2
            emptyline $top 1
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
proc dotext {top ch1 ch2 n1 n2 line1 line2} {
    global doingLine1 doingLine2 Pref mapMax changesList

    if {$n1 == 0 && $n2 == 0} {
        # All blocks have been processed. Continue until end of file.
        # If "only diffs" is on, just display a couple of context lines.
        set limit -1
        if {$Pref(onlydiffs) == 1} {
            set limit $Pref(context)
        }
	# Consider any total limit on displayed lines.
        if {$::diff($top,limitlines)} {
            set limit [expr {$::diff($top,limitlines) - $mapMax}]
            if {$limit < 0} {
                set limit 0
            }
        }
        set t 0
        while {[gets $ch2 apa] != -1} {
            insert $top 2 $doingLine2 $apa
            incr doingLine2
            incr mapMax
            incr t
            if {$limit >= 0 && $t >= $limit} break
        }
        set t 0
        while {[gets $ch1 apa] != -1} {
            insert $top 1 $doingLine1 $apa
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
            insert $top 1 $doingLine1 $apa
            insert $top 2 $doingLine2 $bepa
            incr mapMax
        } elseif {$t == $limit} {
            emptyline $top 1 0
            emptyline $top 2 0
            incr mapMax
        }
        incr doingLine1
        incr doingLine2
        incr t
        if {$::diff($top,limitlines) && $mapMax > $::diff($top,limitlines)} {
            return
        }
    }
    # This should not happen unless something is wrong...
    if {$doingLine2 != $line2} {
        $::diff($top,wDiff1) insert end \
                "**Bad alignment here!! $doingLine2 $line2**\n"
        $::diff($top,wDiff2) insert end \
                "**Bad alignment here!! $doingLine2 $line2**\n"
        $::diff($top,wLine1) insert end "\n"
        $::diff($top,wLine2) insert end "\n"
    }

    # Process the block

    if {$n1 == $n2 && ($n1 == 1 || $Pref(parse) < 2)} {
        # Never do block parsing for one line blocks.
        # If block parsing is turned off, only do line parsing for
        # blocks of equal size.
        for {set t 0} {$t < $n1} {incr t} {
            gets $ch1 textline1
            gets $ch2 textline2
            insertMatchingLines $top $textline1 $textline2
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
            set apa [insertMatchingBlocks $top $block1 $block2]

            lappend changesList $mapMax $apa change \
                    $line1 $n1 $line2 $n2
            incr mapMax $apa
        } else {
            # No extra parsing at all.
            for {set t 0} {$t < $n1} {incr t} {
                gets $ch1 apa
                insert $top 1 $doingLine1 $apa $tag1
                incr doingLine1
            }
            for {set t 0} {$t < $n2} {incr t} {
                gets $ch2 apa
                insert $top 2 $doingLine2 $apa $tag2
                incr doingLine2
            }
            if {$n1 <= $n2} {
                for {set t $n1} {$t < $n2} {incr t} {
                    emptyline $top 1
                }
                lappend changesList $mapMax $n2 $tag2 \
                        $line1 $n1 $line2 $n2
                incr mapMax $n2
            } elseif {$n2 < $n1} {
                for {set t $n2} {$t < $n1} {incr t} {
                    emptyline $top 2
                }
                lappend changesList $mapMax $n1 $tag1 \
                        $line1 $n1 $line2 $n2
                incr mapMax $n1
            }
        }
    }
}

proc enableRedo {top} {
    $top.mf.m entryconfigure 0 -state normal
}

proc disableRedo {top} {
    $top.mf.m entryconfigure 0 -state disabled
}

proc busyCursor {top} {
    global oldcursor oldcursor2
    if {![info exists oldcursor]} {
        set oldcursor [. cget -cursor]
        set oldcursor2 [$::diff($top,wDiff1) cget -cursor]
    }
    . config -cursor watch
    foreach item {wLine1 wDiff1 wLine2 wDiff2} {
        set w $::diff($top,$item)
        $w config -cursor watch
    }
}

proc normalCursor {top} {
    global oldcursor oldcursor2
    . config -cursor $oldcursor
    foreach item {wLine1 wDiff1 wLine2 wDiff2} {
        set w $::diff($top,$item)
        $w config -cursor $oldcursor2
    }
}

# Read a conflict file and extract the two versions.
proc prepareConflict {top} {
    global diff Pref

    set diff($top,leftFile) [tmpfile]
    set diff($top,rightFile) [tmpfile]

    set ch1 [open $diff($top,leftFile) w]
    set ch2 [open $diff($top,rightFile) w]
    set ch [open $diff($top,conflictFile) r]

    set diff($top,conflictDiff) {}
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
            lappend diff($top,conflictDiff) $start1,${end1}c$start2,$end2
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
        set leftName "No Conflict: [file tail $diff($top,conflictFile)]"
        set rightName $leftName
    }
    set diff($top,leftLabel) $leftName
    set diff($top,rightLabel) $rightName
    update idletasks
}

# Clean up after a conflict diff.
proc cleanupConflict {top} {
    global diff Pref

    #cleartmp ;# FIXA
    set diff($top,rightFile) $diff($top,conflictFile)
    set diff($top,leftFile) $diff($top,conflictFile)
}

# Display one chunk from a patch file
proc displayOnePatch {top leftLines rightLines leftLine rightLine} {
    emptyline $top 1
    emptyline $top 2

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
            incr ::mapMax [insertMatchingBlocks $top $lblock $rblock]
            set lblock {}
            set rblock {}
        }
        if {$lmode == "" && $rmode == ""} {
            insert $top 1 $lline $lstr
            insert $top 2 $rline $rstr
            incr leftc
            incr rightc
            incr ::mapMax
            continue
        }
        if {$lmode == "-"} {
            insert $top 1 $lline $lstr new1
            emptyline $top 2
            incr leftc
            incr ::mapMax
            continue
        }
        if {$rmode == "+"} {
            insert $top 2 $rline $rstr new2
            emptyline $top 1
            incr rightc
            incr ::mapMax
            continue
        }
    }
}

# Read a patch file and display it
proc displayPatch {top} {
    global diff Pref changesList mapMax

    set diff($top,leftLabel) "Patch $diff($top,patchFile): old"
    set diff($top,rightLabel) "Patch $diff($top,patchFile): new"
    update idletasks

    set ch [open $diff($top,patchFile) r]

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
                displayOnePatch $top $leftLines $rightLines $leftLine $rightLine
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
            emptyline $top 1
            insert $top 1 "" $divider
            insert $top 1 "" $sub
            insert $top 1 "" $divider
            lappend ::changesList $mapMax 4 change 0 0 0 0
            incr mapMax 4
            continue
        }
        if {$state == "newfile" && [regexp $rightRE $line -> sub]} {
            emptyline $top 2
            insert $top 2 "" $divider
            insert $top 2 "" $sub
            insert $top 2 "" $divider
            continue
        }
        # A new section in a -u style diff
        if {[regexp {^@@\s+-(\d+),\d+\s+\+(\d+),} $line -> sub1 sub2]} {
            if {$state == "both"} {
                displayOnePatch $top $leftLines $rightLines $leftLine $rightLine
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
                displayOnePatch $top $leftLines $rightLines $leftLine $rightLine
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
        displayOnePatch $top $leftLines $rightLines $leftLine $rightLine
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
proc prepareRCS {top} {
    global diff Pref

    set revs {}

    # Search for revision options
    if {$diff($top,doptrev1) != ""} {
        lappend revs $diff($top,doptrev1)
    }
    if {$diff($top,doptrev2) != ""} {
        lappend revs $diff($top,doptrev2)
    }

    switch [llength $revs] {
        0 {
            # Compare local file with latest version.
            set diff($top,leftFile) [tmpfile]
            set diff($top,rightLabel) $diff($top,RCSFile)
            set diff($top,rightFile) $diff($top,RCSFile)

            if {$diff($top,mode) == "CVS"} {
                set diff($top,leftLabel) "$diff($top,RCSFile) (CVS)"
                execCvsUpdate $diff($top,RCSFile) $diff($top,leftFile)
            } else {
                set diff($top,leftLabel) "$diff($top,RCSFile) (RCS)"
                catch {exec co -p [file nativename $diff($top,RCSFile)] \
                        > $diff($top,leftFile)}
            }
        }
        1 {
            # Compare local file with specified version.
            set r [lindex $revs 0]
            set diff($top,leftFile) [tmpfile]
            set diff($top,rightLabel) $diff($top,RCSFile)
            set diff($top,rightFile) $diff($top,RCSFile)

            if {$diff($top,mode) == "CVS"} {
                set diff($top,leftLabel) "$diff($top,RCSFile) (CVS $r)"
                execCvsUpdate $diff($top,RCSFile) $diff($top,leftFile) -r $r
            } else {
                set diff($top,leftLabel) "$diff($top,RCSFile) (RCS $r)"
                catch {exec co -p$r [file nativename $diff($top,RCSFile)] \
                        > $diff($top,leftFile)}
            }
        }
        default {
            # Compare the two specified versions.
            set r1 [lindex $revs 0]
            set r2 [lindex $revs 1]
            set diff($top,leftFile) [tmpfile]
            set diff($top,rightFile) [tmpfile]

            if {$diff($top,mode) == "CVS"} {
                set diff($top,leftLabel) "$diff($top,RCSFile) (CVS $r1)"
                set diff($top,rightLabel) "$diff($top,RCSFile) (CVS $r2)"
                execCvsUpdate $diff($top,RCSFile) $diff($top,leftFile) -r $r1
                execCvsUpdate $diff($top,RCSFile) $diff($top,rightFile) -r $r2
            } else {
                set diff($top,leftLabel) "$diff($top,RCSFile) (RCS $r1)"
                set diff($top,rightLabel) "$diff($top,RCSFile) (RCS $r2)"
                catch {exec co -p$r1 [file nativename $diff($top,RCSFile)] \
                        > $diff($top,leftFile)}
                catch {exec co -p$r2 [file nativename $diff($top,RCSFile)] \
                        > $diff($top,rightFile)}
            }
        }
    }
    # Make sure labels are updated before processing starts
    update idletasks
}

# Clean up after a RCS/CVS diff.
proc cleanupRCS {top} {
    global diff Pref

    #cleartmp ;# FIXA
    set diff($top,rightFile) $diff($top,RCSFile)
    set diff($top,leftFile) $diff($top,RCSFile)
}

# Prepare for a diff by creating needed temporary files
proc prepareFiles {top} {
    if {$::diff($top,mode) == "RCS" || $::diff($top,mode) == "CVS"} {
        prepareRCS $top
    } elseif {[string match "conflict*" $::diff($top,mode)]} {
        prepareConflict $top
    }
}

# Clean up after a diff
proc cleanupFiles {top} {
    if {$::diff($top,mode) == "RCS" || $::diff($top,mode) == "CVS"} {
        cleanupRCS $top
    } elseif {[string match "conflict*" $::diff($top,mode)]} {
        cleanupConflict $top
    }
}

# Main diff function.
proc doDiff {top} {
    global diff Pref
    global doingLine1 doingLine2
    global mapMax changesList

    if {$diff($top,mode) == "" && ($diff($top,leftOK) == 0 || $diff($top,rightOK) == 0)} {
        disableRedo $top
        return
    } else {
        enableRedo $top
    }

    busyCursor $top

    # Clear up everything before starting processing
    foreach item {wLine1 wDiff1 wLine2 wDiff2} {
        set w $::diff($top,$item)
        $w configure -state normal
        $w delete 1.0 end
    }
    set changesList {}
    set mapMax 0
    set ::HighLightCount 0
    highLightChange $top -1
    drawMap $top -1
    # Display a star during diff execution, to know when the internal
    # processing starts, and when the label is "valid".
    set ::diff($top,eqLabel) "*"

    update idletasks

    if {$diff($top,mode) == "patch"} {
        displayPatch $top
        drawMap $top -1
        foreach item {wLine1 wLine2} {
            set w $::diff($top,$item)
            $w configure -state disabled
        }
        update idletasks
        $::diff($top,wLine2) see 1.0
        normalCursor $top
        return
    } else {
        prepareFiles $top
    }

    # Run diff and parse the result.
    if {$::diff(diffutil)} {
        set differr [catch {eval DiffUtil::diffFiles $Pref(ignore) \
                \$diff($top,leftFile) \$diff($top,rightFile)} diffres]
    } else {
        set differr [catch {eval exec \$::diff(diffexe) \
                $diff($top,dopt) $Pref(ignore) \
                \$diff($top,leftFile) \$diff($top,rightFile)} diffres]
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
    if {$diff($top,mode) == "conflictPure"} {
        set result $diff($top,conflictDiff)
    }

    if {[llength $result] == 0} {
        if {$differr == 1} {
            $::diff($top,wDiff1) insert end $diffres
            normalCursor $top
            return
        } else {
            set ::diff($top,eqLabel) "="
        }
    } else {
        set ::diff($top,eqLabel) " "
    }
    # Update the equal label immediately for better feedback
    update idletasks

    set ch1 [open $diff($top,leftFile)]
    set ch2 [open $diff($top,rightFile)]
    if {$::tcl_platform(platform) == "windows" && $Pref(crlf)} {
        fconfigure $ch1 -translation crlf
        fconfigure $ch2 -translation crlf
    }
    set doingLine1 1
    set doingLine2 1
    set t 0
    foreach i $result {
        if {![regexp {(.*)([acd])(.*)} $i -> l c r]} {
            $::diff($top,wDiff1) insert 1.0 "No regexp match for $i\n"
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
                    dotext $top $ch1 $ch2 0 $n2 [expr {$line1 + 1}] $line2
                }
                c {
                    dotext $top $ch1 $ch2 $n1 $n2 $line1 $line2
                }
                d {
                    # lucka i right, new i left
                    dotext $top $ch1 $ch2 $n1 0 $line1 [expr {$line2 + 1}]
                }
            }
        }
        if {$::diff($top,limitlines) && $::mapMax > $::diff($top,limitlines)} {
            break
        }
        if {[incr t] >= 10} {
	    update idletasks
	    $::diff($top,wLine2) see end
	    update idletasks
            set t 0
        }
        bindHighlight $top
        incr ::HighLightCount
    }

    dotext $top $ch1 $ch2 0 0 0 0

    # Make sure all text widgets have the same number of lines.
    # The common y scroll doesn't work well if not.
    set max 0.0
    foreach item {wLine1 wDiff1 wLine2 wDiff2} {
        set w $::diff($top,$item)
        if {[$w index end] > $max} {
            set max [$w index end]
        }
    }
    foreach item {wLine1 wDiff1 wLine2 wDiff2} {
        set w $::diff($top,$item)
        set d [expr {int($max) - int([$w index end])}]
        for {set t 0} {$t < $d} {incr t} {
            $w insert end \n
        }
    }

    close $ch1
    close $ch2

    drawMap $top -1
    foreach item {wLine1 wLine2} {
        set w $::diff($top,$item)
        $w configure -state disabled
    }
    update idletasks
    $::diff($top,wLine2) see 1.0
    normalCursor $top
    showDiff $top 0

    cleanupFiles $top
    if {[string match "conflict*" $diff($top,mode)]} {
        if {$::diff($top,eqLabel) != "="} {
            makeMergeWin $top
        }
    }
    if {$diff($top,printFile) != ""} {
        after idle "doPrint $top 1 ; cleanupAndExit all"
    }
}

# This is the entrypoint to do a diff via DDE or Send
proc remoteDiff {file1 file2} {
    newDiff $file1 $file2
}

#####################################
# Highlight and navigation stuff
#####################################

# Scroll windows to next/previous diff
proc findDiff {top delta} {
    # FIXA: currenthighlight per top
    global CurrentHighLight

    showDiff $top [expr {$CurrentHighLight + $delta}]
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
proc highLightChange {top n} {
    global CurrentHighLight changesList
    if {[info exists CurrentHighLight] && $CurrentHighLight >= 0} {
        $::diff($top,wLine1) tag configure hl$CurrentHighLight -background {}
        $::diff($top,wLine2) tag configure hl$CurrentHighLight -background {}
    }
    set CurrentHighLight $n
    if {$CurrentHighLight < 0} {
        set CurrentHighLight -1
    } elseif {$CurrentHighLight * 7 >= [llength $changesList]} {
        set CurrentHighLight [expr {[llength $changesList] / 7}]
    } else {
        $::diff($top,wLine1) tag configure hl$CurrentHighLight \
                -background yellow
        $::diff($top,wLine2) tag configure hl$CurrentHighLight \
                -background yellow
    }
}

# Highlight a diff and scroll windows to it.
proc showDiff {top num} {
    global CurrentHighLight changesList

    highLightChange $top $num

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

    foreach item {wLine1 wDiff1 wLine2 wDiff2} {
        set w $::diff($top,$item)
        seeText $w $line1 $line2
    }
}

#####################################
# File dialog stuff
#####################################

proc doOpenLeft {top {forget 0}} {
    global diff

    if {!$forget && [info exists diff($top,leftDir)]} {
        set initDir $diff($top,leftDir)
    } elseif {[info exists diff($top,rightDir)]} {
        set initDir $diff($top,rightDir)
    } else {
        set initDir [pwd]
    }

    set apa [tk_getOpenFile -title "Select left file" -initialdir $initDir \
            -parent $top]
    if {$apa != ""} {
        set diff($top,leftDir) [file dirname $apa]
        set diff($top,leftFile) $apa
        set diff($top,leftLabel) $apa
        set diff($top,leftOK) 1
        return 1
    }
    return 0
}

proc doOpenRight {top {forget 0}} {
    global diff
    if {!$forget && [info exists diff($top,rightDir)]} {
        set initDir $diff($top,rightDir)
    } elseif {[info exists diff($top,leftDir)]} {
        set initDir $diff($top,leftDir)
    } else {
        set initDir [pwd]
    }

    set apa [tk_getOpenFile -title "Select right file" -initialdir $initDir \
            -parent $top]
    if {$apa != ""} {
        set diff($top,rightDir) [file dirname $apa]
        set diff($top,rightFile) $apa
        set diff($top,rightLabel) $apa
        set diff($top,rightOK) 1
        return 1
    }
    return 0
}

proc openLeft {top} {
    global diff
    if {[doOpenLeft $top]} {
        set diff($top,mode) ""
        doDiff $top
    }
}

proc openRight {top} {
    global diff
    if {[doOpenRight $top]} {
        set diff($top,mode) ""
        doDiff $top
    }
}

proc openConflict {top} {
    global diff Pref
    if {[doOpenRight $top]} {
        set diff($top,mode) "conflict"
        set Pref(ignore) " "
        set diff($top,conflictFile) $diff($top,rightFile)
        set diff($top,mergeFile) ""
        doDiff $top
    }
}

proc openPatch {top} {
    global diff Pref
    if {[doOpenLeft $top]} {
        set diff($top,mode) "patch"
        set Pref(ignore) " "
        set diff($top,patchFile) $diff($top,leftFile)
        doDiff $top
    }
}

proc openRCS {top} {
    global diff
    if {[doOpenRight $top]} {
        set diff($top,mode) "RCS"
        set diff($top,RCSFile) $diff($top,rightFile)
        set diff($top,leftLabel) "RCS"
        set diff($top,leftOK) 0
        doDiff $top
    }
}

proc openCVS {top} {
    global diff
    if {[doOpenRight $top]} {
        set diff($top,mode) "CVS"
        set diff($top,RCSFile) $diff($top,rightFile)
        set diff($top,leftLabel) "CVS"
        set diff($top,leftOK) 0
        doDiff $top
    }
}

proc openBoth {top forget} {
    global diff
    if {[doOpenLeft $top]} {
        if {[doOpenRight $top $forget]} {
            set diff($top,mode) ""
            doDiff $top
        }
    }
}

#####################################
# Map stuff
#####################################

proc drawMap {top newh} { # FIXA top
    global mapMax Pref changesList

    set oldh [map$top cget -height]
    if {$oldh == $newh} return

    map$top blank
    if {![info exists changesList] || [llength $changesList] == 0} return

    set w [winfo width $top.c]
    set h [winfo height $top.c]
    set x2 [expr {$w - 1}]
    map$top configure -width $w -height $h
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
        map$top put $Pref(color$type) -to 1 $y1 $x2 $y2
    }
}

######################################
# Merge stuff
#####################################

# Get all data from the files to merge
proc collectMergeData {top} {
    global diff
    global changesList mergeSelection
    global leftMergeData rightMergeData

    set leftMergeData {}
    set rightMergeData {}

    if {![info exists changesList]} {
        set changesList {}
    }

    if {$diff($top,mode) == "RCS" || $diff($top,mode) == "CVS"} {
        prepareRCS $top
    } elseif {[string match "conflict*" $diff($top,mode)]} {
        prepareConflict $top
    }

    set ch1 [open $diff($top,leftFile) r]
    set ch2 [open $diff($top,rightFile) r]
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

    if {$diff($top,mode) == "RCS" || $diff($top,mode) == "CVS"} {
        cleanupRCS $top
    } elseif {[string match "conflict*" $diff($top,mode)]} {
        cleanupConflict $top
    }
}

# Fill up the merge window with the initial version of merged files.
proc fillMergeWindow {top} {
    global mergeSelection leftMergeData rightMergeData curMergeSel curMerge

    set w $top.merge.t
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
    showDiff $top 0
    update
    seeText $w merges0 mergee0
}

# Move to and highlight another diff.
proc nextMerge {top delta} {
    global mergeSelection curMergeSel curMerge leftMergeData

    set w $top.merge.t
    $w tag configure merge$curMerge -foreground ""

    set curMerge [expr {$curMerge + $delta}]
    if {$curMerge < 0} {set curMerge 0}
    if {$curMerge >= ([llength $leftMergeData] / 2)} {
        set curMerge [expr {[llength $leftMergeData] / 2 - 1}]
    }
    set curMergeSel $mergeSelection($curMerge)
    $w tag configure merge$curMerge -foreground red
    showDiff $top $curMerge
    seeText $w merges$curMerge mergee$curMerge
}

# Select a merge setting for all diffs.
proc selectMergeAll {top new} {
    global leftMergeData curMerge curMergeSel
    set end [expr {[llength $leftMergeData] / 2}]
    for {set t 0} {$t < $end} {incr t} {
        selectMerge2 $top $t $new
    }
    set curMergeSel $new
    set w $top.merge.t
    seeText $w merges$curMerge mergee$curMerge
}

# Change merge setting fo current diff.
proc selectMerge {top} {
    global curMergeSel curMerge

    set w $top.merge.t
    selectMerge2 $top $curMerge $curMergeSel
    seeText $w merges$curMerge mergee$curMerge
}

# Change merge setting for a diff.
proc selectMerge2 {top no new} {
    global mergeSelection
    global leftMergeData rightMergeData

    set w $top.merge.t
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
proc saveMerge {top} {
    global diff

    set w $top.merge.t

    if {$diff($top,mergeFile) == ""} {
        if {[info exists diff($top,rightDir)]} {
            set initDir $diff($top,rightDir)
        } elseif {[info exists diff($top,leftDir)]} {
            set initDir $diff($top,leftDir)
        } else {
            set initDir [pwd]
        }

        set apa [tk_getSaveFile -title "Save merge file" -initialdir $initDir]
        if {$apa == ""} return
        set diff($top,mergeFile) $apa
    }

    set ch [open $diff($top,mergeFile) "w"]
    puts -nonewline $ch [$w get 1.0 end-1char]
    close $ch
    tk_messageBox -parent $top.merge -icon info -type ok -title "Diff" \
            -message "Saved merge to file $diff($top,mergeFile)."
}

# Close merge window and clean up.
proc closeMerge {top} {
    global mergeSelection leftMergeData rightMergeData

    destroy $top.merge
    set leftMergeData {}
    set rightMergeData {}
    array unset mergeSelection
}

# Create a window to display merge result.
proc makeMergeWin {top} {
    set w $top.merge
    if {![winfo exists $w]} {
        toplevel $w
    } else {
        eval destroy [winfo children $w]
    }

    wm title $w "Merge result"

    frame $w.f

    radiobutton $w.f.rb1 -text "LR" -value 12 -variable curMergeSel \
            -command "selectMerge $top"
    radiobutton $w.f.rb2 -text "L"  -value 1  -variable curMergeSel \
            -command "selectMerge $top"
    radiobutton $w.f.rb3 -text "R"  -value 2  -variable curMergeSel \
            -command "selectMerge $top"
    radiobutton $w.f.rb4 -text "RL" -value 21 -variable curMergeSel \
            -command "selectMerge $top"
    bind $w <Key-Left>  "focus $top.merge; set curMergeSel 1; selectMerge $top"
    bind $w <Key-Right> "focus $top.merge; set curMergeSel 2; selectMerge $top"

    button $w.f.bl -text "All L" -command "selectMergeAll $top 1"
    button $w.f.br -text "All R" -command "selectMergeAll $top 2"

    button $w.f.b1 -text "Prev" -command "nextMerge $top -1"
    button $w.f.b2 -text "Next" -command "nextMerge $top 1"
    bind $w <Key-Down> "focus $top.merge ; nextMerge $top 1"
    bind $w <Key-Up>   "focus $top.merge ; nextMerge $top -1"
    bind $w <Shift-Key-Down> "focus $top.merge ; nextMerge $top 10"
    bind $w <Shift-Key-Up>   "focus $top.merge ; nextMerge $top -10"

    button $w.f.bs -text "Save" -command "saveMerge $top"
    button $w.f.bq -text "Close" -command "closeMerge $top"
    wm protocol $w WM_CLOSE_WINDOW "closeMerge $top"

    grid $w.f.rb1 $w.f.rb2 $w.f.rb3 $w.f.rb4 x $w.f.b1 $w.f.b2 x \
            $w.f.bl $w.f.br x x x $w.f.bs $w.f.bq
    grid columnconfigure $w.f {4 7 10 12} -minsize 10
    grid columnconfigure $w.f 10 -weight 1

    if {[string match conflict* $::diff($top,mode)]} {
        checkbutton $w.f.bm -text "Pure" -variable diff($top,mode) \
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

    collectMergeData $top
    fillMergeWindow $top
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
proc printDiffs {top {quiet 0}} {
    busyCursor $top
    update idletasks
    set tmpFile [file nativename ~/tcldiff.enscript]
    if {$::diff($top,printFile) != ""} {
        set tmpFile2 [file nativename $::diff($top,printFile)]
    } else {
        set tmpFile2 [file nativename ~/tcldiff.ps]
    }

    set lines1 {}
    set lines2 {}

    set tdump1 [$::diff($top,wDiff1) dump -tag -text 1.0 end]
    set tdump2 [$::diff($top,wDiff2) dump -tag -text 1.0 end]
    set lineNo1 [processLineno $::diff($top,wLine1)]
    set lineNo2 [processLineno $::diff($top,wLine2)]

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
    if {![regexp {^(.*)( \(.*?\))$} $::diff($top,leftLabel) -> lfile lrest]} {
        set lfile $::diff($top,leftLabel)
        set lrest ""
    }
    set lfile [file tail $lfile]$lrest
    if {![regexp {^(.*)( \(.*?\))$} $::diff($top,rightLabel) -> rfile rrest]} {
        set rfile $::diff($top,rightLabel)
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

    normalCursor $top
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
        pack .dp.l -side "top"
    }
}

# Create a print dialog.
proc doPrint {top {quiet 0}} {
    if {![info exists ::grayLevel1]} {
        set ::grayLevel1 0.6
        set ::grayLevel2 0.8
    }
    if {$quiet} {
        printDiffs $top 1
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
            -command "destroy .pr; update; printDiffs $top"
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

# A little helper to make a scrolled window
# It returns the name of the scrolled window
proc Scroll {dir class w args} {
    switch -- $dir {
        both {
            set scrollx 1
            set scrolly 1
        }
        x {
            set scrollx 1
            set scrolly 0
        }
        y {
            set scrollx 0
            set scrolly 1
        }
        default {
            return -code error "Bad scrolldirection \"$dir\""
        }
    }

    frame $w
    eval [list $class $w.s] $args

    # Move border properties to frame
    set bw [$w.s cget -borderwidth]
    set relief [$w.s cget -relief]
    $w configure -relief $relief -borderwidth $bw
    $w.s configure -borderwidth 0

    grid $w.s -sticky news

    if {$scrollx} {
        $w.s configure -xscrollcommand [list $w.sbx set]
        scrollbar $w.sbx -orient horizontal -command [list $w.s xview]
        grid $w.sbx -row 1 -sticky we
    }
    if {$scrolly} {
        $w.s configure -yscrollcommand [list $w.sby set]
        scrollbar $w.sby -orient vertical -command [list $w.s yview]
        grid $w.sby -row 0 -column 1 -sticky ns
    }
    grid columnconfigure $w 0 -weight 1
    grid rowconfigure    $w 0 -weight 1

    return $w.s
}

proc formatAlignPattern {p} {
    set raw [binary format I $p]
    binary scan $raw B* bin
    set bin [string trimleft [string range $bin 0 end-8] 0][string range $bin end-7 end]
    set pat [string map {0 . 1 ,} $bin]
    return $pat
}

proc runAlign {top} {
    if {![info exists ::diff($top,aligns)] || [llength $::diff($top,aligns)] == 0} {
        return
    }

    set pattern 0
    foreach align $::diff($top,aligns) {
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

    prepareFiles $top
    foreach n {1 2} src {leftFile rightFile} {
        set tmp [tmpfile]
        set f($n) $tmp
        set cho [open $tmp w]
        #puts $cho hej
        set chi [open $::diff($top,$src) r]
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
    cleanupFiles $top

    newDiff $f(1) $f(2)

    set ::diff($top,aligns) ""
}

# Mark a line as aligned.
proc markAlign {top n line text} {
    set ::diff($top,align$n) $line
    set ::diff($top,aligntext$n) $text

    if {[info exists ::diff($top,align1)] && [info exists ::diff($top,align2)]} {
        set level 2
        if {![string equal $::diff($top,aligntext1) $::diff($top,aligntext2)]} {
            set apa [tk_messageBox -icon question -title "Align" -type yesno \
                    -message "Those lines are not equal.\nReally align them?"]
            if {$apa != "yes"} {
                return
            }
            set level 3
        }

        lappend ::diff($top,aligns) [list $::diff($top,align1) $::diff($top,align2) $level]

        unset ::diff($top,align1)
        unset ::diff($top,align2)
    }
}

# Called by popup menus over row numbers to add command for alignment.
# Returns 1 of nothing added.
proc alignMenu {m top n x y} {
    # Get the row that was clicked
    set w $::diff($top,wLine$n)
    set index [$w index @$x,$y]
    set row [lindex [split $index "."] 0]

    set data [$w get $row.0 $row.end]
    if {![regexp {\d+} $data line]} {
        return 1
    }
    set text [$::diff($top,wDiff$n) get $row.0 $row.end]

    set other [expr {$n == 1 ? 2 : 1}]
    if {![info exists ::diff($top,align$other)]} {
        set label "Mark line for alignment"
    } else {
        set label "Align with line $::diff($top,align$other) on other side"
    }

    .lpm add command -label $label -command [list markAlign $top $n $line $text]
    return 0
}


proc hlSelect {top hl} {
    highLightChange $top $hl
}

proc hlSeparate {top n hl} {
    set ::diff($top,separate$n) $hl
    set wd $::diff($top,wDiff$n)
    set wl $::diff($top,wLine$n)

    if {$hl == ""} {
        set range [$wd tag ranges sel]
    } else {
        set range [$wl tag ranges hl$::diff($top,separate$n)]
    }
    set text [eval $wd get $range]
    set ::diff($top,separatetext$n) $text

    if {[info exists ::diff($top,separate1)] && \
            [info exists ::diff($top,separate2)]} {
        set f1 [tmpfile]
        set f2 [tmpfile]
        set ch [open $f1 w]
        puts $ch $::diff($top,separatetext1)
        close $ch
        set ch [open $f2 w]
        puts $ch $::diff($top,separatetext2)
        close $ch

        newDiff $f1 $f2

        unset ::diff($top,separate1)
        unset ::diff($top,separate2)
    }
}

proc hlPopup {top n hl X Y x y} {
    if {[info exists ::diff($top,nopopup)] && $::diff($top,nopopup)} return
    destroy .lpm
    menu .lpm -tearoff 0

    if {$hl != ""} {
        .lpm add command -label "Select" \
                -command [list hlSelect $top $hl]
    }

    set other [expr {$n == 1 ? 2 : 1}]
    if {![info exists ::diff($top,separate$other)]} {
        set label "Mark for Separate Diff"
    } else {
        set label "Separate Diff"
    }

    .lpm add command -label $label -command [list hlSeparate $top $n $hl]
    alignMenu .lpm $top $n $x $y

    set ::diff($top,nopopup) 1
    tk_popup .lpm $X $Y
    after idle [list after 1 [list set ::diff($top,nopopup) 0]]

    return
}

proc rowPopup {w X Y x y} {
    set top [winfo toplevel $w]
    if {[info exists ::diff($top,nopopup)] && $::diff($top,nopopup)} return
    destroy .lpm
    menu .lpm -tearoff 0

    regexp {\d+} $w n
    if {[alignMenu .lpm $top $n $x $y]} {
        return
    }

    set ::diff($top,nopopup) 1
    tk_popup .lpm $X $Y
    after idle [list after 1 [list set ::diff($top,nopopup) 0]]
}

proc bindHighlight {top} {
    set tag hl$::HighLightCount
    foreach n {1 2} {
        $::diff($top,wLine$n) tag bind $tag <ButtonPress-3> \
                "hlPopup $top $n $::HighLightCount %X %Y %x %y ; break"
        $::diff($top,wLine$n) tag bind $tag <ButtonPress-1> \
                "hlSelect $top $::HighLightCount"
    }
}

proc zoomRow {w X Y x y} {
    global Pref
    set top [winfo toplevel $w]
    # Get the row that was clicked
    set index [$w index @$x,$y]
    set row [lindex [split $index "."] 0]

    # Check if it is selected
    if {[lsearch [$w tag names $index] sel] >= 0} {
        regexp {\d+} $w n
        hlPopup $top $n "" $X $Y $x $y
        return
    }

    # Extract the data
    set data(1) [$::diff($top,wDiff1) dump -tag -text $row.0 $row.end]
    set data(2) [$::diff($top,wDiff2) dump -tag -text $row.0 $row.end]
    if {[llength $data(1)] == 0 && [llength $data(2)] == 0} return

    set font [$::diff($top,wDiff1) cget -font]
    set wx $X
    set wy [expr {$Y + 4}]

    destroy $top.balloon
    toplevel $top.balloon -bg black
    wm withdraw $top.balloon
    wm overrideredirect $top.balloon 1

    set wid 0
    foreach x {1 2} {
        text $top.balloon.t$x -relief flat -font $font -bg #ffffaa -fg black \
                -padx 2 -pady 0 -height 1 -wrap word
        $top.balloon.t$x tag configure new1 -foreground $Pref(colornew1) \
                -background $Pref(bgnew1)
        $top.balloon.t$x tag configure change -foreground $Pref(colorchange) \
                -background $Pref(bgchange)
        $top.balloon.t$x tag configure new2 -foreground $Pref(colornew2) \
                -background $Pref(bgnew2)
        pack $top.balloon.t$x -side "top" -padx 1 -pady 1 -fill both -expand 1

        set tags {}
        foreach {key value index} $data($x) {
            if {$key == "tagon"} {
                lappend tags $value
                set tags [lsort -unique $tags]
            } elseif {$key == "tagoff"} {
                set i [lsearch $tags $value]
                if {$i >= 0} {
                    set tags [lreplace $tags $i $i]
                }
            } else {
                $top.balloon.t$x insert end $value $tags
            }
        }
        set text [$top.balloon.t$x get 1.0 1.end]
        regsub -all "\t" $text "        " text
        $top.balloon.t$x configure -width [string length $text]
    }

    # Let geometry requests propagate
    update idletasks

    # Is the balloon within the diff window?
    set wid [winfo reqwidth $top.balloon]
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
        $top.balloon.t1 configure -height $rows
        $top.balloon.t2 configure -height $rows
        # Let geometry requests propagate
        update idletasks
        wm geometry $top.balloon \
                [winfo screenwidth .]x[winfo reqheight $top.balloon]
        set wx 0
    }
    wm geometry $top.balloon +$wx+$wy
    wm deiconify $top.balloon
}

proc unzoomRow {w} {
    set top [winfo toplevel $w]
    destroy $top.balloon
}

# Procedures for common y-scroll
proc my_yview {top args} {
    foreach item {wLine1 wDiff1 wLine2 wDiff2} {
        set w $::diff($top,$item)
        eval $w yview $args
    }
}

proc my_yscroll {top args} {
    eval $top.sby set $args
    my_yview $top moveto [lindex $args 0]
}

# Reconfigure font
proc chFont {} {
    global Pref

    font configure myfont -size $Pref(fontsize) -family $Pref(fontfamily)
}

# Change color settings
proc applyColor {} {
    global Pref

    foreach top $::diff(diffWindows) {
        foreach item {wLine1 wDiff1 wLine2 wDiff2} {
            set w $::diff($top,$item)

            $w tag configure new1 -foreground $Pref(colornew1) \
                    -background $Pref(bgnew1)
            $w tag configure change -foreground $Pref(colorchange) \
                    -background $Pref(bgchange)
            $w tag configure new2 -foreground $Pref(colornew2) \
                    -background $Pref(bgnew2)
        }
    }
}

# Scroll text windows
proc scroll {top n what} {
    # Do not scroll if focus is in a text window.
    # This is for scroll bindings in the toplevel.
    if {[winfo class [focus]] != "Text"} {
        $::diff($top,wDiff1) yview scroll $n $what
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
	uplevel \#0 "trace variable $var w \
		{after idle {$w xview end} ;#}"
    }
}

# Fill in default data for a diff window
proc initDiffData {top} {
    global diff
    set diff($top,leftOK) 0
    set diff($top,rightOK) 0
    set diff($top,mode) ""
    set diff($top,printFile) ""
    set diff($top,mergeFile) ""
    set diff($top,conflictFile) ""
    set diff($top,limitlines) 0
}

# Create a new diff window and diff two files
proc newDiff {file1 file2} {
    global diff

    makeDiffWin

    set top [lindex $::diff(diffWindows) end]

    set diff($top,leftDir) [file dirname $file1]
    set diff($top,leftFile) $file1
    set diff($top,leftLabel) $file1
    set diff($top,leftOK) 1
    set diff($top,rightDir) [file dirname $file2]
    set diff($top,rightFile) $file2
    set diff($top,rightLabel) $file2
    set diff($top,rightOK) 1
    set diff($top,mode) ""
    wm deiconify $top
    raise $top
    update
    doDiff $top
}

# Build the main window
proc makeDiffWin {{top {}}} {
    global Pref tcl_platform debug
    
    if {$top != "" && [winfo exists $top] && [winfo toplevel $top] == $top} {
        # Reuse the old window
        eval destroy [winfo children $top]
    } else {
        # Locate a free toplevel name
        if {[info exists ::diff(topDiffCnt)]} {
            set t $::diff(topDiffCnt)
        } else {
            set t 0
        }
        while {[winfo exists .diff$t]} {
            incr t
        }
        set top .diff$t
        toplevel $top
        lappend ::diff(diffWindows) $top
    }

    wm title $top "Diff"
    wm protocol $top WM_DELETE_WINDOW [list cleanupAndExit $top]

    frame $top.f
    grid $top.f -row 0 -columnspan 4 -sticky news

    menubutton $top.mf -text File -underline 0 -menu $top.mf.m
    menu $top.mf.m
    if {$debug == 1} {
        $top.mf.m add command -label "Redo Diff" -underline 5 \
                -command [list doDiff $top]
    } else {
        $top.mf.m add command -label "Redo Diff" -underline 5 \
                -command [list doDiff $top] \
                -state disabled
    }
    $top.mf.m add separator
    $top.mf.m add command -label "Open Both..." -underline 0 \
            -command [list openBoth $top 0]
    $top.mf.m add command -label "Open Both (forget)..." \
            -command [list openBoth $top 1]
    $top.mf.m add command -label "Open Left File..." \
            -command [list openLeft $top]
    $top.mf.m add command -label "Open Right File..." \
            -command [list openRight $top]
    $top.mf.m add command -label "Open Conflict File..." \
            -command [list openConflict $top]
    $top.mf.m add command -label "Open Patch File..." \
            -command [list openPatch $top]
    if {$tcl_platform(platform) == "unix"} {
        $top.mf.m add command -label "RCSDiff..." -underline 0 \
                -command [list openRCS $top]
    }
    if {$::diff(cvsExists)} {
        $top.mf.m add command -label "CVSDiff..." -underline 1 \
                -command [list openCVS $top]
    }
    $top.mf.m add separator
    $top.mf.m add command -label "Print..." -underline 0 \
            -command [list doPrint $top]
    $top.mf.m add separator
    $top.mf.m add command -label "New Diff Window" -underline 0 \
            -command makeDiffWin
    $top.mf.m add command -label "Close" -underline 0 \
            -command [list cleanupAndExit $top]
    $top.mf.m add separator
    $top.mf.m add command -label "Quit" -underline 0 \
            -command {cleanupAndExit all}

    menubutton $top.mo -text "Options" -underline 0 -menu $top.mo.m
    menu $top.mo.m
    $top.mo.m add cascade -label "Font" -underline 0 -menu $top.mo.mf
    $top.mo.m add cascade -label "Ignore" -underline 0 -menu $top.mo.mi
    $top.mo.m add cascade -label "Parse" -underline 0 -menu $top.mo.mp
    $top.mo.m add command -label "Colours..." -underline 0 -command makePrefWin
    $top.mo.m add checkbutton -label "Diffs only" -variable Pref(onlydiffs)
    if {$tcl_platform(platform) == "windows"} {
        $top.mo.m add checkbutton -label "Force crlf translation" \
                -variable Pref(crlf)
    }
    $top.mo.m add separator
    $top.mo.m add command -label "Save default" -command saveOptions

    menu $top.mo.mf
    $top.mo.mf add command -label "Select..." -command makeFontWin
    $top.mo.mf add radiobutton -label 6 -variable Pref(fontsize) -value 6 \
            -command chFont
    $top.mo.mf add radiobutton -label 7 -variable Pref(fontsize) -value 7 \
            -command chFont
    $top.mo.mf add radiobutton -label 8 -variable Pref(fontsize) -value 8 \
            -command chFont
    $top.mo.mf add radiobutton -label 9 -variable Pref(fontsize) -value 9 \
            -command chFont
    $top.mo.mf add radiobutton -label 10 -variable Pref(fontsize) -value 10 \
            -command chFont

    menu $top.mo.mi
    $top.mo.mi add radiobutton -label "Nothing" -variable Pref(ignore) -value " "
    $top.mo.mi add radiobutton -label "Space changes (-b)" -variable Pref(ignore) \
            -value "-b"
    $top.mo.mi add radiobutton -label "All spaces (-w)" -variable Pref(ignore) \
            -value "-w"

    menu $top.mo.mp
    $top.mo.mp add radiobutton -label "Nothing" -variable Pref(parse) -value 0
    $top.mo.mp add radiobutton -label "Lines" -variable Pref(parse) -value 1
    $top.mo.mp add radiobutton -label "Blocks (small)" -variable Pref(parse) \
            -value 2
    $top.mo.mp add radiobutton -label "Blocks" -variable Pref(parse) -value 3
    $top.mo.mp add separator
    $top.mo.mp add radiobutton -label "Characters" -variable Pref(lineparsewords) \
            -value "0"
    $top.mo.mp add radiobutton -label "Words" -variable Pref(lineparsewords) \
            -value "1"
    $top.mo.mp add separator
    $top.mo.mp add checkbutton -label "Use 2nd stage" \
            -variable Pref(extralineparse)
    $top.mo.mp add checkbutton -label "Mark last" -variable Pref(marklast)

    menubutton $top.ms -text Search -underline 0 -menu $top.ms.m
    menu $top.ms.m
    if {[info procs textSearch::searchMenu] != ""} {
        textSearch::searchMenu $top.ms.m
    } else {
        $top.ms.m add command -label "Text search not available" \
                -state disabled
    }

    menubutton $top.mh -text "Help" -underline 0 -menu $top.mh.m
    menu $top.mh.m
    $top.mh.m add command -label "Help" -command makeHelpWin -underline 0
    $top.mh.m add command -label "About" -command makeAboutWin -underline 0

    label $top.lo -text "Diff Options"
    addBalloon $top.lo "Options passed to the external diff.\nNote\
            that options for ignoring whitespace are available in\
            the Options menu."
    entry $top.eo -width 6 -textvariable diff($top,dopt)
    label $top.lr1 -text "Rev 1"
    addBalloon $top.lr1 "Revision number for CVS/RCS diff."
    entry $top.er1 -width 6 -textvariable diff($top,doptrev1)
    label $top.lr2 -text "Rev 2"
    addBalloon $top.lr2 "Revision number for CVS/RCS diff."
    entry $top.er2 -width 6 -textvariable diff($top,doptrev2)
    button $top.bfp -text "Prev Diff" -relief raised \
            -command [list findDiff $top -1] \
            -underline 0 -padx 15
    button $top.bfn -text "Next Diff" -relief raised \
            -command [list findDiff $top 1] \
            -underline 0 -padx 15
    bind $top <Alt-n> [list findDiff $top 1]
    bind $top <Alt-p> [list findDiff $top -1]

    catch {font delete myfont}
    font create myfont -family $Pref(fontfamily) -size $Pref(fontsize)

    fileLabel $top.l1 -textvariable diff($top,leftLabel)
    fileLabel $top.l2 -textvariable diff($top,rightLabel)

    frame $top.ft1 -borderwidth 2 -relief sunken
    text $top.ft1.tl -height 40 -width 5 -wrap none \
            -yscrollcommand [list my_yscroll $top] \
            -font myfont -borderwidth 0 -padx 0 -highlightthickness 0 \
            -takefocus 0
    text $top.ft1.tt -height 40 -width 80 -wrap none \
            -yscrollcommand [list my_yscroll $top] \
            -xscrollcommand [list $top.sbx1 set] \
            -font myfont -borderwidth 0 -padx 1 \
            -highlightthickness 0
    frame $top.ft1.f -width 2 -height 2 -bg lightgray
    pack $top.ft1.tl -side left -fill y
    pack $top.ft1.f -side left -fill y
    pack $top.ft1.tt -side right -fill both -expand 1
    scrollbar $top.sby -orient vertical -command [list my_yview $top]
    scrollbar $top.sbx1 -orient horizontal -command [list $top.ft1.tt xview]
    set ::diff($top,wLine1) $top.ft1.tl
    set ::diff($top,wDiff1) $top.ft1.tt

    frame $top.ft2 -borderwidth 2 -relief sunken
    text $top.ft2.tl -height 60 -width 5 -wrap none \
            -yscrollcommand [list my_yscroll $top] \
            -font myfont -borderwidth 0 -padx 0 -highlightthickness 0 \
            -takefocus 0
    text $top.ft2.tt -height 60 -width 80 -wrap none \
            -yscrollcommand [list my_yscroll $top] \
            -xscrollcommand [list $top.sbx2 set] \
            -font myfont -borderwidth 0 -padx 1 \
            -highlightthickness 0
    frame $top.ft2.f -width 2 -height 2 -bg lightgray
    pack $top.ft2.tl -side left -fill y
    pack $top.ft2.f -side left -fill y
    pack $top.ft2.tt -side right -fill both -expand 1
    scrollbar $top.sbx2 -orient horizontal -command [list $top.ft2.tt xview]
    set ::diff($top,wLine2) $top.ft2.tl
    set ::diff($top,wDiff2) $top.ft2.tt

    # Set up a tag for incremental search bindings
    if {[info procs textSearch::enableSearch] != ""} {
        textSearch::enableSearch $top.ft1.tt -label ::diff($top,isearchLabel)
        textSearch::enableSearch $top.ft2.tt -label ::diff($top,isearchLabel)
    }

    label $top.le -textvariable ::diff($top,eqLabel) -width 1
    addBalloon $top.le "* means external diff is running.\n= means files do\
            not differ.\nBlank means files differ."
    label $top.ls -width 1 -pady 0 -padx 0 \
            -textvariable ::diff($top,isearchLabel)
    addBalloon $top.ls "Incremental search indicator"
    canvas $top.c -width 6 -bd 0 -selectborderwidth 0 -highlightthickness 0


    applyColor
    $top.ft1.tt tag configure last -underline 1
    $top.ft2.tt tag configure last -underline 1
    foreach w [list $top.ft1.tt $top.ft2.tt] {
        $w tag raise sel
        bind $w <ButtonPress-3> "zoomRow %W %X %Y %x %y"
        bind $w <ButtonRelease-3> "unzoomRow %W"
    }
    foreach w [list $top.ft1.tl $top.ft2.tl] {
        bind $w <ButtonPress-3> "rowPopup %W %X %Y %x %y"
    }

    grid $top.l1   $top.le -        $top.l2   -row 1 -sticky news
    grid $top.ft1  $top.c  $top.sby $top.ft2  -row 2 -sticky news
    grid $top.sbx1 $top.ls -        $top.sbx2 -row 3 -sticky news
    grid columnconfigure $top {0 3} -weight 1
    grid rowconfigure $top 2 -weight 1
    grid $top.c -pady [expr {[$top.sby cget -width] + 2}]
    grid $top.ls -sticky ""
    
    image create photo map$top
    $top.c create image 0 0 -anchor nw -image map$top
    bind $top.c <Configure> [list drawMap $top %h]

    bind $top <Key-Up>    [list scroll $top -1 u]
    bind $top <Key-Down>  [list scroll $top  1 u]
    bind $top <Key-Prior> [list scroll $top -1 p]
    bind $top <Key-Next>  [list scroll $top  1 p]
    bind $top <Key-Escape> [list focus $top]

    pack $top.mf $top.mo $top.ms $top.mh -in $top.f -side left
    pack $top.bfn -in $top.f -side right -padx {3 6}
    pack $top.bfp $top.er2 $top.lr2 $top.er1 $top.lr1 $top.eo $top.lo \
            -in $top.f -side right -padx 3
    if {$debug == 1} {
        menubutton $top.md -text Debug -menu $top.md.m
        menu $top.md.m
        if {$tcl_platform(platform) == "windows"} {
            $top.md.m add checkbutton -label Console -variable consolestate \
                    -onvalue show -offvalue hide \
                    -command {console $consolestate}
            $top.md.m add separator
        }
        $top.md.m add radiobutton -label "Context 2" \
                -variable ::Pref(context) -value 2
        $top.md.m add radiobutton -label "Context 5" \
                -variable ::Pref(context) -value 5
        $top.md.m add radiobutton -label "Context 10" \
                -variable ::Pref(context) -value 10
        $top.md.m add radiobutton -label "Context 20" \
                -variable ::Pref(context) -value 20
        $top.md.m add separator
        $top.md.m add checkbutton -label Wrap -variable wrapstate \
                -onvalue char -offvalue none -command \
                "$top.ft1.tt configure -wrap \$wrapstate ;\
                $top.ft2.tt configure -wrap \$wrapstate"
        $top.md.m add command -label "Merge" -command [list makeMergeWin $top]
        $top.md.m add command -label "Date Filter" \
                -command {set ::diff(filter) {^Date}}
        $top.md.m add command -label "Align" -command [list runAlign $top]
        $top.md.m add separator
        $top.md.m add command -label "Reread Source" \
                -command {source $thisscript}
        $top.md.m add separator
        $top.md.m add command -label "Redraw Window" \
                -command [list makeDiffWin $top]
        $top.md.m add separator
        $top.md.m add command -label "Normal Cursor" \
                -command [list normalCursor $top]
        $top.md.m add separator
        $top.md.m add command -label "Evalstats" -command {evalstats}
        $top.md.m add command -label "_stats" -command {parray _stats}
        $top.md.m add command -label "Nuisance" -command {makeNuisance \
                "It looks like you are trying out the debug menu."}
        pack $top.md -in $top.f -side left -padx 20
    }

    initDiffData $top
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

# Create a window for changing preferences.
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

    button .pr.fc.b1 -text Sel -command "selColor colorchange"
    button .pr.fc.b2 -text Sel -command "selColor colornew1"
    button .pr.fc.b3 -text Sel -command "selColor colornew2"

    entry .pr.fc.e4 -textvariable "TmpPref(bgchange)" -width 10
    entry .pr.fc.e5 -textvariable "TmpPref(bgnew1)" -width 10
    entry .pr.fc.e6 -textvariable "TmpPref(bgnew2)" -width 10

    button .pr.fc.b4 -text Sel -command "selColor bgchange"
    button .pr.fc.b5 -text Sel -command "selColor bgnew1"
    button .pr.fc.b6 -text Sel -command "selColor bgnew2"

    text .pr.fc.t1 -width 12 -height 1 -font myfont -takefocus 0
    text .pr.fc.t2 -width 12 -height 1 -font myfont -takefocus 0
    text .pr.fc.t3 -width 12 -height 1 -font myfont -takefocus 0
    .pr.fc.t1 tag configure change -foreground $TmpPref(colorchange) \
            -background $TmpPref(bgchange)
    .pr.fc.t2 tag configure new1 -foreground $TmpPref(colornew1) \
            -background $TmpPref(bgnew1)
    .pr.fc.t3 tag configure new2 -foreground $TmpPref(colornew2) \
            -background $TmpPref(bgnew2)
    .pr.fc.t1 insert end "Changed text" change
    .pr.fc.t2 insert end "Deleted text" new1
    .pr.fc.t3 insert end "Added text" new2

    .pr.fc.t1 configure -state disabled
    .pr.fc.t2 configure -state disabled
    .pr.fc.t3 configure -state disabled

    button .pr.b1 -text "Apply" -command applyPref
    button .pr.b2 -text "Test"  -command testColor
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
    pack .pr.b1 .pr.b2 .pr.b3 -side left -expand 1 -fill x -anchor s \
            -padx 2 -pady 2
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
proc exampleFont {lb} {
    global TmpPref
    set i [lindex [$lb curselection] 0]
    if {$i == ""} return
    set TmpPref(fontfamily) [$lb get $i]

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
    update

    catch {font delete tmpfont}
    font create tmpfont

    array set TmpPref [array get Pref]
    label .fo.lf -text Family -anchor w
    set lb [Scroll y listbox .fo.lb -width 15 -height 10 \
            -exportselection no -selectmode single]
    bind $lb <<ListboxSelect>> [list exampleFont $lb]

    label .fo.ls -text Size -anchor w
    button .fo.bm -text - -padx 0 -pady 0 -highlightthickness 0 \
            -command "incr TmpPref(fontsize) -1 ; exampleFont $lb"
    button .fo.bp -text + -padx 0 -pady 0 -highlightthickness 0 \
            -command "incr TmpPref(fontsize) ; exampleFont $lb"
    entry .fo.es -textvariable TmpPref(fontsize) -width 3
    bind .fo.es <KeyPress> [list after idle [list exampleFont $lb]]
    label .fo.le -text Example -anchor w -font tmpfont -width 1
    button .fo.bo -text Ok -command "applyFont; destroy .fo"
    button .fo.ba -text Apply -command "applyFont"
    button .fo.bc -text Close -command "destroy .fo"

    if {![info exists FontCache]} {
        set fam [lsort -dictionary [font families]]
        font create testfont
        foreach f $fam {
            if {![string equal $f ""]} {
                font configure testfont -family $f
                if {[font metrics testfont -fixed]} {
                    lappend FontCache $f
                }
            }
        }
        font delete testfont
    }
    foreach f $FontCache {
        $lb insert end $f
        if {[string equal -nocase $f $Pref(fontfamily)]} {
            $lb selection set end
            $lb see end
        }
    }

    destroy .fo.ltmp

    grid .fo.lf .fo.ls -      - -sticky w
    grid .fo.lb .fo.es .fo.bm .fo.bp -sticky new
    grid x      .fo.le -      - -sticky we -padx 2 -pady 2
    grid x      .fo.bo -      - -sticky we -padx 2 -pady 2
    grid x      .fo.ba -      - -sticky we -padx 2 -pady 2
    grid x      .fo.bc -      - -sticky we -padx 2 -pady 2
    grid .fo.lb -sticky news -rowspan 5
    grid columnconfigure .fo 0 -weight 1
    grid rowconfigure .fo 1 -weight 1

    exampleFont $lb
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

proc helpWin {w title} {
    destroy $w

    toplevel $w
    wm title $w $title
    bind $w <Key-Return> "destroy $w"
    bind $w <Key-Escape> "destroy $w"
    frame $w.f
    button $w.b -text "Close" -command "destroy $w" -width 10 \
            -default active
    pack $w.b -side bottom -pady 3
    pack $w.f -side top -expand y -fill both
    focus $w
    return $w.f
}

proc makeAboutWin {} {
    global diffver

    set w [helpWin .ab "About Diff"]

    text $w.t -width 45 -height 11 -wrap none -relief flat \
            -bg [$w cget -bg]
    pack $w.t -side top -expand y -fill both

    $w.t insert end "A Tcl/Tk frontend to diff\n\n"
    $w.t insert end "$diffver\n"
    $w.t insert end "Made by Peter Spjuth\n"
    $w.t insert end "E-Mail: peter.spjuth@space.se\n\n"
    $w.t insert end "Credits:\n"
    $w.t insert end "Ideas for scrollbar map and merge function\n"
    $w.t insert end "taken from TkDiff\n"

    set last [lindex [split [$w.t index end] "."] 0]
    $w.t configure -height $last
    $w.t configure -state disabled
}

# Insert a text file into a text widget.
# Any XML-style tags in the file are used as tags in the text window.
proc insertTaggedText {w file} {
    set ch [open $file r]
    set data [read $ch]
    close $ch

    set tags {}
    while {$data != ""} {
        if {[regexp {^([^<]*)<(/?)([^>]+)>(.*)$} $data -> pre sl tag post]} {
            $w insert end $pre $tags
            set i [lsearch $tags $tag]
            if {$sl != ""} {
                # Remove tag
                if {$i >= 0} {
                    set tags [lreplace $tags $i $i]
                }
            } else {
                # Add tag
                lappend tags $tag
            }
            set data $post
        } else {
            $w insert end $data $tags
            set data ""
        }
    }
}

proc makeHelpWin {} {
    global Pref

    set doc [file join $::thisdir doc/diff.txt]
    if {![file exists $doc]} return

    set w [helpWin .he "Diff Help"]

    text $w.t -width 82 -height 35 -wrap word -yscrollcommand "$w.sb set"\
            -font "Courier 10"
    scrollbar $w.sb -orient vert -command "$w.t yview"
    pack $w.sb -side right -fill y
    pack $w.t -side left -expand 1 -fill both

    # Move border properties to frame
    set bw [$w.t cget -borderwidth]
    set relief [$w.t cget -relief]
    $w configure -relief $relief -borderwidth $bw
    $w.t configure -borderwidth 0

    # Set up tags
    $w.t tag configure new1 -foreground $Pref(colornew1) \
            -background $Pref(bgnew1)
    $w.t tag configure new2 -foreground $Pref(colornew2) \
            -background $Pref(bgnew2)
    $w.t tag configure change -foreground $Pref(colorchange) \
            -background $Pref(bgchange)
    $w.t tag configure ul -underline 1

    insertTaggedText $w.t $doc
    $w.t configure -state disabled
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

  -limit <lines> : Do not process more than <lines> lines.}
}

proc parseCommandLine {} {
    global diff Pref
    global argv argc tcl_platform

    if {$argc == 0} return
    
    set top [lindex $::diff(diffWindows) end]

    set noautodiff 0
    set autobrowse 0
    set files ""
    set nextArg ""
    set revNo 1
    foreach arg $argv {
        if {$nextArg != ""} {
            if {$nextArg == "mergeFile"} {
                set diff($top,mergeFile) [file join [pwd] $arg]
            } elseif {$nextArg == "printFile"} {
                set diff($top,printFile) [file join [pwd] $arg]
            } elseif {$nextArg == "revision"} {
                set diff($top,doptrev$revNo) $arg
                incr revNo
            } elseif {$nextArg == "limitlines"} {
                set diff($top,limitlines) $arg
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
            set diff($top,mode) "conflict"
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
            set diff($top,doptrev$revNo) [string range $arg 2 end]
            incr revNo
        } elseif {[string range $arg 0 0] == "-"} {
            append diff($top,dopt) " $arg"
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
        if {$diff($top,mode) == "conflict"} {
            set diff($top,conflictFile) $fullname
            set diff($top,rightDir) $fulldir
            set diff($top,rightOK) 1
            set diff($top,rightLabel) $fullname
            set diff($top,leftLabel) $fullname
            after idle [list doDiff $top]
            return
        } elseif {!$autobrowse && \
                [llength [glob -nocomplain [file join $fulldir RCS]]]} {
            set diff($top,mode) "RCS"
            set diff($top,rightDir) $fulldir
            set diff($top,RCSFile) $fullname
            set diff($top,rightLabel) $fullname
            set diff($top,rightFile) $fullname
            set diff($top,rightOK) 1
            set diff($top,leftLabel) "RCS"
            if {$noautodiff} {
                enableRedo $top
            } else {
                after idle [list doDiff $top]
            }
        } elseif {!$autobrowse && \
                [llength [glob -nocomplain [file join $fulldir CVS]]]} {
            set diff($top,mode) "CVS"
            set diff($top,rightDir) $fulldir
            set diff($top,RCSFile) $fullname
            set diff($top,rightLabel) $fullname
            set diff($top,rightFile) $fullname
            set diff($top,rightOK) 1
            set diff($top,leftLabel) "CVS"
            if {$noautodiff} {
                enableRedo $top
            } else {
                after idle [list doDiff $top]
            }
        } else {
            set diff($top,leftDir) $fulldir
            set diff($top,leftFile) $fullname
            set diff($top,leftLabel) $fullname
            set diff($top,leftOK) 1
            if {[regexp {\.(diff|patch)$} $fullname]} {
                set diff($top,mode) "patch"
                set diff($top,patchFile) $fullname
                set autobrowse 0
                if {$noautodiff} {
                    enableRedo $top
                } else {
                    after idle [list doDiff $top]
                }
            }
        }
    } elseif {$len >= 2} {
        set fullname [file join [pwd] [lindex $files 0]]
        set fulldir [file dirname $fullname]
        set diff($top,leftDir) $fulldir
        set diff($top,leftFile) $fullname
        set diff($top,leftLabel) $fullname
        set diff($top,leftOK) 1
        set fullname [file join [pwd] [lindex $files 1]]
        set fulldir [file dirname $fullname]
        set diff($top,rightDir) $fulldir
        set diff($top,rightFile) $fullname
        set diff($top,rightLabel) $fullname
        set diff($top,rightOK) 1
        if {$noautodiff} {
            enableRedo $top
        } else {
            after idle [list doDiff $top]
        }
    }
    if {$autobrowse && (!$diff($top,leftOK) || !$diff($top,rightOK))} {
        if {!$diff($top,leftOK) && !$diff($top,rightOK)} {
            openBoth $top 0
        } elseif {!$diff($top,leftOK)} {
            openLeft $top
        } elseif {!$diff($top,rightOK)} {
            openRight $top
        }
        # If we cancel the second file and detect CVS, ask about it.
        if {$diff($top,leftOK) && !$diff($top,rightOK) && \
                [llength [glob -nocomplain [file join $fulldir CVS]]]} {

            if {[tk_messageBox -title Diff -icon question \
                    -message "Do CVS diff?" -type yesno] == "yes"} {
                set fulldir $diff($top,leftDir)
                set fullname $diff($top,leftFile)
                set diff($top,leftOK) 0
                set diff($top,mode) "CVS"
                set diff($top,rightDir) $fulldir
                set diff($top,RCSFile) $fullname
                set diff($top,rightLabel) $fullname
                set diff($top,rightFile) $fullname
                set diff($top,rightOK) 1
                set diff($top,leftLabel) "CVS"
                after idle [list doDiff $top]
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
        puts $ch [list set Pref($i) $Pref($i)]
    }
    close $ch
}

proc getOptions {} {
    global Pref

    set Pref(fontsize) 9
    set Pref(fontfamily) Courier
    set Pref(ignore) "-b"
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


if {![info exists gurkmeja]} {
    set gurkmeja 1
    option add *Menu.tearOff 0
    getOptions
    makeDiffWin
    wm withdraw .
    update idletasks
    parseCommandLine
}
