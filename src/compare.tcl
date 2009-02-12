#----------------------------------------------------------------------
#  Eskil, comparing
#
#  Copyright (c) 1998-2005, Peter Spjuth  (peter.spjuth@gmail.com)
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

proc maxAbs {a b} {
    return [expr {abs($a) > abs($b) ? $a : $b}]
}

# Compare two lines and rate how much they resemble each other.
# This has never worked well. Some day I'll sit down, think this through,
# and come up with a better algorithm.
proc CompareLines {line1 line2} {
    set opts $::Pref(ignore)
    if {$::Pref(nocase)} {lappend opts -nocase}
    set res [DiffUtil::diffStrings {*}$opts $line1 $line2]

    # Collect identical pieces and different pieces
    set sames {}
    set diffs1 {}
    set diffs2 {}
    foreach {same1 same2 diff1 diff2} $res {
        lappend sames $same1
        if {$diff1 != ""} {
            lappend diffs1 $diff1
        }
        if {$diff2 != ""} {
            lappend diffs2 $diff2
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
#    puts "S $sumsame  D $sumdiff1 D $sumdiff2"
    return [expr {$sumsame - [maxAbs $sumdiff1 $sumdiff2]}]
}

# Initialise a multidimensional list with some value
# This should use lrepeat once 8.5 is required
# The args are in the same order as indexes to lset/lindex
proc Linit {elem args} {
    for {set t [expr {[llength $args] - 1}]} {$t >= 0} {incr t -1} {
	set new {}
	for {set j [lindex $args $t]} {$j >= 1} {incr j -1} {
	    lappend new $elem
	}
	set elem $new
    }
    return $elem
}

# Decide how to display change blocks
# This tries to match the lines that resemble each other and put them
# next to each other.
# As CompareLines, this would need a complete rework and a
# better algorithm.
#
# Constraint: block1 may not be longer than block2
#
# Result is a list with one element per row in block1.
# The element is the index of the matching row in block2, and could be
# out of range.
proc CompareBlocks2 {block1 block2 scoresName} {
    upvar 1 $scoresName scores
    set size1 [llength $block1]
    set size2 [llength $block2]

    # A "constant", so I don't need to create it more than once
    set emptyResult [Linit {} $size1]

    # Collect statistics about each pair of lines.
    set scores [Linit {} $size1 $size2]
    # Store the best match for each item
    set scoresbest $emptyResult
    set origresult $emptyResult

    set j 0
    set bestsum 0
    foreach line1 $block1 {
        set bestscore -100000
        set bestline 0
        set i 0
        foreach line2 $block2 {
            set x [CompareLines $line1 $line2]
            lset scores $j $i $x
            #puts "Score $j $i : $x"
            if {$x > $bestscore} {
                set bestscore $x
                set bestline $i
            }
            incr i
        }
        #puts "Best for $j is $bestline : $bestscore"
        lset origresult $j $bestline
        lset scoresbest $j $bestscore
        incr bestsum $bestscore
        incr j
    }
    #puts "Bestsum: $bestsum"

    # origresult holds a mapping between blocks where each row
    # is paired with its best match. This may not be a possible
    # result since it has to be in order.

    #puts "Origresult: $origresult"

    # If the size is 1, it is automatically in order so we
    # don't need further processing.

    if {$size1 == 1} {
        return $origresult
    }

    # Start with a check if the theoretical best works, since often that
    # is the case.
    set order 1
    set result $origresult
    for {set i 0} {$i < ($size1 - 1)} {incr i} {
        if {[lindex $result $i] >= [lindex $result [expr {$i + 1}]]} {
            set order 0
            break
        }
    }
    if {$order} {
        #puts "ORDER"
        return $origresult
    }

    set bestresult $origresult
    set bestscoresum -100000

    # Look through the obvious "subblock" alternatives

    for {set startj 0} {$startj < ($size2 - $size1 + 1)} {incr startj} {
        set sum 0
        set result $emptyResult
        for {set i 0 ; set j $startj} {$i < $size1} {incr i ; incr j} {
            lset result $i $j
            incr sum [lindex $scores $i $j]
        }
        #puts "Subblock $startj sum: $sum"
        if {$sum > $bestscoresum} {
            #puts "New best: $sum ($bestscoresum)"
            set bestresult $result
            set bestscoresum $sum
        }
    }

    # If we reach 75% if the theoretical best, we take it
    while {$bestscoresum < (3 * $bestsum / 4)} {
        #puts "Outer: $scoresbest"
        # The outer loop restarts from the "best mapping"
        set result $origresult
        set mark [Linit 0 $size1]
        set high $mark

        # If result is in order, no problem.
        # Otherwise, try to adjust result to make it ordered

        while {1} {
            #puts "Inner: $scoresbest"
            # The inner loop tries to get the result in order
            set besti 0
            set bestscore -100000
            set order 1
            for {set i 0} {$i < $size1} {incr i} {
                if {[lindex $mark $i] == 0} {
                    for {set j [expr {$i + 1}]} {$j < $size1} {incr j} {
                        if {[lindex $mark $j] == 0} break
                    }
                    if {$j < $size1 && \
                            [lindex $result $i] >= [lindex $result $j]} {
                        set order 0
                    }
                    set x [lindex $scoresbest $i]
                    if {$x > $bestscore} {
                        set bestscore $x
                        set besti $i
                    }
                }
            }
            #puts "Best $besti order $order sc $bestscore"
            if {$order} break
            lset mark $besti 1
            set bestr [lindex $result $besti]
            for {set i 0} {$i < $besti} {incr i} {
                if {[lindex $mark $i] == 0 && \
                        [lindex $result $i] >= $bestr} {
                    lset mark $i 2
                }
            }
            for {set i [expr {$besti + 1}]} {$i < $size1} {incr i} {
                if {[lindex $mark $i] == 0 && \
                        [lindex $result $i] <= $bestr} {
                    lset mark $i 2
                }
            }
        }

        set prev $size2
        for {set i [expr {$size1 - 1}]} {$i >= 0} {incr i -1} {
            if {[lindex $mark $i] != 2} {
                set prev [lindex $result $i]
            } else {
                lset high $i [expr {$prev - 1}]
            }
        }
        set prev -1
        for {set i 0} {$i < $size1} {incr i} {
            if {[lindex $mark $i] != 2} {
                set prev [lindex $result $i]
            } else {
                if {[lindex $high $i] > $prev} {
                    incr prev
                    lset result $i $prev
                } else {
                    lset result $i -1
                }
            }
        }
        set scoresum 0
        for {set i 0} {$i < $size1} {incr i} {
            set j [lindex $result $i]
            set sc [lindex $scores $i $j] ;# FIXA: can this fail?
            if {[string is integer -strict $sc]} {
                #puts "Score: $i $j [lindex $scores $i $j]"
                incr scoresum $sc
            }
        }
        #puts "Scoresum: $scoresum ($bestscoresum)"
        
        # If it was not an improvement over previous iteration, quit
        if {$scoresum <= $bestscoresum} {
            break
        }

        set bestresult $result
        set bestscoresum $scoresum

        # We are redoing from start, but try to improve by
        # ignoring the most awkwardly placed line.
        set mostp -1
        set mosti 0
        for {set i 0} {$i < $size1} {incr i} {
            if {[lindex $mark $i] == 1} {
                if {abs([lindex $result $i] - $i) > $mostp} {
                    set mostp [expr {abs([lindex $result $i] - $i)}]
                    set mosti $i
                }
            }
        }
        #puts "Most $mosti $mostp"
        lset scoresbest $mosti 0
    }

    return $bestresult
}

# Decide how to display change blocks
# This tries to match the lines that resemble each other and put them
# next to each other.
# Returns diff-like codes to use as display info.
proc compareBlocks {block1 block2} {
    set size1 [llength $block1]
    set size2 [llength $block2]

    # Things below assume that block1 is not bigger than block2.
    # Swap if block1 is bigger
    if {$size1 > $size2} {
        set apa $block1
        set block1 $block2
        set block2 $apa
        set size1 [llength $block1]
        set size2 [llength $block2]
        # Swap output symbols
        set dsym a
        set asym d
    } else {
        set dsym d
        set asym a
    }

    set result [CompareBlocks2 $block1 $block2 scores]

    # Collect the result into diff-like codes to use as display info.

    set apa {}
    set t1 0
    set t2 0
    while {$t1 < $size1 || $t2 < $size2} {
        if {$t1 < $size1} {
            set r [lindex $result $t1]
            if {$r < $t2 || $t2 >= $size2} {
                # Deleted row
                lappend apa $dsym
                incr t1
            } elseif {$r == $t2} {
                #if {[string match Wm* [lindex $block2 $t2]]} {
                #    puts "Left : [lindex $block1 $t1]"
                #    puts "Right: [lindex $block2 $t2]"
                #    puts "Score: $scores($t1,$t2)"
                #}

                # If the score is too bad, don't do line parsing.
                if {[lindex $scores $t1 $t2] < 0} {
                    lappend apa "C"
                } else {
                    lappend apa "c"
                }
                incr t1
                incr t2
            } else {
                # Added row
                lappend apa $asym
                incr t2
            }
        } else {
            # Added row
            lappend apa $asym
            incr t2
        }
    }
    return $apa
}

