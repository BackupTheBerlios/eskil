#!/bin/sh
#----------------------------------------------------------------------
#
#  Eskil, a Graphical frontend to diff
#
#  Copyright (c) 1998-2004, Peter Spjuth  (peter.spjuth@space.se)
#
#  Usage
#             Do 'eskil.tcl' for interactive mode
#             Do 'eskil.tcl --help' for command line usage
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
# the next line restarts using tclsh \
exec tclsh "$0" "$@"

package provide app-eskil 2.0
package require Tcl 8.4
# Stop Tk from meddling with the command line by copying it first.
set ::eskil(argv) $::argv
set ::eskil(argc) $::argc
set ::argv {}
set ::argc 0
package require Tk 8.4
catch {package require textSearch}

package require pstools
namespace import -force pstools::*

if {[catch {package require psballoon}]} {
    # Add a dummy if it does not exist.
    proc addBalloon {args} {}
} else {
    namespace import -force psballoon::addBalloon
}

set debug 0
set diffver "Version 2.0.3+ 2004-05-26"
set thisScript [file join [pwd] [info script]]
set thisDir [file dirname $thisScript]

# Follow any link
set tmplink $thisScript
while {[file type $tmplink] eq "link"} {
    set tmplink [file readlink $tmplink]
    set tmplink [file normalize [file join $thisDir $tmplink]]
    set thisDir [file dirname $tmplink]
}
unset tmplink

set ::util(cvsExists) [expr {![string equal [auto_execok cvs] ""]}]
set ::util(diffexe) diff

# Diff functionality is in the DiffUtil package.
package require DiffUtil
# Help DiffUtil to find a diff executable, if needed
catch {DiffUtil::LocateDiffExe $thisScript}

# Figure out a place to store temporary files.
locateTmp ::diff(tmpdir)

if {$tcl_platform(platform) eq "windows"} {
    # Locate CVS if it is in c:/bin
    if {!$::util(cvsExists) && [file exists "c:/bin/cvs.exe"]} {
        set env(PATH) "$env(PATH);c:\\bin"
        auto_reset
        set ::util(cvsExists) [expr {![string equal [auto_execok cvs] ""]}]
    }
}

# Debug function to be able to reread the source even when wrapped in a kit.
proc EskilRereadSource {} {
    set this $::thisScript

    # Are we in a Starkit?
    if {[regexp {^(.*eskil)((?:\.[^/]+)?)(/lib/app-eskil.*)$} $this -> \
            pre ext post]} {
        if {$ext ne ".vfs"} {
            # If the unpacked vfs directory is available, read from that
            # instead.
            set src $pre.vfs$post
            if {[file readable $src]} {
                set this $src
            }
        }
    }
    puts "Resourcing $this"
    uplevel \#0 [list source $this]
}

# This function is called when a toplevel is closed.
# If it is the last remaining toplevel, the application quits.
# If top = "all" it means quit.
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
        tk_messageBox -icon error -title "Eskil Error" -message \
                "An error occured in the close process.\n$errMsg\n\
                (This is a bug)\nTerminating application." -type ok
    }
    if {$cont} return

    clearTmp
    exit
}

# Format a line number
proc myFormL {lineNo} {
    if {![string is integer -strict $lineNo]} {return "$lineNo\n"}
      return [format "%3d: \n" $lineNo]
}

proc maxAbs {a b} {
    return [expr {abs($a) > abs($b) ? $a : $b}]
}

proc tmpFile {} {
    if {[info exists ::tmpcnt]} {
        incr ::tmpcnt
    } else {
        set ::tmpcnt 0
    }
    set name [file join $::diff(tmpdir) "tmpd[pid]a$::tmpcnt"]
    lappend ::tmpfiles $name
    return $name
}

proc clearTmp {args} {
    if {![info exists ::tmpfiles]} {
        set ::tmpfiles {}
        return
    }
    if {[llength $args] > 0} {
        foreach f $args {
            set i [lsearch -exact $f $::tmpfiles]
            if {$i >= 0} {
                catch {file delete $f}
                set ::tmpfiles [lreplace $::tmpfiles $i $i]
            }
        }
    } else {
        foreach f $::tmpfiles {
            catch {file delete $f}
        }
        set ::tmpfiles {}
    }
}

# Compare two lines and rate how much they resemble each other.
# This has never worked well. Some day I'll sit down, think this through,
# and come up with a better algorithm.
proc compareLines2 {line1 line2} {
    set res [DiffUtil::diffStrings $line1 $line2]

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
#    puts "S $sumsame D $sumdiff1 D $sumdiff2"
    return [expr {$sumsame - [maxAbs $sumdiff1 $sumdiff2]}]
}

# Initialise a multidimensional list with empty values
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
# As the previous procedure, this would need a complete rework and a
# better algorithm.
proc compareBlocks {block1 block2} {
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
    set scores [Linit {} $size1 $size2]
    set emptyResult [Linit {} $size1]
    set scoresbest $emptyResult
    set origresult $emptyResult

    set j 0
    set bestsum 0
    foreach line1 $block1 {
        set bestscore -100000
        set bestline 0
        set i 0
        foreach line2 $block2 {
            set x [compareLines2 $line1 $line2]
            lset scores $j $i $x
#            puts "Score $j $i : $x"
            if {$x > $bestscore} {
                set bestscore $x
                set bestline $i
            }
            incr i
        }
#        puts "Best for $j is $bestline : $bestscore"
        lset origresult $j $bestline
        lset scoresbest $j $bestscore
        incr bestsum $bestscore
        incr j
    }
#    puts "Bestsum: $bestsum"

    # origresult holds a mapping between blocks where each row
    # is paired with its best match. This may not be a possible
    # result since it has to be in order.

    set bestresult $origresult
    set bestscoresum -100000

    # If the size is 1, it is automatically in order so we
    # don't need further processing.
    if {$size1 > 1} {

	# If both blocks are the same size, try first with the
	# simple row to row match, as a base score
	if {$size1 == $size2} {
	    set sum 0
	    set result $emptyResult
	    for {set i 0} {$i < $size1} {incr i} {
		lset result $i $i
		incr sum [lindex $scores $i $i]
	    }
#	    puts "Simple map sum: $sum"
	    set bestresult $result
	    set bestscoresum $sum
	}

	# If result is in order, no problem.
	# Otherwise, try to adjust result to make it ordered
        while {1} {
	    # The outer loop restarts from the "best mapping"
	    set result $origresult
            set mark [Linit 0 $size1]
            set high $mark

            while {1} {
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
#                puts "Best $besti order $order sc $bestscore"
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
#                    puts "Score: $i $j $scores($i,$j)"
                    incr scoresum $sc
                }
            }
#            puts "Scoresum: $scoresum ($bestscoresum)"

	    # If it was not an improvement over previous iteration, quit
            if {$scoresum <= $bestscoresum} {
                break
	    }

	    set bestresult $result
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
		if {[lindex $mark $i] == 1} {
		    if {abs([lindex $result $i] - $i) > $mostp} {
			set mostp [expr {abs([lindex $result $i] - $i)}]
                        set mosti $i
		    }
		}
	    }
#	    puts "Most $mosti $mostp"
	    lset scoresbest $mosti 0
        }
    }

    set result $bestresult

    # Collect the result into diff-like codes to use as display info.

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
proc insertLine {top n line text {tag {}}} {
    $::widgets($top,wDiff$n) insert end "$text\n" $tag
    if {$tag != ""} {
        set tag "hl$::HighLightCount $tag"
    }
    $::widgets($top,wLine$n) insert end [myFormL $line] $tag
}

proc emptyLine {top n {highlight 1}} {
    if {$highlight} {
        $::widgets($top,wLine$n) insert end "\n" hl$::HighLightCount
    } else {
        $::widgets($top,wLine$n) insert end "*****\n"
    }
    $::widgets($top,wDiff$n) insert end "\n"
}

# Insert one line in each text widget.
# Mark them as changed, and optionally parse them.
proc insertMatchingLines {top line1 line2} {
    global doingLine1 doingLine2 Pref

    if {$::diff(filter) != ""} {
        if {[regexp $::diff(filter) $line1]} {
            insertLine $top 1 $doingLine1 $line1
            insertLine $top 2 $doingLine2 $line2
            incr doingLine1
            incr doingLine2
            set ::diff(filterflag) 1
            return
        }
        set ::diff(filterflag) 0
    }

    if {$Pref(parse) != 0} {
        set opts $Pref(ignore)
        if {$Pref(lineparsewords)} {lappend opts -words}
        set res [eval DiffUtil::diffStrings $opts \$line1 \$line2]
        set dotag 0
        set n [expr {[llength $res] / 2}]
        $::widgets($top,wLine1) insert end [myFormL $doingLine1] "hl$::HighLightCount change"
        $::widgets($top,wLine2) insert end [myFormL $doingLine2] "hl$::HighLightCount change"
        set new1 "new1"
        set new2 "new2"
        set change "change"
        foreach {i1 i2} $res {
            incr n -1
            if {$dotag} {
                if {$n == 1 && $Pref(marklast)} {
                    lappend new1 last
                    lappend new2 last
                    lappend change last
                }
                if {$i1 eq ""} {
                    $::widgets($top,wDiff2) insert end $i2 $new2
                } elseif {$i2 eq ""} {
                    $::widgets($top,wDiff1) insert end $i1 $new1
                } else {
                    $::widgets($top,wDiff1) insert end $i1 $change
                    $::widgets($top,wDiff2) insert end $i2 $change
                }
                set dotag 0
            } else {
                $::widgets($top,wDiff1) insert end $i1
                $::widgets($top,wDiff2) insert end $i2
                set dotag 1
            }
        }
        $::widgets($top,wDiff1) insert end "\n"
        $::widgets($top,wDiff2) insert end "\n"
    } else {
        insertLine $top 1 $doingLine1 $line1 "change"
        insertLine $top 2 $doingLine2 $line2 "change"
    }
    incr doingLine1
    incr doingLine2
}

# Insert two blocks of lines in the compare windows.
# Returns number of lines used to display the blocks
proc insertMatchingBlocks {top block1 block2} {
    global doingLine1 doingLine2

    # A large block may take time.  Give a small warning.
    if {[llength $block1] * [llength $block2] > 1000} {
        set ::widgets($top,eqLabel) "!"
        #puts "Eskil warning: Analyzing a large block. ($size1 $size2)"
        update idletasks
    }

    set apa [compareBlocks $block1 $block2]

    set t1 0
    set t2 0
    foreach c $apa {
        if {$c eq "c"} {
            set textline1 [lindex $block1 $t1]
            set textline2 [lindex $block2 $t2]
            insertMatchingLines $top $textline1 $textline2
            incr t1
            incr t2
        } elseif {$c eq "C"} {
	    # This is two lines that the block matching considered
	    # too different to use line parsing on them.
	    # Marked the whole line as deleted/inserted
            set textline1 [lindex $block1 $t1]
            set textline2 [lindex $block2 $t2]
            $::widgets($top,wLine1) insert end [myFormL $doingLine1] \
                    "hl$::HighLightCount change"
            $::widgets($top,wDiff1) insert end "$textline1\n" new1
            $::widgets($top,wLine2) insert end [myFormL $doingLine2] \
                    "hl$::HighLightCount change"
            $::widgets($top,wDiff2) insert end "$textline2\n" new2
            incr doingLine1
            incr doingLine2
            incr t1
            incr t2
        } elseif {$c eq "d"} {
            set bepa [lindex $block1 $t1]
            $::widgets($top,wLine1) insert end [myFormL $doingLine1] \
                    "hl$::HighLightCount change"
            $::widgets($top,wDiff1) insert end "$bepa\n" new1
            emptyLine $top 2
            incr doingLine1
            incr t1
        } elseif {$c eq "a"} {
            set bepa [lindex $block2 $t2]
            $::widgets($top,wLine2) insert end [myFormL $doingLine2] \
                    "hl$::HighLightCount change"
            $::widgets($top,wDiff2) insert end "$bepa\n" new2
            emptyLine $top 1
            incr doingLine2
            incr t2
        }
    }
    return [llength $apa]
}

# Process one of the change/add/delete blocks reported by diff.
#  ch1 is a file channel for the left file
#  ch2 is a file channel for the right file
#  n1/n2 is the number of lines involved
#  line1/line2 says on what lines this block starts
# If n1/n2 are both 0, it means that this is the last lines to be displayed.
#  In that case line1/line2, if non-zero says the last line to display.
proc doText {top ch1 ch2 n1 n2 line1 line2} {
    global doingLine1 doingLine2 Pref

    if {$n1 == 0 && $n2 == 0} {
        # All blocks have been processed. Continue until end of file.
        # If "only diffs" is on, just display a couple of context lines.
        set limit -1
        if {$Pref(context) > 0} {
            set limit $Pref(context)
        }
	# Consider any total limit on displayed lines.
        if {$::diff($top,limitlines)} {
            set limit [expr {$::diff($top,limitlines) - $::diff($top,mapMax)}]
            if {$limit < 0} {
                set limit 0
            }
        }
        set t 0
        while {[gets $ch2 apa] != -1} {
            if {$line2 > 0 && $doingLine2 > $line2} break
            insertLine $top 2 $doingLine2 $apa
            incr doingLine2
            addMapLines $top 1
            incr t
            if {$limit >= 0 && $t >= $limit} break
        }
        set t 0
        while {[gets $ch1 apa] != -1} {
            if {$line1 > 0 && $doingLine1 > $line1} break
            insertLine $top 1 $doingLine1 $apa
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
    if {$Pref(context) > 0 && \
            ($line1 - $doingLine1 > (2 * $Pref(context) + 2))} {
        set limit $Pref(context)
    }
    set t 0
    while {$doingLine1 < $line1} {
        gets $ch1 apa
        gets $ch2 bepa
        if {$limit < 0 || ($t < $limit) || \
                ($line1 - $doingLine1) <= $limit} {
            insertLine $top 1 $doingLine1 $apa
            insertLine $top 2 $doingLine2 $bepa
            addMapLines $top 1
        } elseif {$t == $limit} {
            emptyLine $top 1 0
            emptyLine $top 2 0
            addMapLines $top 1
        }
        incr doingLine1
        incr doingLine2
        incr t
        if {$::diff($top,limitlines) && \
                ($::diff($top,mapMax) > $::diff($top,limitlines))} {
            return
        }
    }
    # This should not happen unless something is wrong...
    if {$doingLine2 != $line2} {
        $::widgets($top,wDiff1) insert end \
                "**Bad alignment here!! $doingLine2 $line2**\n"
        $::widgets($top,wDiff2) insert end \
                "**Bad alignment here!! $doingLine2 $line2**\n"
        $::widgets($top,wLine1) insert end "\n"
        $::widgets($top,wLine2) insert end "\n"
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
            addMapLines $top $n1
        } else {
            addChange $top $n1 change $line1 $n1 $line2 $n2
        }
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

            addChange $top $apa change $line1 $n1 $line2 $n2
        } else {
            # No extra parsing at all.
            for {set t 0} {$t < $n1} {incr t} {
                gets $ch1 apa
                insertLine $top 1 $doingLine1 $apa $tag1
                incr doingLine1
            }
            for {set t 0} {$t < $n2} {incr t} {
                gets $ch2 apa
                insertLine $top 2 $doingLine2 $apa $tag2
                incr doingLine2
            }
            if {$n1 <= $n2} {
                for {set t $n1} {$t < $n2} {incr t} {
                    emptyLine $top 1
                }
                addChange $top $n2 $tag2 $line1 $n1 $line2 $n2
            } elseif {$n2 < $n1} {
                for {set t $n2} {$t < $n1} {incr t} {
                    emptyLine $top 2
                }
                addChange $top $n1 $tag1 $line1 $n1 $line2 $n2
            }
        }
    }
}

proc enableRedo {top} {
    $top.mf.m entryconfigure "Redo Diff" -state normal
    $top.mt.m entryconfigure "Merge"     -state normal
}

proc disableRedo {top} {
    $top.mf.m entryconfigure "Redo Diff" -state disabled
    $top.mt.m entryconfigure "Merge"     -state disabled
}

proc busyCursor {top} {
    global oldcursor oldcursor2
    if {![info exists oldcursor]} {
        set oldcursor [$top cget -cursor]
        set oldcursor2 [$::widgets($top,wDiff1) cget -cursor]
    }
    $top config -cursor watch
    foreach item {wLine1 wDiff1 wLine2 wDiff2} {
        if {[info exists ::widgets($top,$item)]} {
            set w $::widgets($top,$item)
            $w config -cursor watch
        }
    }
}

proc normalCursor {top} {
    global oldcursor oldcursor2
    $top config -cursor $oldcursor
    foreach item {wLine1 wDiff1 wLine2 wDiff2} {
        if {[info exists ::widgets($top,$item)]} {
            set w $::widgets($top,$item)
            $w config -cursor $oldcursor2
        }
    }
}

# Read a conflict file and extract the two versions.
proc prepareConflict {top} {
    global Pref

    set ::diff($top,leftFile) [tmpFile]
    set ::diff($top,rightFile) [tmpFile]

    set ch1 [open $::diff($top,leftFile) w]
    set ch2 [open $::diff($top,rightFile) w]
    set ch [open $::diff($top,conflictFile) r]

    set ::diff($top,conflictDiff) {}
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
        } elseif {[string match ======* $line] && $state eq "right"} {
            set state left
            set end2 [expr {$rightLine - 1}]
            set start1 $leftLine
        } elseif {[string match >>>>>>* $line] && $state eq "left"} {
            set state both
            regexp {>*\s*(.*)} $line -> leftName
            set end1 [expr {$leftLine - 1}]
            lappend diff($top,conflictDiff) [list \
                    $start1 [expr {$end1 - $start1 + 1}] \
                    $start2 [expr {$end2 - $start2 + 1}]]
        } elseif {$state eq "both"} {
            puts $ch1 $line
            puts $ch2 $line
            incr leftLine
            incr rightLine
        } elseif {$state eq "left"} {
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

    if {$leftName eq "" && $rightName eq ""} {
        set leftName "No Conflict: [file tail $::diff($top,conflictFile)]"
        set rightName $leftName
    }
    set ::diff($top,leftLabel) $leftName
    set ::diff($top,rightLabel) $rightName
    update idletasks
}

# Clean up after a conflict diff.
proc cleanupConflict {top} {
    global Pref

    clearTmp $::diff($top,rightFile) $::diff($top,leftFile)
    set ::diff($top,rightFile) $::diff($top,conflictFile)
    set ::diff($top,leftFile) $::diff($top,conflictFile)
}

# Display one chunk from a patch file
proc displayOnePatch {top leftLines rightLines leftLine rightLine} {
    emptyLine $top 1
    emptyLine $top 2

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
            addMapLines $top [insertMatchingBlocks $top $lblock $rblock]
            set lblock {}
            set rblock {}
        }
        if {$lmode == "" && $rmode == ""} {
            insertLine $top 1 $lline $lstr
            insertLine $top 2 $rline $rstr
            incr leftc
            incr rightc
            addMapLines $top 1
            continue
        }
        if {$lmode == "-"} {
            insertLine $top 1 $lline $lstr new1
            emptyLine $top 2
            incr leftc
            addMapLines $top 1
            continue
        }
        if {$rmode == "+"} {
            insertLine $top 2 $rline $rstr new2
            emptyLine $top 1
            incr rightc
            addMapLines $top 1
            continue
        }
    }
}

# Read a patch file and display it
proc displayPatch {top} {
    global Pref

    set ::diff($top,leftLabel) "Patch $::diff($top,patchFile): old"
    set ::diff($top,rightLabel) "Patch $::diff($top,patchFile): new"
    update idletasks

    set ch [open $::diff($top,patchFile) r]

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
        if {$state eq "none" && [regexp {^\*\*\*} $line]} {
            set state newfile
            set style c
            set leftRE {^\*\*\*\s+(.*)$}
            set rightRE {^---\s+(.*)$}
        }
        # Detect the first line in a -u style diff
        if {$state eq "none" && [regexp {^---} $line]} {
            set state newfile
            set style u
            set leftRE {^---\s+(.*)$}
            set rightRE {^\+\+\+\s+(.*)$}
        }
        if {$state eq "newfile" && [regexp $leftRE $line -> sub]} {
            emptyLine $top 1
            insertLine $top 1 "" $divider
            insertLine $top 1 "" $sub
            insertLine $top 1 "" $divider
            addChange $top 4 change 0 0 0 0
            continue
        }
        if {$state eq "newfile" && [regexp $rightRE $line -> sub]} {
            emptyLine $top 2
            insertLine $top 2 "" $divider
            insertLine $top 2 "" $sub
            insertLine $top 2 "" $divider
            continue
        }
        # A new section in a -u style diff
        if {[regexp {^@@\s+-(\d+),\d+\s+\+(\d+),} $line -> sub1 sub2]} {
            if {$state eq "both"} {
                displayOnePatch $top $leftLines $rightLines \
                        $leftLine $rightLine
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
            if {$state eq "right"} {
                displayOnePatch $top $leftLines $rightLines $leftLine $rightLine
            }
            set leftLines {}
            set rightLines {}
            set state left
            continue
        }
        # We are in the left part of a -c style diff
        if {$state eq "left"} {
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
        if {$state eq "right"} {
            if {![regexp {^[\s!+-]} $line]} continue
            lappend rightLines [list $rightLine \
                    [string trim [string range $line 0 1]] \
                    [string range $line 2 end]]
            incr rightLine
            continue
        }
        # We are in a -u style diff
        if {$state eq "both"} {
            if {![regexp {^[\s+-]} $line]} continue
            set sig [string trim [string index $line 0]]
            set str [string range $line 1 end]
            if {$sig eq ""} {
                lappend leftLines [list $leftLine "" $str]
                lappend rightLines [list $leftLine "" $str]
                incr leftLine
                incr rightLine
            } elseif {$sig eq "-"} {
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
    global Pref

    set revs {}

    # Search for revision options
    if {$::diff($top,doptrev1) != ""} {
        lappend revs $::diff($top,doptrev1)
    }
    if {$::diff($top,doptrev2) != ""} {
        lappend revs $::diff($top,doptrev2)
    }

    switch [llength $revs] {
        0 {
            # Compare local file with latest version.
            set ::diff($top,leftFile) [tmpFile]
            set ::diff($top,rightLabel) $::diff($top,RCSFile)
            set ::diff($top,rightFile)  $::diff($top,RCSFile)

            if {$::diff($top,mode) eq "CVS"} {
                set ::diff($top,leftLabel) "$::diff($top,RCSFile) (CVS)"
                execCvsUpdate $::diff($top,RCSFile) $::diff($top,leftFile)
            } else {
                set ::diff($top,leftLabel) "$::diff($top,RCSFile) (RCS)"
                catch {exec co -p [file nativename $::diff($top,RCSFile)] \
                        > $::diff($top,leftFile)}
            }
        }
        1 {
            # Compare local file with specified version.
            set r [lindex $revs 0]
            set ::diff($top,leftFile) [tmpFile]
            set ::diff($top,rightLabel) $::diff($top,RCSFile)
            set ::diff($top,rightFile) $::diff($top,RCSFile)

            if {$::diff($top,mode) eq "CVS"} {
                set ::diff($top,leftLabel) "$::diff($top,RCSFile) (CVS $r)"
                execCvsUpdate $::diff($top,RCSFile) $::diff($top,leftFile) -r $r
            } else {
                set ::diff($top,leftLabel) "$::diff($top,RCSFile) (RCS $r)"
                catch {exec co -p$r [file nativename $::diff($top,RCSFile)] \
                        > $::diff($top,leftFile)}
            }
        }
        default {
            # Compare the two specified versions.
            set r1 [lindex $revs 0]
            set r2 [lindex $revs 1]
            set ::diff($top,leftFile) [tmpFile]
            set ::diff($top,rightFile) [tmpFile]

            if {$::diff($top,mode) eq "CVS"} {
                set ::diff($top,leftLabel) "$::diff($top,RCSFile) (CVS $r1)"
                set ::diff($top,rightLabel) "$::diff($top,RCSFile) (CVS $r2)"
                execCvsUpdate $::diff($top,RCSFile) $::diff($top,leftFile) -r $r1
                execCvsUpdate $::diff($top,RCSFile) $::diff($top,rightFile) -r $r2
            } else {
                set ::diff($top,leftLabel) "$::diff($top,RCSFile) (RCS $r1)"
                set ::diff($top,rightLabel) "$::diff($top,RCSFile) (RCS $r2)"
                catch {exec co -p$r1 [file nativename $::diff($top,RCSFile)] \
                        > $::diff($top,leftFile)}
                catch {exec co -p$r2 [file nativename $::diff($top,RCSFile)] \
                        > $::diff($top,rightFile)}
            }
        }
    }
    # Make sure labels are updated before processing starts
    update idletasks
}

# Clean up after a RCS/CVS/CT diff.
proc cleanupRCS {top} {
    global Pref

    clearTmp $::diff($top,rightFile) $::diff($top,leftFile)
    set ::diff($top,rightFile) $::diff($top,RCSFile)
    set ::diff($top,leftFile) $::diff($top,RCSFile)
}

# Prepare for ClearCase diff. Checkout copies of the versions needed.
proc prepareClearCase {top} {
    global Pref

    # Compare local file with latest version.
    set ::diff($top,leftFile) [tmpFile]
    set ::diff($top,rightLabel) $::diff($top,RCSFile)
    set ::diff($top,rightFile) $::diff($top,RCSFile)

    set ::diff($top,leftLabel) "$::diff($top,RCSFile) (CT)"
    if {[catch {exec cleartool ls $::diff($top,RCSFile)} info]} {
        puts "Cleartool error: $info"
        return
    }

    set prevV {}
    if {![regexp {@@(\S+)\s+from (\S+)\s+Rule} $info -> thisV prevV]} {
        regexp {@@(\S+)} $info -> thisV
        # Maybe do something fancy here?
        set prevV $thisV
    }

    if {[catch {exec cleartool get -to $::diff($top,leftFile) [file nativename $::diff($top,RCSFile)@@$prevV]} msg]} {
        puts "Cleartool error: $msg"
        return
    }
}

# Prepare for a diff by creating needed temporary files
proc prepareFiles {top} {
    set ::diff($top,cleanup) ""
    if {$::diff($top,mode) eq "RCS" || $::diff($top,mode) eq "CVS"} {
        prepareRCS $top
        set ::diff($top,cleanup) "RCS"
    } elseif {$::diff($top,mode) eq "CT"} {
        prepareClearCase $top
        set ::diff($top,cleanup) "CT"
    } elseif {[string match "conflict*" $::diff($top,mode)]} {
        prepareConflict $top
        set ::diff($top,cleanup) "conflict"
    }
}

# Clean up after a diff
proc cleanupFiles {top} {
    switch $::diff($top,cleanup) {
        "RCS" - "CT" {cleanupRCS      $top}
        "conflict"   {cleanupConflict $top}
    }
}

# Main diff function.
proc doDiff {top} {
    global Pref
    global doingLine1 doingLine2

    if {$::diff($top,mode) eq "" && ($::diff($top,leftOK) == 0 || $::diff($top,rightOK) == 0)} {
        disableRedo $top
        return
    } else {
        enableRedo $top
    }

    busyCursor $top

    # Clear up everything before starting processing
    foreach item {wLine1 wDiff1 wLine2 wDiff2} {
        set w $::widgets($top,$item)
        $w configure -state normal
        $w delete 1.0 end
    }
    clearMap $top
    set ::HighLightCount 0
    highLightChange $top -1
    # Display a star during diff execution, to know when the internal
    # processing starts, and when the label is "valid".
    set ::widgets($top,eqLabel) "*"

    update idletasks

    if {$::diff($top,mode) eq "patch"} {
        displayPatch $top
        drawMap $top -1
        foreach item {wLine1 wLine2} {
            set w $::widgets($top,$item)
            $w configure -state disabled
        }
        update idletasks
        $::widgets($top,wLine2) see 1.0
        normalCursor $top
        return
    } else {
        prepareFiles $top
    }

    # Run diff and parse the result.
    set opts $Pref(ignore)
    if {[info exists ::diff($top,aligns)] && \
            [llength $::diff($top,aligns)] > 0} {
        lappend opts -align $::diff($top,aligns)
    }
    set range {}
    if {[info exists ::diff($top,range)] && \
            [llength $::diff($top,range)] == 4} {
        set range $::diff($top,range)
        lappend opts -range $range
    }
    set differr [catch {eval DiffUtil::diffFiles $opts \
            \$::diff($top,leftFile) \$::diff($top,rightFile)} diffres]

    # In conflict mode we can use the diff information collected when
    # parsing the conflict file. This makes sure the blocks in the conflict
    # file become change-blocks during merge.
    if {$::diff($top,mode) eq "conflictPure"} {
        set diffres $::diff($top,conflictDiff)
    }

    if {$differr != 0} {
        $::widgets($top,wDiff1) insert end $diffres
        normalCursor $top
        return
    }
    if {[llength $diffres] == 0} {
        set ::widgets($top,eqLabel) "="
    } else {
        set ::widgets($top,eqLabel) " "
    }
    # Update the equal label immediately for better feedback
    update idletasks

    set firstview 1

    set ch1 [open $::diff($top,leftFile)]
    set ch2 [open $::diff($top,rightFile)]
    if {$::tcl_platform(platform) eq "windows" && $Pref(crlf)} {
        fconfigure $ch1 -translation crlf
        fconfigure $ch2 -translation crlf
    }
    set doingLine1 1
    set doingLine2 1

    # If there is a range, skip lines up to the range
    if {[llength $range] != 0} {
        foreach {start1 end1 start2 end2} $range break
        while {$doingLine1 < $start1 && [gets $ch1 line] >= 0} {
            incr doingLine1
        }
        while {$doingLine2 < $start2 && [gets $ch2 line] >= 0} {
            incr doingLine2
        }
    }

    set t 0
    foreach i $diffres {
        foreach {line1 n1 line2 n2} $i break
        doText $top $ch1 $ch2 $n1 $n2 $line1 $line2
        if {$::diff($top,limitlines) && \
                ($::diff($top,mapMax) > $::diff($top,limitlines))} {
            break
        }
        bindHighlight $top
        incr ::HighLightCount

        # Get one update when the screen has been filled.
        # Show the first diff.
        if {$firstview && $::diff($top,mapMax) > 100} {
            set firstview 0
            showDiff $top 0
            update idletasks
        }
        if {0 && [incr t] >= 10} {
	    update idletasks
	    $::widgets($top,wLine2) see end
	    update idletasks
            set t 0
        }
    }

    # If there is a range, just display the range
    if {[llength $range] != 0} {
        foreach {start1 end1 start2 end2} $range break
    } else {
        set end1 0
        set end2 0
    }
    doText $top $ch1 $ch2 0 0 $end1 $end2

    # Make sure all text widgets have the same number of lines.
    # The common y scroll doesn't work well if not.
    set max 0.0
    foreach item {wLine1 wDiff1 wLine2 wDiff2} {
        set w $::widgets($top,$item)
        if {[$w index end] > $max} {
            set max [$w index end]
        }
    }
    foreach item {wLine1 wDiff1 wLine2 wDiff2} {
        set w $::widgets($top,$item)
        set d [expr {int($max) - int([$w index end])}]
        for {set t 0} {$t < $d} {incr t} {
            $w insert end \n
        }
    }

    close $ch1
    close $ch2

    drawMap $top -1
    foreach item {wLine1 wLine2} {
        set w $::widgets($top,$item)
        $w configure -state disabled
    }
    update idletasks
    $::widgets($top,wLine2) see 1.0
    normalCursor $top
    showDiff $top 0
    if {$::widgets($top,eqLabel) eq "!"} {
        set ::widgets($top,eqLabel) " "
    }

    cleanupFiles $top
    if {[string match "conflict*" $::diff($top,mode)]} {
        if {$::widgets($top,eqLabel) != "="} {
            makeMergeWin $top
        }
    }
    if {$::diff($top,printFile) != ""} {
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
    showDiff $top [expr {$::diff($top,currHighLight) + $delta}]
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
    if {[info exists ::diff($top,currHighLight)] && \
            $::diff($top,currHighLight) >= 0} {
        $::widgets($top,wLine1) tag configure hl$::diff($top,currHighLight) \
                -background {}
        $::widgets($top,wLine2) tag configure hl$::diff($top,currHighLight) \
                -background {}
    }
    set ::diff($top,currHighLight) $n
    if {$::diff($top,currHighLight) < 0} {
        set ::diff($top,currHighLight) -1
    } elseif {$::diff($top,currHighLight) >= [llength $::diff($top,changes)]} {
        set ::diff($top,currHighLight) [llength $::diff($top,changes)]
    } else {
        $::widgets($top,wLine1) tag configure hl$::diff($top,currHighLight) \
                -background yellow
        $::widgets($top,wLine2) tag configure hl$::diff($top,currHighLight) \
                -background yellow
    }
}

# Highlight a diff and scroll windows to it.
proc showDiff {top num} {
    highLightChange $top $num

    set change [lindex $::diff($top,changes) $::diff($top,currHighLight)]
    set line1 [lindex $change 0]

    if {$::diff($top,currHighLight) < 0} {
        set line1 1.0
        set line2 1.0
    } elseif {$line1 eq ""} {
        set line1 end
        set line2 end
    } else {
        set line2 [expr {$line1 + [lindex $change 1]}]
        incr line1
        set line1 $line1.0
        set line2 $line2.0
    }

    foreach item {wLine1 wDiff1 wLine2 wDiff2} {
        set w $::widgets($top,$item)
        seeText $w $line1 $line2
    }
}

#####################################
# File dialog stuff
#####################################

# A wrapper for tk_getOpenFile
proc myOpenFile {args} {
    # When in tutorial mode, make sure the Tcl file dialog is used
    # to be able to access the files in a starkit.
    if {[info exists ::diff(tutorial)] && $::diff(tutorial)} {
        # Only do this if tk_getOpenFile is not a proc.
        if {[info procs tk_getOpenFile] eq ""} {
            # If there is any problem, call the real one
            if {![catch {set res [eval ::tk::dialog::file:: open $args]}]} {
                return $res
            }
        }
    }
    return [eval tk_getOpenFile $args]
}

proc doOpenLeft {top {forget 0}} {
    if {!$forget && [info exists diff($top,leftDir)]} {
        set initDir $::diff($top,leftDir)
    } elseif {[info exists diff($top,rightDir)]} {
        set initDir $::diff($top,rightDir)
    } else {
        set initDir [pwd]
    }

    set apa [myOpenFile -title "Select left file" -initialdir $initDir \
            -parent $top]
    if {$apa != ""} {
        set ::diff($top,leftDir) [file dirname $apa]
        set ::diff($top,leftFile) $apa
        set ::diff($top,leftLabel) $apa
        set ::diff($top,leftOK) 1
        return 1
    }
    return 0
}

proc doOpenRight {top {forget 0}} {
    if {!$forget && [info exists diff($top,rightDir)]} {
        set initDir $::diff($top,rightDir)
    } elseif {[info exists diff($top,leftDir)]} {
        set initDir $::diff($top,leftDir)
    } else {
        set initDir [pwd]
    }

    set apa [myOpenFile -title "Select right file" -initialdir $initDir \
            -parent $top]
    if {$apa != ""} {
        set ::diff($top,rightDir) [file dirname $apa]
        set ::diff($top,rightFile) $apa
        set ::diff($top,rightLabel) $apa
        set ::diff($top,rightOK) 1
        return 1
    }
    return 0
}

proc openLeft {top} {
    if {[doOpenLeft $top]} {
        set ::diff($top,mode) ""
        doDiff $top
    }
}

proc openRight {top} {
    if {[doOpenRight $top]} {
        set ::diff($top,mode) ""
        doDiff $top
    }
}

proc openConflict {top} {
    global Pref
    if {[doOpenRight $top]} {
        set ::diff($top,mode) "conflict"
        set Pref(ignore) " "
        set ::diff($top,conflictFile) $::diff($top,rightFile)
        set ::diff($top,mergeFile) ""
        doDiff $top
    }
}

proc openPatch {top} {
    global Pref
    if {[doOpenLeft $top]} {
        set ::diff($top,mode) "patch"
        set Pref(ignore) " "
        set ::diff($top,patchFile) $::diff($top,leftFile)
        doDiff $top
    }
}

proc openRCS {top} {
    if {[doOpenRight $top]} {
        set ::diff($top,mode) "RCS"
        set ::diff($top,RCSFile) $::diff($top,rightFile)
        set ::diff($top,leftLabel) "RCS"
        set ::diff($top,leftOK) 0
        doDiff $top
    }
}

proc openCVS {top} {
    if {[doOpenRight $top]} {
        set ::diff($top,mode) "CVS"
        set ::diff($top,RCSFile) $::diff($top,rightFile)
        set ::diff($top,leftLabel) "CVS"
        set ::diff($top,leftOK) 0
        doDiff $top
    }
}

proc openBoth {top forget} {
    if {[doOpenLeft $top]} {
        if {[doOpenRight $top $forget]} {
            set ::diff($top,mode) ""
            doDiff $top
        }
    }
}

#####################################
# Map stuff
#####################################

proc clearMap {top} {
    set ::diff($top,changes) {}
    set ::diff($top,mapMax) 0
    drawMap $top -1
}

proc addChange {top n tag line1 n1 line2 n2} {
    if {$tag ne ""} {
        lappend ::diff($top,changes) [list $::diff($top,mapMax) $n \
                $tag $line1 $n1 $line2 $n2]
    }
    incr ::diff($top,mapMax) $n
}

proc addMapLines {top n} {
    incr ::diff($top,mapMax) $n
}

proc drawMap {top newh} {
    global Pref

    set oldh [map$top cget -height]
    if {$oldh == $newh} return

    map$top blank
    if {![info exists ::diff($top,changes)] || \
            [llength $::diff($top,changes)] == 0} return

    set w [winfo width $top.c]
    set h [winfo height $top.c]
    set x2 [expr {$w - 1}]
    map$top configure -width $w -height $h
    incr h -1
    foreach change $::diff($top,changes) {
        foreach {start length type dum1 dum2 dum3 dum4} $change break
        set y1 [expr {$start * $h / $::diff($top,mapMax) + 1}]
        if {$y1 < 1} {set y1 1}
        if {$y1 > $h} {set y1 $h}
        set y2 [expr {($start + $length) * $h / $::diff($top,mapMax) + 1}]
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
    # FIXA separate merge variables per top
    global mergeSelection
    global leftMergeData rightMergeData

    set leftMergeData {}
    set rightMergeData {}

    if {![info exists ::diff($top,changes)]} {
        set ::diff($top,changes) {}
    }

    if {$::diff($top,mode) eq "RCS" || $::diff($top,mode) eq "CVS"} {
        prepareRCS $top
    } elseif {[string match "conflict*" $::diff($top,mode)]} {
        prepareConflict $top
    }

    set ch1 [open $::diff($top,leftFile) r]
    set ch2 [open $::diff($top,rightFile) r]
    set doingLine1 1
    set doingLine2 1
    set changeNo 0
    foreach change $::diff($top,changes) {
        foreach {start length type line1 n1 line2 n2} $change break
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

    if {$::diff($top,mode) eq "RCS" || $::diff($top,mode) eq "CVS"} {
        cleanupRCS $top
    } elseif {[string match "conflict*" $::diff($top,mode)]} {
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
    set w $top.merge.t

    if {$::diff($top,mergeFile) eq ""} {
        set apa no
        if {[string match "conflict*" $::diff($top,mode)]} {
            set apa [tk_messageBox -parent $top.merge -icon question \
                    -title "Save merge file" -type yesno -message \
                    "Do you want to overwrite the original conflict file?"]
        }
        if {$apa == "yes"} {
            set ::diff($top,mergeFile) $::diff($top,conflictFile)
        } else {
            # Browse
            if {[info exists diff($top,rightDir)]} {
                set initDir $::diff($top,rightDir)
            } elseif {[info exists diff($top,leftDir)]} {
                set initDir $::diff($top,leftDir)
            } else {
                set initDir [pwd]
            }

            set apa [tk_getSaveFile -title "Save merge file" -initialdir $initDir \
                    -parent $top.merge]
            if {$apa eq ""} return
            set ::diff($top,mergeFile) $apa
        }
    }

    set ch [open $::diff($top,mergeFile) "w"]
    puts -nonewline $ch [$w get 1.0 end-1char]
    close $ch
    tk_messageBox -parent $top.merge -icon info -type ok -title "Diff" \
            -message "Saved merge to file $::diff($top,mergeFile)."
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
    wm protocol $w WM_DELETE_WINDOW "closeMerge $top"

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
    if {[string is integer -strict $lineno]} {
        set res [format "%3d: " $lineno]
    } else {
        set res [format "%-5s" $lineno]
        set gray 0.9
    }
    if {[string length $res] > 5} {
        set res [string range $res end-5 end-1]
    }
    if {$gray eq "1.0"} {
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
        if {$key eq "tagon"} {
            if {$value eq "change"} {
                set gray $::grayLevel1
            } elseif {[string match "new*" $value]} {
                set gray $::grayLevel2
            }
        } elseif {$key eq "tagoff"} {
            if {$value eq "change" || [string match "new*" $value]} {
                set gray 1.0
            }
        } elseif {$key eq "text"} {
            append line $value
            if {[string index $value end] eq "\n"} {
                set line [string trim [string trim $line] :]
                if {$line eq ""} {
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
proc lineWrap {gray} {
    if {$gray eq "1.0"} {
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
        if {$i eq -1} break
        set n [expr {(- $i - $index - 1) % 8 + 1}]
        set text [string replace $text $i $i [format %${n}s ""]]
    }
    return $text
}

# Main print function
proc printDiffs {top {quiet 0}} {
    busyCursor $top
    update idletasks
    set tmpFile [file nativename ~/eskil.enscript]
    if {$::diff($top,printFile) != ""} {
        set tmpFile2 [file nativename $::diff($top,printFile)]
    } else {
        set tmpFile2 [file nativename ~/eskil.ps]
    }

    set lines1 {}
    set lines2 {}

    set tdump1 [$::widgets($top,wDiff1) dump -tag -text 1.0 end]
    set tdump2 [$::widgets($top,wDiff2) dump -tag -text 1.0 end]
    set lineNo1 [processLineno $::widgets($top,wLine1)]
    set lineNo2 [processLineno $::widgets($top,wLine2)]

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
                    if {[string index $value end] eq "\n"} {
                        set newline 1
                        set value [string trimright $value "\n"]
                    }
                    set len [string length $value]
                    while {$chars + $len > 84} {
                        set wrap [expr {84 - $chars}]
                        set val1 [string range $value 0 [expr {$wrap - 1}]]
                        set value [string range $value $wrap end]
                        append line $val1
                        append line [lineWrap $gray]
                        set chars 5
                        incr wrapc
                        set len [string length $value]
                    }
                    append line $value
                    incr chars $len
                }
                tagon {
                    if {$value eq "change"} {
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

    if {$::tcl_platform(platform) eq "windows" &&\
            ![info exists ::env(ENSCRIPT_LIBRARY)]} {
        set ::env(ENSCRIPT_LIBRARY) [pwd]
    }
    set enscriptCmd [list enscript -2jcre -L 66 -M A4]
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
        wm title .dp "Eskil Print"
        button .dp.b -text "Close" -command {destroy .dp}
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
            file \"eskil.ps\" in your home directory."
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

    button .pr.b1 -text "Print" -width 7 \
            -command "destroy .pr; update; printDiffs $top"
    button .pr.b2 -text "Cancel" -width 7 \
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

################
# Align function
################

proc enableAlign {top} {
    eval $::widgets($top,enableAlignCmd)
}

proc disableAlign {top} {
    eval $::widgets($top,disableAlignCmd)
}

proc clearAlign {top} {
    set ::diff($top,aligns) {}
    disableAlign $top
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

        lappend ::diff($top,aligns) $::diff($top,align1) $::diff($top,align2)
        enableAlign $top

        unset ::diff($top,align1)
        unset ::diff($top,align2)
        unset ::diff($top,aligntext1)
        unset ::diff($top,aligntext2)
    }
}

# Called by popup menus over row numbers to add command for alignment.
# Returns 1 if nothing was added.
proc alignMenu {m top n x y} {
    # Get the row that was clicked
    set w $::widgets($top,wLine$n)
    set index [$w index @$x,$y]
    set row [lindex [split $index "."] 0]

    set data [$w get $row.0 $row.end]
    if {![regexp {\d+} $data line]} {
        return 1
    }
    set text [$::widgets($top,wDiff$n) get $row.0 $row.end]

    set other [expr {$n == 1 ? 2 : 1}]
    if {![info exists ::diff($top,align$other)]} {
        set label "Mark line for alignment"
    } else {
        set label "Align with line $::diff($top,align$other) on other side"
    }

    .lpm add command -label $label -command [list markAlign $top $n $line $text]
    return 0
}

###################
# Diff highlighting
###################

proc hlSelect {top hl} {
    highLightChange $top $hl
}

proc hlSeparate {top n hl} {
    set ::diff($top,separate$n) $hl
    set wd $::widgets($top,wDiff$n)
    set wl $::widgets($top,wLine$n)

    if {$hl eq ""} {
        set range [$wd tag ranges sel]
    } else {
        set range [$wl tag ranges hl$::diff($top,separate$n)]
    }
    set text [eval $wd get $range]
    set ::diff($top,separatetext$n) $text

    # Get the lines involved in the display
    set from [lindex $range 0]
    set to   [lindex $range 1]
    foreach {froml fromi} [split $from "."] break
    foreach {tol   toi}   [split $to   "."] break
    if {$toi == 0} {incr tol -1}
    # Get the corresponding lines in the file
    set t [$wl get $froml.0 $tol.end]
    set lines [lsort -integer [regexp -all -inline {\d+} $t]]
    set froml [lindex $lines 0]
    set tol [lindex $lines end]
    set ::diff($top,separatelines$n) [list $froml $tol]

    if {[info exists ::diff($top,separate1)] && \
            [info exists ::diff($top,separate2)]} {
        if {1} {
            cloneDiff $top [concat $::diff($top,separatelines1) \
                    $::diff($top,separatelines2)]
        } else {
            set f1 [tmpFile]
            set f2 [tmpFile]
            set ch [open $f1 w]
            puts $ch $::diff($top,separatetext1)
            close $ch
            set ch [open $f2 w]
            puts $ch $::diff($top,separatetext2)
            close $ch

            newDiff $f1 $f2
        }
        unset ::diff($top,separate1)
        unset ::diff($top,separate2)
    }
}

proc hlPopup {top n hl X Y x y} {
    if {[info exists ::diff($top,nopopup)] && $::diff($top,nopopup)} return
    destroy .lpm
    menu .lpm

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
    menu .lpm

    regexp {(\d+)\D*$} $w -> n
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
        $::widgets($top,wLine$n) tag bind $tag <ButtonPress-3> \
                "hlPopup $top $n $::HighLightCount %X %Y %x %y ; break"
        $::widgets($top,wLine$n) tag bind $tag <ButtonPress-1> \
                "hlSelect $top $::HighLightCount"
    }
}

#########
# Zooming
#########

proc zoomRow {w X Y x y} {
    global Pref
    set top [winfo toplevel $w]
    # Get the row that was clicked
    set index [$w index @$x,$y]
    set row [lindex [split $index "."] 0]

    # Check if it is selected
    if {[lsearch [$w tag names $index] sel] >= 0} {
        regexp {(\d+)\D*$} $w -> n
        hlPopup $top $n "" $X $Y $x $y
        return
    }

    # Extract the data
    set data(1) [$::widgets($top,wDiff1) dump -tag -text $row.0 $row.end]
    set data(2) [$::widgets($top,wDiff2) dump -tag -text $row.0 $row.end]
    if {[llength $data(1)] == 0 && [llength $data(2)] == 0} return

    set font [$::widgets($top,wDiff1) cget -font]
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
            if {$key eq "tagon"} {
                lappend tags $value
                set tags [lsort -unique $tags]
            } elseif {$key eq "tagoff"} {
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
    if {$wid + $wx > [winfo rootx $top] + [winfo width $top]} {
        # No.
        # Center on diff window
        set wx [expr {([winfo width $top] - $wid) / 2 + [winfo rootx $top]}]
        if {$wx < 0} {set wx 0}
        # Is the balloon not within the screen?
        if {$wx + $wid > [winfo screenwidth $top]} {
            # Center in screen
            set wx [expr {([winfo screenwidth $top] - $wid) / 2}]
            if {$wx < 0} {set wx 0}
        }
    }

    # Does the balloon fit within the screen?
    if {$wid > [winfo screenwidth $top]} {
        # How many rows does it take?
        set rows [expr {ceil(double($wid) / [winfo screenwidth $top])}]
        # Add rows and fill screen width
        $top.balloon.t1 configure -height $rows
        $top.balloon.t2 configure -height $rows
        # Let geometry requests propagate
        update idletasks
        wm geometry $top.balloon \
                [winfo screenwidth $top]x[winfo reqheight $top.balloon]
        set wx 0
    }
    wm geometry $top.balloon +$wx+$wy
    wm deiconify $top.balloon
}

proc unzoomRow {w} {
    set top [winfo toplevel $w]
    destroy $top.balloon
}

# Reconfigure font
proc chFont {} {
    global Pref

    font configure myfont -size $Pref(fontsize) -family $Pref(fontfamily)
}

# Change color settings
proc applyColor {} {
    global dirdiff Pref

    foreach top $::diff(diffWindows) {
        if {$top eq ".clipdiff"} continue
        if {$top != ".dirdiff"} {
            foreach item {wLine1 wDiff1 wLine2 wDiff2} {
                set w $::widgets($top,$item)

                $w tag configure new1 -foreground $Pref(colornew1) \
                        -background $Pref(bgnew1)
                $w tag configure change -foreground $Pref(colorchange) \
                        -background $Pref(bgchange)
                $w tag configure new2 -foreground $Pref(colornew2) \
                        -background $Pref(bgnew2)
            }
            continue
        }

        $dirdiff(wLeft) tag configure new1 -foreground $Pref(colornew1) \
                -background $Pref(bgnew1)
        $dirdiff(wLeft) tag configure change -foreground $Pref(colorchange) \
                -background $Pref(bgchange)
        $dirdiff(wLeft) tag configure changed -foreground $Pref(colorchange)
        $dirdiff(wLeft) tag configure invalid -background #a9a9a9
        $dirdiff(wRight) tag configure new2 -foreground $Pref(colornew2) \
                -background $Pref(bgnew2)
        $dirdiff(wRight) tag configure change -foreground $Pref(colorchange) \
                -background $Pref(bgchange)
        $dirdiff(wRight) tag configure changed -foreground $Pref(colorchange)
        $dirdiff(wRight) tag configure invalid -background #a9a9a9

    }
}

# Scroll text windows
proc scrollText {top n what} {
    # Do not scroll if focus is in a text window.
    # This is for scroll bindings in the toplevel.
    if {[winfo class [focus]] != "Text"} {
        $::widgets($top,wDiff1) yview scroll $n $what
    }
}

# Experiment using snit
if {[catch {package require snit}]} {
    namespace eval snit {
        proc widgetadaptor {args} {}
    }
}
# Emulate a label that:
# 1 : Displays the right part of the text if there isn't enough room
# 2 : Justfify text to the left if there is enough room.
# 3 : Does not try to allocate space according to its contents
::snit::widgetadaptor FileLabel {
    delegate method * to hull
    delegate option * to hull

    constructor {args} {
        eval label $self $args
        set fg [$self cget -foreground]
        set bg [$self cget -background]
        set font [$self cget -font]
        destroy $self
        installhull [entry $self -relief flat -bd 0 -highlightthickness 0 \
                             -foreground $fg -background $bg -font $font]
        $self configurelist $args
        $self configure -takefocus 0 -state readonly -readonlybackground $bg

        set var [$self cget -textvariable]
        if {$var != ""} {
            uplevel \#0 "[list trace variable $var w] \
		{[list after idle [mymethod xview end]] ;#}"
        }
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

    $w configure -takefocus 0 -state readonly -readonlybackground $bg

    set i [lsearch $args -textvariable]
    if {$i >= 0} {
	set var [lindex $args [expr {$i + 1}]]
	uplevel \#0 "trace variable $var w \
		{after idle {$w xview end} ;#}"
    }
}

# Fill in default data for a diff window
proc initDiffData {top} {
    set ::diff($top,leftOK) 0
    set ::diff($top,rightOK) 0
    set ::diff($top,mode) ""
    set ::diff($top,printFile) ""
    set ::diff($top,mergeFile) ""
    set ::diff($top,conflictFile) ""
    set ::diff($top,limitlines) 0
}

# Create a new diff window and diff two files
proc newDiff {file1 file2 {range {}}} {
    set top [makeDiffWin]
    update

    set ::diff($top,leftDir) [file dirname $file1]
    set ::diff($top,leftFile) $file1
    set ::diff($top,leftLabel) $file1
    set ::diff($top,leftOK) 1
    set ::diff($top,rightDir) [file dirname $file2]
    set ::diff($top,rightFile) $file2
    set ::diff($top,rightLabel) $file2
    set ::diff($top,rightOK) 1
    set ::diff($top,mode) ""
    set ::diff($top,range) $range
    wm deiconify $top
    raise $top
    update
    doDiff $top
}

# Create a new diff window equal to another, except for possibly a range
proc cloneDiff {other {range {}}} {
    set top [makeDiffWin]
    update

    foreach item [array names ::diff $other,*] {
        regsub {^[^,]*,} $item {} item
        set ::diff($top,$item) $::diff($other,$item)
    }
    if {[llength $range] != 0} {
        set ::diff($top,range) $range
    }
    wm deiconify $top
    raise $top
    update
    doDiff $top
}

# A thing to easily get to debug mode
proc backDoor {a} {
    append ::eskil(backdoor) $a
    set ::eskil(backdoor) [string range $::eskil(backdoor) end-9 end]
    if {$::eskil(backdoor) eq "PeterDebug"} {
        set ::debug 1
        catch {console show}
        set ::eskil(backdoor) ""
    }
}

# Build the main window
proc makeDiffWin {{top {}}} {
    global Pref tcl_platform debug

    if {$top != "" && [winfo exists $top] && [winfo toplevel $top] eq $top} {
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

    wm title $top "Eskil"
    wm protocol $top WM_DELETE_WINDOW [list cleanupAndExit $top]

    frame $top.f
    grid $top.f -row 0 -columnspan 4 -sticky news

    if {$tcl_platform(platform) eq "windows"} {
        #frame $top.f.line -height 1 -bg SystemButtonHighlight
        #pack $top.f.line -side bottom -fill x
    }

    menubutton $top.mf -text "File" -underline 0 -menu $top.mf.m
    menu $top.mf.m
    $top.mf.m add command -label "Redo Diff" -underline 5 \
            -command [list doDiff $top] -state disabled
    if {$debug == 1} {
        $top.mf.m entryconfigure "Redo Diff" -state normal
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
    if {$tcl_platform(platform) eq "unix"} {
        $top.mf.m add command -label "RCSDiff..." -underline 0 \
                -command [list openRCS $top]
    }
    if {$::util(cvsExists)} {
        $top.mf.m add command -label "CVSDiff..." -underline 1 \
                -command [list openCVS $top]
    }
    $top.mf.m add separator
    $top.mf.m add command -label "Print..." -underline 0 \
            -command [list doPrint $top]
    $top.mf.m add separator
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
    $top.mo.m add cascade -label "Context" -underline 1 -menu $top.mo.mc
    if {$tcl_platform(platform) eq "windows"} {
        $top.mo.m add checkbutton -label "Force crlf translation" \
                -variable Pref(crlf)
    }
    $top.mo.m add separator
    $top.mo.m add command -label "Save default" \
            -command [list saveOptions $top]

    menu $top.mo.mf
    $top.mo.mf add command -label "Select..." -command makeFontWin -underline 0
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
    $top.mo.mi add radiobutton -label "Nothing" \
            -variable Pref(ignore) -value " "
    $top.mo.mi add radiobutton -label "Space changes (-b)" \
            -variable Pref(ignore) -value "-b"
    $top.mo.mi add radiobutton -label "All spaces (-w)" \
            -variable Pref(ignore) -value "-w"

    menu $top.mo.mp
    $top.mo.mp add radiobutton -label "Nothing" -variable Pref(parse) -value 0
    $top.mo.mp add radiobutton -label "Lines" -variable Pref(parse) -value 1
    $top.mo.mp add radiobutton -label "Blocks (small)" -variable Pref(parse) \
            -value 2
    $top.mo.mp add radiobutton -label "Blocks" -variable Pref(parse) -value 3
    $top.mo.mp add separator
    $top.mo.mp add radiobutton -label "Characters" \
            -variable Pref(lineparsewords) -value "0"
    $top.mo.mp add radiobutton -label "Words" \
            -variable Pref(lineparsewords) -value "1"
    $top.mo.mp add separator
    $top.mo.mp add checkbutton -label "Mark last" -variable Pref(marklast)

    menu $top.mo.mc
    $top.mo.mc add radiobutton -label "Show all lines" \
            -variable ::Pref(context) -value 0
    $top.mo.mc add separator
    $top.mo.mc add radiobutton -label "Context 2 lines" \
            -variable ::Pref(context) -value 2
    $top.mo.mc add radiobutton -label "Context 5 lines" \
            -variable ::Pref(context) -value 5
    $top.mo.mc add radiobutton -label "Context 10 lines" \
            -variable ::Pref(context) -value 10
    $top.mo.mc add radiobutton -label "Context 20 lines" \
            -variable ::Pref(context) -value 20

    menubutton $top.ms -text "Search" -underline 0 -menu $top.ms.m
    menu $top.ms.m
    if {[info procs textSearch::searchMenu] != ""} {
        textSearch::searchMenu $top.ms.m
    } else {
        $top.ms.m add command -label "Text search not available" \
                -state disabled
    }

    menubutton $top.mt -text "Tools" -underline 0 -menu $top.mt.m
    menu $top.mt.m
    $top.mt.m add command -label "New Diff Window" -underline 0 \
            -command makeDiffWin
    $top.mt.m add command -label "Directory Diff" -underline 0 \
            -command makeDirDiffWin
    $top.mt.m add command -label "Clip Diff" -underline 0 \
            -command makeClipDiffWin
    $top.mt.m add command -label "Merge" -underline 0 \
            -command [list makeMergeWin $top] -state disabled
    $top.mt.m add command -label "Clear Align" \
            -command [list clearAlign $top] -state disabled
    set ::widgets($top,enableAlignCmd) [list \
            $top.mt.m entryconfigure "Clear Align" -state normal]
    set ::widgets($top,disableAlignCmd) [list \
            $top.mt.m entryconfigure "Clear Align" -state disabled]

    if {$::tcl_platform(platform) eq "windows"} {
        if {![catch {package require registry}]} {
            $top.mt.m add separator
            $top.mt.m add command -label "Setup Registry" -underline 6 \
                    -command makeRegistryWin
        }
    }

    menubutton $top.mh -text "Help" -underline 0 -menu $top.mh.m
    menu $top.mh.m
    $top.mh.m add command -label "Help" -command makeHelpWin -underline 0
    $top.mh.m add command -label "Tutorial" -command makeTutorialWin \
            -underline 0
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
    text $top.ft1.tl -height $Pref(lines) -width 5 -wrap none \
            -font myfont -borderwidth 0 -padx 0 -highlightthickness 0 \
            -takefocus 0
    text $top.ft1.tt -height $Pref(lines) -width $Pref(linewidth) -wrap none \
            -xscrollcommand [list $top.sbx1 set] \
            -font myfont -borderwidth 0 -padx 1 \
            -highlightthickness 0
    frame $top.ft1.f -width 2 -height 2 -bg lightgray
    pack $top.ft1.tl -side left -fill y
    pack $top.ft1.f -side left -fill y
    pack $top.ft1.tt -side right -fill both -expand 1
    scrollbar $top.sby -orient vertical
    scrollbar $top.sbx1 -orient horizontal -command [list $top.ft1.tt xview]
    set ::widgets($top,wLine1) $top.ft1.tl
    set ::widgets($top,wDiff1) $top.ft1.tt

    frame $top.ft2 -borderwidth 2 -relief sunken
    text $top.ft2.tl -height $Pref(lines) -width 5 -wrap none \
            -font myfont -borderwidth 0 -padx 0 -highlightthickness 0 \
            -takefocus 0
    text $top.ft2.tt -height $Pref(lines) -width $Pref(linewidth) -wrap none \
            -xscrollcommand [list $top.sbx2 set] \
            -font myfont -borderwidth 0 -padx 1 \
            -highlightthickness 0
    frame $top.ft2.f -width 2 -height 2 -bg lightgray
    pack $top.ft2.tl -side left -fill y
    pack $top.ft2.f -side left -fill y
    pack $top.ft2.tt -side right -fill both -expand 1
    scrollbar $top.sbx2 -orient horizontal -command [list $top.ft2.tt xview]
    set ::widgets($top,wLine2) $top.ft2.tl
    set ::widgets($top,wDiff2) $top.ft2.tt
    commonYScroll $top.sby $top.ft1.tl $top.ft1.tt $top.ft2.tl $top.ft2.tt

    # Set up a tag for incremental search bindings
    if {[info procs textSearch::enableSearch] != ""} {
        textSearch::enableSearch $top.ft1.tt -label ::widgets($top,isearchLabel)
        textSearch::enableSearch $top.ft2.tt -label ::widgets($top,isearchLabel)
    }

    label $top.le -textvariable ::widgets($top,eqLabel) -width 1
    addBalloon $top.le "* means external diff is running.\n= means files do\
            not differ.\nBlank means files differ."
    label $top.ls -width 1 -pady 0 -padx 0 \
            -textvariable ::widgets($top,isearchLabel)
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
    bind $top.c <Destroy> [list image delete map$top]
    bind $top.c <Configure> [list drawMap $top %h]

    bind $top <Key-Up>    [list scrollText $top -1 u]
    bind $top <Key-Down>  [list scrollText $top  1 u]
    bind $top <Key-Prior> [list scrollText $top -1 p]
    bind $top <Key-Next>  [list scrollText $top  1 p]
    bind $top <Key-Escape> [list focus $top]
    if {$debug == 0} {
        bind $top <Key> "backDoor %A"
    }

    pack $top.mf $top.mo $top.ms $top.mt -in $top.f -side left -anchor n
    pack $top.mh -in $top.f -side left -anchor n
    pack $top.bfn -in $top.f -side right -padx {3 6}
    pack $top.bfp $top.er2 $top.lr2 $top.er1 $top.lr1 $top.eo $top.lo \
            -in $top.f -side right -padx 3
    if {$debug == 1} {
        menubutton $top.md -text "Debug" -menu $top.md.m -underline 0
        menu $top.md.m
        if {$tcl_platform(platform) eq "windows"} {
            $top.md.m add checkbutton -label "Console" -variable consolestate \
                    -onvalue show -offvalue hide \
                    -command {console $consolestate}
            $top.md.m add separator
        }
        $top.md.m add checkbutton -label "Wrap" -variable wrapstate \
                -onvalue char -offvalue none -command \
                "$top.ft1.tt configure -wrap \$wrapstate ;\
                $top.ft2.tt configure -wrap \$wrapstate"
        $top.md.m add command -label "Date Filter" \
                -command {set ::diff(filter) {^Date}}
        $top.md.m add separator
        $top.md.m add command -label "Reread Source" -underline 0 \
                -command {EskilRereadSource}
        $top.md.m add separator
        $top.md.m add command -label "Redraw Window" \
                -command [list makeDiffWin $top]
        $top.md.m add separator
        $top.md.m add command -label "Normal Cursor" \
                -command [list normalCursor $top]
        $top.md.m add separator
        $top.md.m add command -label "Evalstats" -command {evalstats}
        $top.md.m add command -label "_stats" -command {parray _stats}
        $top.md.m add command -label "Nuisance" -command [list makeNuisance \
                $top "It looks like you are trying out the debug menu."]
        pack $top.md -in $top.f -side left -padx 20 -anchor n
    }

    initDiffData $top
    return $top
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
    wm title .pr "Eskil Preferences"

    frame .pr.fc -borderwidth 1 -relief solid
    label .pr.fc.l1 -text "Colours" -anchor w
    label .pr.fc.l2 -text "Text" -anchor w
    label .pr.fc.l3 -text "Background" -anchor w

    entry .pr.fc.e1 -textvariable "TmpPref(colorchange)" -width 10
    entry .pr.fc.e2 -textvariable "TmpPref(colornew1)" -width 10
    entry .pr.fc.e3 -textvariable "TmpPref(colornew2)" -width 10

    button .pr.fc.b1 -text "Sel" -command "selColor colorchange"
    button .pr.fc.b2 -text "Sel" -command "selColor colornew1"
    button .pr.fc.b3 -text "Sel" -command "selColor colornew2"

    entry .pr.fc.e4 -textvariable "TmpPref(bgchange)" -width 10
    entry .pr.fc.e5 -textvariable "TmpPref(bgnew1)" -width 10
    entry .pr.fc.e6 -textvariable "TmpPref(bgnew2)" -width 10

    button .pr.fc.b4 -text "Sel" -command "selColor bgchange"
    button .pr.fc.b5 -text "Sel" -command "selColor bgnew1"
    button .pr.fc.b6 -text "Sel" -command "selColor bgnew2"

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
proc applyFont {lb} {
    global Pref TmpPref

    set Pref(fontsize) $TmpPref(fontsize)

    set i [lindex [$lb curselection] 0]
    set Pref(fontfamily) [$lb get $i]

    chFont
}

# Update example font
proc exampleFont {lb} {
    global TmpPref
    set i [lindex [$lb curselection] 0]
    if {$i eq ""} return
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
    toplevel .fo -padx 3 -pady 3
    wm title .fo "Select Font"

    label .fo.ltmp -text "Searching for fonts..."
    pack .fo.ltmp -padx {10 50} -pady {10 50}
    update

    catch {font delete tmpfont}
    font create tmpfont

    array set TmpPref [array get Pref]
    labelframe .fo.lf -text "Family" -padx 3 -pady 3
    set lb [Scroll y listbox .fo.lf.lb -width 15 -height 10 \
            -exportselection no -selectmode single]
    bind $lb <<ListboxSelect>> [list exampleFont $lb]
    pack .fo.lf.lb -fill both -expand 1

    labelframe .fo.ls -text "Size" -padx 3 -pady 3
    spinbox .fo.ls.sp -from 1 -to 30 -increment 1 -width 3 -state readonly \
            -textvariable TmpPref(fontsize) -command [list exampleFont $lb]
    pack .fo.ls.sp -fill both -expand 1

    label .fo.le -text "Example" -anchor w -font tmpfont -width 1
    button .fo.bo -text "Ok"    -padx 10 -command "applyFont $lb ; destroy .fo"
    button .fo.ba -text "Apply" -padx 10 -command "applyFont $lb"
    button .fo.bc -text "Close" -padx 10 -command "destroy .fo"

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

    grid .fo.lf .fo.ls -sticky news -padx 3 -pady 3
    grid x      .fo.le -sticky nwe  -padx 3 -pady 3
    grid x      .fo.bo -sticky we   -padx 3 -pady 3
    grid x      .fo.ba -sticky we   -padx 3 -pady 3
    grid x      .fo.bc -sticky we   -padx 3 -pady 3
    grid .fo.lf -sticky news -rowspan 5
    grid columnconfigure .fo 0 -weight 1
    grid rowconfigure .fo 1 -weight 1

    exampleFont $lb
}

#####################################
# Registry section
#####################################

proc makeRegistryFrame {w label key newvalue} {
    set old {}
    catch {set old [registry get $key {}]}

    set l [labelframe $w -text $label -padx 4 -pady 4]

    label $l.key1 -text "Key:"
    label $l.key2 -text $key
    label $l.old1 -text "Old value:"
    label $l.old2 -text $old
    label $l.new1 -text "New value:"
    label $l.new2 -text $newvalue

    button $l.change -text "Change" -width 10 -command \
            "[list registry set $key {} $newvalue] ; \
             [list $l.change configure -state disabled]"
    button $l.delete -text "Delete" -width 10 -command \
            "[list registry delete $key] ; \
             [list $l.delete configure -state disabled]"
    if {[string equal $newvalue $old]} {
        $l.change configure -state disabled
    }
    if {[string equal "" $old]} {
        $l.delete configure -state disabled
    }
    grid $l.key1 $l.key2 -     -sticky "w" -padx 4 -pady 4
    grid $l.old1 $l.old2 -     -sticky "w" -padx 4 -pady 4
    grid $l.new1 $l.new2 -     -sticky "w" -padx 4 -pady 4
    grid $l.delete - $l.change -sticky "w" -padx 4 -pady 4
    grid $l.change -sticky "e"
    grid columnconfigure $l 2 -weight 1
}

proc makeRegistryWin {} {
    global thisScript

    # Locate executable for this program
    set exe [info nameofexecutable]
    if {[regexp {^(.*wish)\d+\.exe$} $exe -> pre]} {
        set alt $pre.exe
        if {[file exists $alt]} {
            set a [tk_messageBox -icon question -title "Which Wish" -message \
                    "Would you prefer to use the executable\n\
                    \"$alt\"\ninstead of\n\
                    \"$exe\"\nin the registry settings?" -type yesno]
            if {$a eq "yes"} {
                set exe $alt
            }
        }
    }

    set top .reg
    destroy $top
    toplevel $top
    wm title $top "Register Eskil"

    # Registry keys

    #set keyg {HKEY_CLASSES_ROOT\Folder\shell\Grep\command}
    set keydd {HKEY_CLASSES_ROOT\Folder\shell\Diff\command}
    set keyd {HKEY_CLASSES_ROOT\*\shell\Diff\command}
    set keyc {HKEY_CLASSES_ROOT\*\shell\DiffC\command}
    set keye {HKEY_CLASSES_ROOT\*\shell\Emacs\command}

    # Are we in a starkit?
    if {[info exists ::starkit::topdir]} {
        # In a starpack ?
        set exe [file normalize $exe]
        if {[string equal [file normalize $::starkit::topdir] $exe]} {
            set myexe [list $exe]
        } else {
            set myexe [list $exe $::starkit::topdir]
        }
    } else {
        if {[regexp {wish\d+\.exe} $exe]} {
            set exe [file join [file dirname $exe] wish.exe]
            if {[file exists $exe]} {
                set myexe [list $exe]
            }
        }
        set myexe [list $exe $thisScript]
    }

    set valbase {}
    foreach item $myexe {
        lappend valbase \"[file nativename $item]\"
    }
    set valbase [join $valbase]

    set new "$valbase -browse \"%1\""
    makeRegistryFrame $top.d "Diff" $keyd $new

    set new "$valbase -o \"%1\" -conflict \"%1\""
    makeRegistryFrame $top.c "Diff Conflict" $keyc $new

    set new "$valbase \"%1\""
    makeRegistryFrame $top.dd "Directory Diff" $keydd $new

    pack $top.d $top.c $top.dd -side "top" -fill x -padx 4 -pady 4

    locateEditor ::util(editor)
    if {[string match "*runemacs.exe" $::util(editor)]} {
        # Set up emacs
        set newkey "\"[file nativename $::util(editor)]\" \"%1\""
        makeRegistryFrame $top.e "Emacs" $keye $newkey
        pack $top.e -side "top" -fill x -padx 4 -pady 4
    }

    button $top.close -text "Close" -width 10 -command [list destroy $top] \
            -default active
    pack $top.close -side bottom -pady 4
    bind $top <Key-Return> [list destroy $top]
    bind $top <Key-Escape> [list destroy $top]
}

#####################################
# Directory diff section
#####################################

# Compare file names
proc FStrCmp {s1 s2} {
    # On Unix filenames are case sensitive
    if {$::tcl_platform(platform) eq "unix"} {
	return [string compare $s1 $s2]
    }
    string compare -nocase $s1 $s2
}

# Sort file names
proc Fsort {l} {
    if {$::tcl_platform(platform) eq "unix"} {
	return [lsort $l]
    }
    # Case insensitive on windows
    lsort -dictionary $l
}

# Compare two files
proc compareFiles {file1 file2} {
    global Pref
    if {[catch {file stat $file1 stat1}]} {
        return 0
    }
    if {[catch {file stat $file2 stat2}]} {
        return 0
    }

    # Same type?
    if {[file isdirectory $file1] != [file isdirectory $file2]} {
	return 0
    }
    # If contents is not checked, same size is enough to be equal
    if {$stat1(size) == $stat2(size) && $Pref(comparelevel) == 0} {
        return 1
    }
    # Different size is enough when doing binary compare
    if {$stat1(size) != $stat2(size) && $Pref(comparelevel) eq "1b"} {
        return 0
    }
    # Same size and time is always considered equal
    if {$stat1(size) == $stat2(size) && $stat1(mtime) == $stat2(mtime)} {
	return 1
    }
    # Don't check further if contents should not be checked
    if {$Pref(comparelevel) == 0} {
        return 0
    }
    # Don't check further if any is a directory
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
            if {$Pref(comparelevel) eq "1b"} {
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
            set eq [expr {![catch {exec $::util(diffexe) \
                    "--ignore-matching-lines=RCS: @(\#) \$Id" \
                    $file1 $file2} differr]}]
        }
    }
    return $eq
}

# Display two files in the directory windows and set up info for
# interacting with them.
# diff: Do they differ.
# level: Depth in a recursive run.
# The values in infoFiles are:
# 1 = noLeft, 2 = noRight, 4 = left is dir, 8 = right is dir, 16 = diff
proc listFiles {df1 df2 diff level} {
    global dirdiff Pref

    # Optionally do not list directories.
    if {$Pref(nodir)} {
        if {$df1 != "" && [file isdirectory $df1] && \
                $df2 != "" && [file isdirectory $df2] } {
            return
        }
    }

    lappend dirdiff(leftFiles) $df1
    lappend dirdiff(rightFiles) $df2
    set info [expr {$diff? 16 : 0}]
    if {$df1 eq ""} {
        incr info 1
    }
    if {$df2 eq ""} {
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

    set maptag [expr {$diff ? "change" : ""}]
    if {$df1 eq ""} {
	set tag2 new2
        set maptag new2
    } elseif {!$diff} {
	set tag2 ""
    } else {
        if {$info & 8} {
            set tag2 changed
        } else {
            set tag2 change
        }
    }

    if {$df2 eq ""} {
	set tag1 new1
        set maptag new1
    } elseif {!$diff} {
	set tag1 ""
    } else {
        if {$info & 4} {
            set tag1 changed
        } else {
            set tag1 change
        }
    }
    addChange .dirdiff 1 $maptag 0 0 0 0

    if {$df2 eq ""} {
	$dirdiff(wRight) insert end \n
    } else {
        if {[catch {set size [file size $df2]}]} {
            set size -1
            set mtime 0
            lappend tag2 invalid
        } else {
            set mtime [file mtime $df2]
        }
	$dirdiff(wRight) insert end [format "%-30s %8d %16s\n" \
                $f2 $size \
		[clock format $mtime -format "%Y-%m-%d %H:%M"]] \
		$tag2
    }
    if {$df1 eq ""} {
	$dirdiff(wLeft) insert end \n
    } else {
        if {[catch {set size [file size $df1]}]} {
            set size -1
            set mtime 0
            lappend tag1 invalid
        } else {
            set mtime [file mtime $df1]
        }
	$dirdiff(wLeft) insert end [format "%-30s %8d %16s\n" \
                $f1 $size \
		[clock format $mtime -format "%Y-%m-%d %H:%M"]] \
		$tag1
    }
    lappend dirdiff(infoFiles) $info
}

# Compare two directories.
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

# Run the directory comparison
proc doCompare {} {
    global dirdiff
    if {![file isdirectory $dirdiff(leftDir)]} return
    if {![file isdirectory $dirdiff(rightDir)]} return
    set dirdiff(leftFiles)  {}
    set dirdiff(rightFiles) {}
    set dirdiff(infoFiles)  {}
    set dirdiff(leftMark)   ""
    set dirdiff(rightMark)  ""

    $dirdiff(wLeft) delete 1.0 end
    $dirdiff(wRight) delete 1.0 end
    set top .dirdiff
    busyCursor $top
    clearMap $top
    update idletasks
    compareDirs $dirdiff(leftDir) $dirdiff(rightDir)
    normalCursor .dirdiff
    drawMap $top -1
}

# Pick a directory for compare
proc browseDir {dirVar entryW} {
    global Pref
    upvar "#0" $dirVar dir

    set newdir $dir
    while {$newdir != "." && ![file isdirectory $newdir]} {
        set newdir [file dirname $newdir]
    }
    set newdir [tk_chooseDirectory -initialdir $newdir -title "Select Directory"]
    if {$newdir != ""} {
        set dir $newdir
        $entryW xview end
    }
    if {$Pref(autocompare)} doCompare
}

# This is called when double clicking on a file in
# the directory compare window
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
        # Open a diff window for them
        newDiff $lf $rf
    }
}

# Bring up a context menu on a file.
proc rightClick {w x y X Y} {
    global dirdiff Pref

    set row [expr {int([$w index @$x,$y]) - 1}]
    set lf [lindex $dirdiff(leftFiles) $row]
    set rf [lindex $dirdiff(rightFiles) $row]
    set i [lindex $dirdiff(infoFiles) $row]

    set m .dirdiff.m
    destroy $m
    menu $m
    if {($i & 12) == 12} { # Both are dirs
        $m add command -label "Compare Directories" -command "
            [list set dirdiff(leftDir) $lf]
            [list set dirdiff(rightDir) $rf]
            [list if \$Pref(autocompare) "after idle doCompare"]
        "
    }
    if {$i & 4 && $w eq $dirdiff(wLeft)} { # Left is dir
        $m add command -label "Step down left directory" -command "
            [list set dirdiff(leftDir) $lf]
            [list if \$Pref(autocompare) "after idle doCompare"]
        "
    }
    if {$i & 8 && $w eq $dirdiff(wRight)} { # Right is dir
        $m add command -label "Step down right directory" -command "
            [list set dirdiff(rightDir) $rf]
            [list if \$Pref(autocompare) "after idle doCompare"]
        "
    }
    if {($i & 12) == 0 && ($i & 3) == 0} { # Neither is dir, Both exists
        $m add command -label "Compare Files" -command [list \
                newDiff $lf $rf]
    }
    if {$w eq $dirdiff(wLeft) && ($i & 13) == 0} {
        $m add command -label "Copy File" \
                -command [list copyFile $row right]
        $m add command -label "Edit File" \
                -command [list editFile $row left]
        $m add command -label "Mark File" \
                -command [list set ::dirdiff(leftMark) $lf]
	if {$::dirdiff(rightMark) != ""} {
	    $m add command -label "Compare with $::dirdiff(rightMark)" \
		    -command [list newDiff $lf $::dirdiff(rightMark)]
	}
    }
    if {$w eq $dirdiff(wRight) && ($i & 14) == 0} {
        $m add command -label "Copy File" \
                -command [list copyFile $row left]
        $m add command -label "Edit File" \
                -command [list editFile $row right]
        $m add command -label "Mark File" \
                -command [list set ::dirdiff(rightMark) $rf]
	if {$::dirdiff(leftMark) != ""} {
	    $m add command -label "Compare with $::dirdiff(leftMark)" \
		    -command [list newDiff $::dirdiff(leftMark) $rf]
	}
    }

    tk_popup $m $X $Y
}

# Copy a file from one directory to the other
proc copyFile {row to} {
    global dirdiff Pref

    if {$to eq "left"} {
        set src [lindex $dirdiff(rightFiles) $row]
        set n [expr {[string length $dirdiff(rightDir)] + 1}]
        set dst [file join $dirdiff(leftDir) [string range $src $n end]]
    } elseif {$to eq "right"} {
        set src [lindex $dirdiff(leftFiles) $row]
        set n [expr {[string length $dirdiff(leftDir)] + 1}]
        set dst [file join $dirdiff(rightDir) [string range $src $n end]]
    } else {
        error "Bad to argument to copyFile: $to"
    }

    if {[file exists $dst]} {
        if {[tk_messageBox -icon question -title "Overwrite file?" -message \
                "Copy\n$src\noverwriting\n$dst ?" -type yesno] eq "yes"} {
            file copy -force $src $dst
        }
    } else {
        if {[tk_messageBox -icon question -title "Copy file?" -message \
                "Copy\n$src\nto\n$dst ?" -type yesno] eq "yes"} {
            file copy $src $dst
        }
    }
}

# Bring up an editor to display a file.
proc editFile {row from} {
    global dirdiff Pref

    if {$from eq "left"} {
        set src [file join $dirdiff(leftDir) [lindex $dirdiff(leftFiles) $row]]
    } elseif {$from eq "right"} {
        set src [file join $dirdiff(rightDir) [lindex $dirdiff(rightFiles) $row]]
    } else {
        error "Bad from argument to editFile: $from"
    }

    locateEditor ::util(editor)
    exec $::util(editor) $src &
}

# Go up one level in directory hierarchy.
# 0 = both
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

# Create directory diff window.
proc makeDirDiffWin {{redraw 0}} {
    global Pref dirdiff

    set top .dirdiff
    if {[winfo exists $top] && [winfo toplevel $top] eq $top} {
        if {$redraw} {
            eval destroy [winfo children $top]
        } else {
            raise $top
            focus -force $top
            return
        }
    } else {
        destroy $top
        toplevel $top
        lappend ::diff(diffWindows) $top
    }

    wm title $top "Directory Diff"
    wm protocol $top WM_DELETE_WINDOW [list cleanupAndExit $top]

    frame $top.fm
    frame $top.fe1
    frame $top.fe2

    menubutton $top.mf -menu $top.mf.m -text "File" -underline 0
    menu $top.mf.m
    $top.mf.m add command -label "Close" -underline 0 \
            -command [list cleanupAndExit $top]
    $top.mf.m add separator
    $top.mf.m add command -label "Quit" -underline 0 \
            -command [list cleanupAndExit all]

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

    menubutton $top.mt -text "Tools" -underline 0 -menu $top.mt.m
    menu $top.mt.m
    $top.mt.m add command -label "New Diff Window" -underline 0 \
            -command makeDiffWin
    $top.mt.m add command -label "Clip Diff" -underline 0 \
            -command makeClipDiffWin
    if {$::tcl_platform(platform) eq "windows"} {
        if {![catch {package require registry}]} {
            $top.mt.m add separator
            $top.mt.m add command -label "Setup Registry" -underline 6 \
                    -command makeRegistryWin
        }
    }

    menubutton $top.mh -text "Help" -underline 0 -menu $top.mh.m
    menu $top.mh.m
    #$top.mh.m add command -label "Help" -command makeHelpWin -underline 0
    $top.mh.m add command -label "Tutorial" -command makeTutorialWin \
            -underline 0
    $top.mh.m add command -label "About" -command makeAboutWin -underline 0

    pack $top.mf $top.mo $top.mt $top.mh -in $top.fm -side left -anchor n

    if {$::debug} {
        menubutton $top.md -text "Debug" -menu $top.md.m -underline 0
        menu $top.md.m
        if {$::tcl_platform(platform) eq "windows"} {
            $top.md.m add checkbutton -label "Console" -variable consolestate \
                    -onvalue show -offvalue hide -command {console $consolestate}
            $top.md.m add separator
        }
        $top.md.m add command -label "Reread Source" -underline 0 \
                -command {EskilRereadSource}
        $top.md.m add separator
        $top.md.m add command -label "Redraw Window" -command {makeDirDiffWin 1}
        pack $top.md -in $top.fm -side left -padx 20 -anchor n
    }

    button $top.bu -text "Up Both" -command upDir -underline 0 -padx 10
    bind $top <Alt-u> "$top.bu invoke"
    button $top.bc -text "Compare" -command doCompare -underline 0 -padx 10
    bind $top <Alt-c> "$top.bc invoke"
    pack $top.bc $top.bu -in $top.fm -side right
    pack $top.bu -padx 6

    catch {font delete myfont}
    font create myfont -family $Pref(fontfamily) -size $Pref(fontsize)

    entry $top.e1 -textvariable dirdiff(leftDir)
    button $top.bu1 -text "Up" -padx 10 -command {upDir 1}
    button $top.bb1 -text "Browse"  -padx 10 \
            -command [list browseDir dirdiff(leftDir) $top.e1]
    $top.e1 xview end
    entry $top.e2 -textvariable dirdiff(rightDir)
    button $top.bu2 -text "Up" -padx 10 -command {upDir 2}
    button $top.bb2 -text "Browse" -padx 10 \
            -command [list browseDir dirdiff(rightDir) $top.e2]
    $top.e2 xview end
    bind $top.e1 <Return> doCompare
    bind $top.e2 <Return> doCompare

    pack $top.bb1 $top.bu1 -in $top.fe1 -side right -pady 1
    pack $top.bu1 -padx 6
    pack $top.e1 -in $top.fe1 -side left -fill x -expand 1
    pack $top.bb2 $top.bu2 -in $top.fe2 -side right -pady 1
    pack $top.bu2 -padx 6
    pack $top.e2 -in $top.fe2 -side left -fill x -expand 1

    text $top.t1 -height 40 -width 60 -wrap none -font myfont \
	    -xscrollcommand "$top.sbx1 set" -takefocus 0
    scrollbar $top.sby -orient vertical
    scrollbar $top.sbx1 -orient horizontal -command "$top.t1 xview"
    text $top.t2 -height 40 -width 60 -wrap none -font myfont \
	    -xscrollcommand "$top.sbx2 set" -takefocus 0
    scrollbar $top.sbx2 -orient horizontal -command "$top.t2 xview"
    commonYScroll $top.sby $top.t1 $top.t2
    canvas $top.c -width 6 -bd 0 -selectborderwidth 0 -highlightthickness 0

    image create photo map$top
    $top.c create image 0 0 -anchor nw -image map$top
    bind $top.c <Destroy> [list image delete map$top]
    bind $top.c <Configure> [list drawMap $top %h]

    bind $top.t1 <Double-Button-1> "after idle selectFile $top.t1 %x %y"
    bind $top.t2 <Double-Button-1> "after idle selectFile $top.t2 %x %y"
    bind $top.t1 <Button-3> "rightClick $top.t1 %x %y %X %Y"
    bind $top.t2 <Button-3> "rightClick $top.t2 %x %y %X %Y"

    set dirdiff(wLeft)  $top.t1
    set dirdiff(wRight) $top.t2
    set dirdiff(wY) $top.sby
    # Interact better with diff by setting these
    set ::widgets($top,wDiff1) $top.t1
    set ::widgets($top,wDiff2) $top.t2

    applyColor

    grid $top.fm   - - -   -     -sticky we
    grid $top.fe1  x  x    $top.fe2  -sticky we
    grid $top.t1   $top.c $top.sby $top.t2   -sticky news
    grid $top.sbx1 x  x    $top.sbx2 -sticky we

    grid $top.c -pady [expr {[$top.sby cget -width] + 2}]

    grid rowconfigure    $top  2    -weight 1
    grid columnconfigure $top {0 3} -weight 1
}


#####################################
# Clip diff section
#####################################

proc doClipDiff {} {
    set f1 [tmpFile]
    set f2 [tmpFile]

    set ch [open $f1 w]
    puts $ch [string trimright [$::diff(wClip1) get 1.0 end] \n]
    close $ch
    set ch [open $f2 w]
    puts $ch [string trimright [$::diff(wClip2) get 1.0 end] \n]
    close $ch

    newDiff $f1 $f2
}

proc makeClipDiffWin {} {
    set top .clipdiff
    if {[winfo exists $top] && [winfo toplevel $top] eq $top} {
        raise $top
        focus -force $top
        return
    }
    destroy $top
    toplevel $top
    lappend diff(diffWindows) $top

    wm title $top "Clip Diff"
    wm protocol $top WM_DELETE_WINDOW "cleanupAndExit $top"
    set t1 [Scroll both \
            text $top.t1 -width 60 -height 35 -font myfont]
    set t2 [Scroll both \
            text $top.t2 -width 60 -height 35 -font myfont]

    set ::diff(wClip1) $t1
    set ::diff(wClip2) $t2

    bind $t1 <Control-o> [list focus $t2]
    bind $t2 <Control-o> [list focus $t1]

    frame $top.f
    menubutton $top.f.mf -menu $top.f.mf.m -text "File" -underline 0
    menu $top.f.mf.m
    $top.f.mf.m add command -label "Close" -underline 0 \
            -command [list cleanupAndExit $top]
    $top.f.mf.m add separator
    $top.f.mf.m add command -label "Quit" -underline 0 \
            -command [list cleanupAndExit all]

    button $top.f.b -text "Diff" -command doClipDiff -underline 0 -width 8
    bind $top <Alt-d> [list $top.f.b invoke]
    button $top.f.b2 -text "Left Clear" -command "$t1 delete 1.0 end" \
            -underline 0
    bind $top <Alt-l> "[list $top.f.b2 invoke] ; [list focus $t1]"

    button $top.f.b3 -text "Right Clear" -command "$t2 delete 1.0 end" \
            -underline 0
    bind $top <Alt-r> "[list $top.f.b3 invoke] ; [list focus $t2]"
    button $top.f.b4 -text "Left Clear&Paste" -command \
            "$t1 delete 1.0 end ; event generate $t1 <<Paste>>"
    button $top.f.b5 -text "Right Clear&Paste" -command \
            "$t2 delete 1.0 end ; event generate $t2 <<Paste>>"

    foreach w [list $top.f.b2 $top.f.b4 $top.f.b $top.f.b3 $top.f.b5] {
        raise $w
    }
    grid $top.f.mf $top.f.b2 $top.f.b4 x $top.f.b x $top.f.b3 $top.f.b5 x \
            -padx 4 -pady 2 -sticky "w"
    grid $top.f.mf -sticky nw -pady 0 -padx 0
    grid columnconfigure $top.f {0 3 5 8} -weight 1
    grid columnconfigure $top.f 8 -minsize [winfo reqwidth $top.f.mf]

    grid $top.f    -       -sticky we
    grid $top.t1   $top.t2 -sticky news
    grid $top.t2 -padx {2 0}
    grid rowconfigure    $top 1     -weight 1
    grid columnconfigure $top {0 1} -weight 1
}

#####################################
# Help and startup functions
#####################################

proc makeNuisance {top {str {Hi there!}}} {
    if {[lsearch [image names] nuisance] < 0} {
        set file [file join $::thisDir Nuisance.gif]
        if {![file exists $file]} return
        image create photo nuisance -file $file
    }

    destroy $top.nui
    toplevel $top.nui
    wm transient $top.nui $top
    wm geometry $top.nui +400+400
    wm title $top.nui ""
    label $top.nui.l -image nuisance
    pack $top.nui.l
    wm protocol $top.nui WM_DELETE_WINDOW [list destroy $top.nui2 $top.nui]
    update

    destroy $top.nui2
    toplevel $top.nui2 -bg yellow
    wm transient $top.nui2 $top.nui
    wm overrideredirect $top.nui2 1
    wm title $top.nui2 ""
    label $top.nui2.l -text "$str\nDo you want help?" -justify left -bg yellow
    button $top.nui2.b -text "No, get out of my face!" \
            -command [list destroy $top.nui2 $top.nui] -bg yellow
    pack $top.nui2.l $top.nui2.b -side "top" -fill x
    wm geometry $top.nui2 +[expr {405 + [winfo width $top.nui]}]+400
}

proc makeAboutWin {} {
    global diffver

    set w [helpWin .ab "About Eskil"]

    text $w.t -width 45 -height 11 -wrap none -relief flat \
            -bg [$w cget -bg]
    pack $w.t -side top -expand y -fill both

    $w.t insert end "A Tcl/Tk frontend to diff\n\n"
    $w.t insert end "$diffver\n\n"
    $w.t insert end "Made by Peter Spjuth\n"
    $w.t insert end "E-Mail: peter.spjuth@space.se\n"
    $w.t insert end "\nURL: http://spjuth.pointclark.net/Eskil.html\n"
    $w.t insert end "\nTcl version: [info patchlevel]\n"
    $w.t insert end "\nCredits:\n"
    $w.t insert end "Ideas for scrollbar map and merge function\n"
    $w.t insert end "taken from TkDiff"

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

    set doc [file join $::thisDir doc/eskil.txt]
    if {![file exists $doc]} return

    set w [helpWin .he "Eskil Help"]

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

proc makeTutorialWin {} {
    global Pref

    set doc [file join $::thisDir doc/tutorial.txt]
    if {![file exists $doc]} return

    if {[catch {cd [file join $::thisDir examples]}]} {
        tk_messageBox -icon error -title "Eskil Error" -message \
                "Could not locate examples directory." \
                -type ok
        return
    }
    #set ::diff(tutorial) 1

    # Start up a dirdiff in the examples directory
    set ::dirdiff(leftDir) [file join [pwd] dir1]
    set ::dirdiff(rightDir) [file join [pwd] dir2]
    makeDirDiffWin
    doCompare

    set w [helpWin .ht "Eskil Tutorial"]

    text $w.t -width 82 -height 35 -wrap word -yscrollcommand "$w.sb set"
    scrollbar $w.sb -orient vert -command "$w.t yview"
    pack $w.sb -side right -fill y
    pack $w.t -side left -expand 1 -fill both

    catch {font delete tutFont}
    catch {font delete tutFontB}
    #eval font create tutFont [font actual [$w.t cget -font]]
    font create tutFont -family Helvetica -size -14
    eval font create tutFontB [font configure tutFont] -weight bold
    $w.t configure -font tutFont

    # Move border properties to frame
    set bw [$w.t cget -borderwidth]
    set relief [$w.t cget -relief]
    $w configure -relief $relief -borderwidth $bw
    $w.t configure -borderwidth 0

    # Set up tags
    $w.t tag configure ul -underline 1
    $w.t tag configure b -font tutFontB

    insertTaggedText $w.t $doc
    $w.t configure -state disabled
}

proc printUsage {} {
    puts {Usage: eskil.tcl [options] [file1] [file2]
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
                command line to run diff, Eskil will do so unless
                this option is specified.
  -dir        : Start in directory diff mode. Ignores other args.
  -clip       : Start in clip diff mode. Ignores other args.

  -noparse    : Eskil can perform analysis of changed blocks to
  -line       : improve display. See online help for details.
  -smallblock : The default. Do block analysis on small blocks.
  -block      : Full block analysis. This can be slow if there
                are large change blocks.

  -char       : The analysis of changes can be done on either
  -word       : character or word basis. -char is the default.

  -noignore   : Don't ignore any whitespace.
  -b          : Ignore space changes. Default.
  -w          : Ignore all spaces.

  -conflict   : Treat file as a merge conflict file and enter merge
                mode.
  -o <file>   : Specify merge result output file.

  -browse     : Automatically bring up file dialog after starting.
  -server     : Set up Eskil to be controllable from the outside.

  -print <file> : Generate postscript and exit.

  -limit <lines> : Do not process more than <lines> lines.}
}

proc parseCommandLine {} {
    global dirdiff Pref

    if {$::eskil(argc) == 0} {
        makeDiffWin
        return
    }

    set noautodiff 0
    set autobrowse 0
    set dodir 0
    set doclip 0
    set files ""
    set nextArg ""
    set revNo 1
    foreach arg $::eskil(argv) {
        if {$nextArg != ""} {
            if {$nextArg eq "mergeFile"} {
                set opts(mergeFile) [file join [pwd] $arg]
            } elseif {$nextArg eq "printFile"} {
                set opts(printFile) [file join [pwd] $arg]
            } elseif {$nextArg eq "revision"} {
                set opts(doptrev$revNo) $arg
                incr revNo
            } elseif {$nextArg eq "limitlines"} {
                set opts(limitlines) $arg
            }
            set nextArg ""
            continue
        }
        if {$arg eq "-w"} {
            set Pref(ignore) "-w"
        } elseif {$arg eq "--help" || $arg eq "-help"} {
            printUsage
            exit
        } elseif {$arg eq "-b"} {
            set Pref(ignore) "-b"
        } elseif {$arg eq "-noignore"} {
            set Pref(ignore) " "
        } elseif {$arg eq "-noparse"} {
            set Pref(parse) 0
        } elseif {$arg eq "-line"} {
            set Pref(parse) 1
        } elseif {$arg eq "-smallblock"} {
            set Pref(parse) 2
        } elseif {$arg eq "-block"} {
            set Pref(parse) 3
        } elseif {$arg eq "-char"} {
            set Pref(lineparsewords) 0
        } elseif {$arg eq "-word"} {
            set Pref(lineparsewords) 1
        } elseif {$arg eq "-2nd"} {
            #set Pref(extralineparse) 1
        } elseif {$arg eq "-no2nd"} {
            #set Pref(extralineparse) 0
        } elseif {$arg eq "-limit"} {
            set nextArg limitlines
        } elseif {$arg eq "-nodiff"} {
            set noautodiff 1
        } elseif {$arg eq "-dir"} {
            set dodir 1
        } elseif {$arg eq "-clip"} {
            set doclip 1
        } elseif {$arg eq "-browse"} {
            set autobrowse 1
        } elseif {$arg eq "-conflict"} {
            set opts(mode) "conflict"
            set Pref(ignore) " "
        } elseif {$arg eq "-print"} {
            set nextArg printFile
        } elseif {$arg eq "-server"} {
            if {$::tcl_platform(platform) eq "windows"} {
                catch {
                    package require dde
                    dde servername Eskil
                }
            } else {
                tk appname Eskil
            }
        } elseif {$arg eq "-o"} {
            set nextArg mergeFile
        } elseif {$arg eq "-r"} {
            set nextArg revision
        } elseif {[string range $arg 0 1] eq "-r"} {
            set opts(doptrev$revNo) [string range $arg 2 end]
            incr revNo
        } elseif {[string range $arg 0 0] eq "-"} {
            append opts(dopt) " $arg"
        } else {
            set apa [file normalize [file join [pwd] $arg]]
            if {![file exists $apa]} {
                puts "Ignoring argument: $arg"
            } else {
                lappend files $apa
            }
        }
    }

    # Do we start in clip diff mode?
    if {$doclip} {
        makeClipDiffWin
        return
    }

    # Figure out if we start in a diff or dirdiff window.
    set len [llength $files]

    if {$len == 0 && $dodir} {
        set dirdiff(leftDir) [pwd]
        set dirdiff(rightDir) [pwd]
        makeDirDiffWin
        return
    }
    if {$len == 1} {
        set fullname [file join [pwd] [lindex $files 0]]
        if {[file isdirectory $fullname]} {
            set dirdiff(leftDir) $fullname
            set dirdiff(rightDir) $dirdiff(leftDir)
            makeDirDiffWin
            return
        }
    } elseif {$len >= 2} {
        set fullname1 [file join [pwd] [lindex $files 0]]
        set fullname2 [file join [pwd] [lindex $files 1]]
        if {[file isdirectory $fullname1] && [file isdirectory $fullname2]} {
            set dirdiff(leftDir) $fullname1
            set dirdiff(rightDir) $fullname2
            makeDirDiffWin
            after idle doCompare
            return
        }
    }

    # Ok, we have a normal diff
    makeDiffWin
    update
    set top [lindex $::diff(diffWindows) end]
    # Copy the previously collected options
    foreach {item val} [array get opts] {
        set ::diff($top,$item) $val
    }

    if {$len == 1} {
        set fullname [file join [pwd] [lindex $files 0]]
        set fulldir [file dirname $fullname]
        if {$::diff($top,mode) eq "conflict"} {
            set ::diff($top,conflictFile) $fullname
            set ::diff($top,rightDir) $fulldir
            set ::diff($top,rightOK) 1
            set ::diff($top,rightLabel) $fullname
            set ::diff($top,leftLabel) $fullname
            after idle [list doDiff $top]
            return
        }
        if {!$autobrowse} {
            # Check for revision control
            # RCS
            if {[llength [glob -nocomplain [file join $fulldir RCS]]]} {
                set ::diff($top,mode) "RCS"
                set ::diff($top,rightDir) $fulldir
                set ::diff($top,RCSFile) $fullname
                set ::diff($top,rightLabel) $fullname
                set ::diff($top,rightFile) $fullname
                set ::diff($top,rightOK) 1
                set ::diff($top,leftLabel) "RCS"
                if {$noautodiff} {
                    enableRedo $top
                } else {
                    after idle [list doDiff $top]
                }
                return
            }
            # CVS
            if {[llength [glob -nocomplain [file join $fulldir CVS]]]} {
                set ::diff($top,mode) "CVS"
                set ::diff($top,rightDir) $fulldir
                set ::diff($top,RCSFile) $fullname
                set ::diff($top,rightLabel) $fullname
                set ::diff($top,rightFile) $fullname
                set ::diff($top,rightOK) 1
                set ::diff($top,leftLabel) "CVS"
                if {$noautodiff} {
                    enableRedo $top
                } else {
                    after idle [list doDiff $top]
                }
                return
            }
            # ClearCase
            if {[auto_execok cleartool] != ""} {
                if {![catch {exec cleartool pwv -s} view] && \
                            $view != "** NONE **"} {
                    set ::diff($top,mode) "CT"
                    set ::diff($top,rightDir) $fulldir
                    set ::diff($top,RCSFile) $fullname
                    set ::diff($top,rightLabel) $fullname
                    set ::diff($top,rightFile) $fullname
                    set ::diff($top,rightOK) 1
                    set ::diff($top,leftLabel) "CT"
                    if {$noautodiff} {
                        enableRedo $top
                    } else {
                        after idle [list doDiff $top]
                    }
                    return
                }
            }
        }
        # No revision control. Is it a patch file?
        set ::diff($top,leftDir) $fulldir
        set ::diff($top,leftFile) $fullname
        set ::diff($top,leftLabel) $fullname
        set ::diff($top,leftOK) 1
        if {[regexp {\.(diff|patch)$} $fullname]} {
            set ::diff($top,mode) "patch"
            set ::diff($top,patchFile) $fullname
            set autobrowse 0
            if {$noautodiff} {
                enableRedo $top
            } else {
                after idle [list doDiff $top]
            }
            return
        }
    } elseif {$len >= 2} {
        set fullname [file join [pwd] [lindex $files 0]]
        set fulldir [file dirname $fullname]
        set ::diff($top,leftDir) $fulldir
        set ::diff($top,leftFile) $fullname
        set ::diff($top,leftLabel) $fullname
        set ::diff($top,leftOK) 1
        set fullname [file join [pwd] [lindex $files 1]]
        set fulldir [file dirname $fullname]
        set ::diff($top,rightDir) $fulldir
        set ::diff($top,rightFile) $fullname
        set ::diff($top,rightLabel) $fullname
        set ::diff($top,rightOK) 1
        if {$noautodiff} {
            enableRedo $top
        } else {
            after idle [list doDiff $top]
        }
    }
    if {$autobrowse && (!$::diff($top,leftOK) || !$::diff($top,rightOK))} {
        if {!$::diff($top,leftOK) && !$::diff($top,rightOK)} {
            openBoth $top 0
        } elseif {!$::diff($top,leftOK)} {
            openLeft $top
        } elseif {!$::diff($top,rightOK)} {
            openRight $top
        }
        # If we cancel the second file and detect CVS, ask about it.
        if {$::diff($top,leftOK) && !$::diff($top,rightOK) && \
                [llength [glob -nocomplain [file join $fulldir CVS]]]} {

            if {[tk_messageBox -title Diff -icon question \
                    -message "Do CVS diff?" -type yesno] eq "yes"} {
                set fulldir $::diff($top,leftDir)
                set fullname $::diff($top,leftFile)
                set ::diff($top,leftOK) 0
                set ::diff($top,mode) "CVS"
                set ::diff($top,rightDir) $fulldir
                set ::diff($top,RCSFile) $fullname
                set ::diff($top,rightLabel) $fullname
                set ::diff($top,rightFile) $fullname
                set ::diff($top,rightOK) 1
                set ::diff($top,leftLabel) "CVS"
                after idle [list doDiff $top]
            }
        }
    }
}

proc saveOptions {top} {
    global Pref

    # Check if the window size has changed
    set w $::widgets($top,wDiff1)
    if {[winfo reqwidth $w] != [winfo width $w] || \
            [winfo reqheight $w] != [winfo height $w]} {
        set dx [expr {[winfo width $w] - [winfo reqwidth $w]}]
        set dy [expr {[winfo height $w] - [winfo reqheight $w]}]
        set cx [font measure myfont 0]
        set cy [font metrics myfont -linespace]
        set neww [expr {[$w cget -width]  + $dx / $cx}]
        set newh [expr {[$w cget -height] + $dy / $cy}]
        if {$neww != $Pref(linewidth) || $newh != $Pref(lines)} {
            set apa [tk_messageBox -title "Save Preferences" -icon question \
                    -type yesno -message "Should I save the current window\
                    size with the preferences?\nCurrent: $neww x $newh  Old:\
                    $Pref(linewidth) x $Pref(lines)"]
            if {$apa == "yes"} {
                set Pref(linewidth) $neww
                set Pref(lines) $newh
            }
        }
    }

    set rcfile "~/.eskilrc"
    if {[catch {set ch [open $rcfile "w"]} err]} {
        tk_messageBox -icon error -title "File error" -message \
                "Error when trying to save preferences:\n$err"
        return
    }

    foreach i [array names Pref] {
        puts $ch [list set Pref($i) $Pref($i)]
    }
    close $ch

    tk_messageBox -icon info -title "Saved" -message \
            "Preferences saved to:\n[file nativename $rcfile]"
}

proc getOptions {} {
    global Pref

    set Pref(fontsize) 8
    set Pref(fontfamily) Courier
    set Pref(ignore) "-b"
    set Pref(parse) 2
    set Pref(lineparsewords) 0
    set Pref(colorchange) red
    set Pref(colornew1) darkgreen
    set Pref(colornew2) blue
    set Pref(bgchange) #ffe0e0
    set Pref(bgnew1) #a0ffa0
    set Pref(bgnew2) #e0e0ff
    set Pref(context) 0
    set Pref(crlf) 0
    set Pref(marklast) 1
    set Pref(linewidth) 80
    set Pref(lines) 60
    set Pref(editor) ""
    # Directory diff options
    set Pref(comparelevel) 1
    set Pref(recursive) 0
    set Pref(dir,onlydiffs) 0
    set Pref(nodir) 0
    set Pref(autocompare) 1
 
    # Backward compatibilty option
    set Pref(onlydiffs) -1
 
    set ::diff(filter) ""

    if {![info exists ::eskil_testsuite] && [file exists "~/.eskilrc"]} {
        safeLoad "~/.eskilrc" Pref
    }

    if {$Pref(editor) ne ""} {
        set ::util(editor) $Pref(editor)
    }

    # If the user's file has this old option, translate it to the new
    if {$Pref(onlydiffs) == 0} {
        set Pref(context) 0
    }
    unset Pref(onlydiffs)
}

proc defaultGuiOptions {} {
    catch {package require griffin}

    option add *Menu.tearOff 0
    if {[tk windowingsystem] eq "x11"} {
        option add *Menu.activeBorderWidth 1
        option add *Menu.borderWidth 1

        option add *Listbox.exportSelection 0
        option add *Listbox.borderWidth 1
        #option add *Listbox.highlightThickness 1
        option add *Font "Helvetica -12"
        option add *Scrollbar.highlightThickness 0
        option add *Scrollbar.takeFocus 0
    }

    if {$::tcl_platform(platform) eq "windows"} {
        option add *Panedwindow.sashRelief flat
        option add *Panedwindow.sashWidth 4
        option add *Panedwindow.sashPad 0
        #option add *Menubutton.activeBackground SystemHighlight
        #option add *Menubutton.activeForeground SystemHighlightText
        option add *Menubutton.padY 1
    }
}

if {![info exists gurkmeja]} {
    set gurkmeja 1
    defaultGuiOptions
    if {0 && [bind all <Alt-KeyPress>] eq ""} {
        bind all <Alt-KeyPress> [bind Menubutton <Alt-KeyPress>]
        #after 500 "tk_messageBox -message Miffo"
    }
    wm withdraw .
    getOptions
    if {![info exists ::eskil_testsuite]} {
        parseCommandLine
    }
}
