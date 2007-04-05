#!/bin/sh
#----------------------------------------------------------------------
#
#  Eskil, a Graphical frontend to diff
#
#  Copyright (c) 1998-2007, Peter Spjuth  (peter.spjuth@space.se)
#
#  Usage
#             Do 'eskil' for interactive mode
#             Do 'eskil --help' for command line usage
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

package require Tcl 8.4

# Stop Tk from meddling with the command line by copying it first.
set ::eskil(argv) $::argv
set ::eskil(argc) $::argc
set ::argv {}
set ::argc 0

set debug 0
set diffver "Version 2.2 2007-04-05"
set ::thisScript [file join [pwd] [info script]]

# Do initalisations for needed packages and globals.
# This is not run until needed to speed up command line error reporting.
proc Init {} {
    package require Tk 8.4
    catch {package require textSearch}

    package require wcb

    if {[catch {package require psballoon}]} {
        # Add a dummy if it does not exist.
        proc addBalloon {args} {}
    } else {
        namespace import -force psballoon::addBalloon
    }

    set ::thisDir [file dirname $::thisScript]

    # Follow any link
    set tmplink $::thisScript
    while {[file type $tmplink] eq "link"} {
        set tmplink [file readlink $tmplink]
        set tmplink [file normalize [file join $::thisDir $tmplink]]
        set ::thisDir [file dirname $tmplink]
    }

    # Get all other source files
    source $::thisDir/clip.tcl
    source $::thisDir/compare.tcl
    source $::thisDir/map.tcl
    source $::thisDir/merge.tcl
    source $::thisDir/registry.tcl
    source $::thisDir/dirdiff.tcl
    source $::thisDir/help.tcl
    source $::thisDir/printobj.tcl
    source $::thisDir/print.tcl
    source $::thisDir/rev.tcl

    set ::util(diffexe) diff

    # Diff functionality is in the DiffUtil package.
    package require DiffUtil
    # Help DiffUtil to find a diff executable, if needed
    catch {DiffUtil::LocateDiffExe $::thisScript}

    # Figure out a place to store temporary files.
    locateTmp ::diff(tmpdir)

    if {$::tcl_platform(platform) eq "windows"} {
        # Locate CVS if it is in c:/bin
        if {[auto_execok cvs] eq "" && [file exists "c:/bin/cvs.exe"]} {
            set ::env(PATH) "$::env(PATH);c:\\bin"
            auto_reset
        }
    }
    defaultGuiOptions
    if {0 && [bind all <Alt-KeyPress>] eq ""} {
        bind all <Alt-KeyPress> [bind Menubutton <Alt-KeyPress>]
        #after 500 "tk_messageBox -message Miffo"
    }
    wm withdraw .
}

# Debug function to be able to reread the source even when wrapped in a kit.
proc EskilRereadSource {} {
    set this $::thisScript

    # FIXA: Better detection of starkit?
    # Maybe look at ::starkit::topdir ?

    #if {[info exists ::starkit::topdir]} {
    #    puts "Topdir: $::starkit::topdir"
    #}

    # Are we in a Starkit?
    if {[regexp {^(.*eskil)((?:\.[^/]+)?)(/src/.*)$} $this -> \
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
            set i [lsearch $::widgets(toolbars) $top.f]
            if {$i >= 0} {
                set ::widgets(toolbars) [lreplace $::widgets(toolbars) $i $i]
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

# Get a name for a temporary file
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

# Delete temporary files
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

# Insert lineno and text
proc insertLine {top n line text {tag {}} {linetag {}}} {
    $::widgets($top,wDiff$n) insert end "$text\n" $tag
    if {$linetag ne ""} {
        append tag " $linetag"
    }
    if {$tag != ""} {
        set tag "hl$::HighLightCount $tag"
    }
    $::widgets($top,wLine$n) insert end [myFormL $line] $tag
}

# Insert an empty line on one side of the diff.
proc emptyLine {top n {highlight 1}} {
    if {$highlight} {
        $::widgets($top,wLine$n) insert end "\n" hl$::HighLightCount
    } else {
        $::widgets($top,wLine$n) insert end "*****\n"
    }
    $::widgets($top,wDiff$n) insert end "\n" padding
}

# Insert one line in each text widget.
# Mark them as changed, and optionally parse them.
proc insertMatchingLines {top line1 line2} {
    global doingLine1 doingLine2 Pref

    # FIXA: fully implement filter
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
        if {$Pref(nocase)} {lappend opts -nocase}
        if {$Pref(lineparsewords)} {lappend opts -words}
        set res [eval DiffUtil::diffStrings $opts \$line1 \$line2]
        set dotag 0
        set n [expr {[llength $res] / 2}]
        $::widgets($top,wLine1) insert end [myFormL $doingLine1] \
                "hl$::HighLightCount change"
        $::widgets($top,wLine2) insert end [myFormL $doingLine2] \
                "hl$::HighLightCount change"
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

# Detect if only newlines has changed within the block, e.g.
# when rearranging newlines.
# Rearranging newlines in comment blocks usually leads to
# words moving across "*", ignore * too.
# Returns 0 if the block in not handled here, non-zero if the block is done.
proc ParseBlocksAcrossNewline {top block1 block2} {
    global doingLine1 doingLine2

    set map {{ } {} \t {}}
    set RE {\n\s*\*?|\s}
    set equal 0
    set visible [expr {$::eskil(ignorenewline) == 1}]

    if 1 {
        set block1nospace [regsub -all $RE [join $block1 \n] {}]
        set block2nospace [regsub -all $RE [join $block2 \n] {}]
        if {$block1nospace eq $block2nospace} {
            set equal 1
        }
    } else {
        set block1nospace [string map $map [join $block1 ""]]
        set block2nospace [string map $map [join $block2 ""]]
        if {$block1nospace eq $block2nospace} {
            set equal 1
        } else {
            # Look for newlines rearranged in a comment block.
            set block1nostar [string map {* {}} $block1nospace]
            set block2nostar [string map {* {}} $block2nospace]
            if {$block1nostar eq $block2nostar} {
                set equal 1
            }
        }
    }
    if {!$equal} {
        return 0
    }

    if {$visible} {
        set tag change
    } else {
        set tag {}
    }
    # Just insert the blocks
    foreach line $block1 {
        insertLine $top 1 $doingLine1 $line {} $tag
        incr doingLine1
    }
    foreach line $block2 {
        insertLine $top 2 $doingLine2 $line {} $tag
        incr doingLine2
    }
    set n1 [llength $block1]
    set n2 [llength $block2]
    if {$n1 <= $n2} {
        for {set t $n1} {$t < $n2} {incr t} {
            if {$visible} {
                $::widgets($top,wDiff1) insert end "\n" "padding change"
                $::widgets($top,wLine1) insert end "\n" hl$::HighLightCount
            } else {
                emptyLine $top 1
            }
        }
    } elseif {$n2 < $n1} {
        if {$visible} {
            for {set t $n2} {$t < $n1} {incr t} {
                $::widgets($top,wDiff2) insert end "\n" "padding change"
                $::widgets($top,wLine2) insert end "\n" hl$::HighLightCount
            }
        } else {
            emptyLine $top 2
        }
    }
    if {$visible} {
        $::widgets($top,wDiff1) insert end "\n" "padding change"
        $::widgets($top,wDiff2) insert end "\n" "padding change"
        $::widgets($top,wLine1) insert end "\n" hl$::HighLightCount
        $::widgets($top,wLine2) insert end "\n" hl$::HighLightCount
        return [expr {($n1 > $n2 ? $n1 : $n2) + 1}]
    } else {
        return [expr {-($n1 > $n2 ? $n1 : $n2)}]
    }
}

# Insert two blocks of lines in the compare windows.
# Returns number of lines used to display the blocks
# Negative if the block should be viewed as equal
proc insertMatchingBlocks {top block1 block2} {
    global doingLine1 doingLine2

    # A large block may take time.  Give a small warning.
    if {[llength $block1] * [llength $block2] > 1000} {
        set ::widgets($top,eqLabel) "!"
        #puts "Eskil warning: Analyzing a large block. ($size1 $size2)"
        update idletasks
    }

    # Detect if only newlines has changed within the block, e.g.
    # when rearranging newlines.
    if {$::eskil(ignorenewline)} {
        set res [ParseBlocksAcrossNewline $top $block1 $block2]
        if {$res != 0} {
            return $res
        }
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
        if {$limit >= 0} {disallowEdit $top}

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
        disallowEdit $top
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
            if {$apa >= 0} {
                addChange $top $apa change $line1 $n1 $line2 $n2
            } else {
                addMapLines $top [expr {-$apa}]
                # In this case, a change is not visible
                return 1
            }
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
    # Empty return value
    return
}

proc enableRedo {top} {
    $top.m.mf entryconfigure "Redo Diff" -state normal
    $top.m.mt entryconfigure "Merge"     -state normal
}

proc disableRedo {top} {
    $top.m.mf entryconfigure "Redo Diff" -state disabled
    $top.m.mt entryconfigure "Merge"     -state disabled
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

#####################################
# Special cases.  Conflict/patch
#####################################

proc startConflictDiff {top file} {
    set ::diff($top,mode) "conflict"
    set ::diff($top,modetype) ""
    set ::diff($top,conflictFile) $file
    set ::diff($top,rightDir) [file dirname $file]
    set ::diff($top,rightOK) 1
    set ::diff($top,rightLabel) $file
    set ::diff($top,leftLabel) $file
    set ::diff($top,leftOK) 0

    # Turn off ignore
    set ::Pref(ignore) " "
    set ::Pref(nocase) 0
}

# Read a conflict file and extract the two versions.
proc prepareConflict {top} {
    global Pref

    disallowEdit $top
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
            lappend ::diff($top,conflictDiff) [list \
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
        if {[regexp {^\*\*\* } $line]} {
            if {$state eq "right"} {
                displayOnePatch $top $leftLines $rightLines $leftLine $rightLine
                set leftLines {}
                set rightLines {}
                set state none
            }
            if {$state eq "none"} {
                set state newfile
                set style c
                set leftRE {^\*\*\*\s+(.*)$}
                set rightRE {^---\s+(.*)$}
            }
        }
        # Detect the first line in a -u style diff
        if {[regexp {^--- } $line]} {
            if {$state eq "right" || $state eq "both"} {
                displayOnePatch $top $leftLines $rightLines $leftLine $rightLine
                set leftLines {}
                set rightLines {}
                set state none
            }
            if {$state eq "none"} {
                set state newfile
                set style u
                set leftRE {^---\s+(.*)$}
                set rightRE {^\+\+\+\s+(.*)$}
            }
        }
        if {$state eq "newfile" && [regexp $leftRE $line -> sub]} {
            emptyLine $top 1
            insertLine $top 1 "" $divider patch
            insertLine $top 1 "" $sub     patch
            insertLine $top 1 "" $divider patch
            addChange $top 4 change 0 0 0 0
            continue
        }
        if {$state eq "newfile" && [regexp $rightRE $line -> sub]} {
            emptyLine $top 2
            insertLine $top 2 "" $divider patch
            insertLine $top 2 "" $sub     patch
            insertLine $top 2 "" $divider patch
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

#####################################
# Main diff
#####################################

# Prepare for a diff by creating needed temporary files
proc prepareFiles {top} {
    set ::diff($top,cleanup) ""
    if {$::diff($top,mode) eq "rev"} {
        prepareRev $top
        set ::diff($top,cleanup) "rev"
    } elseif {$::diff($top,mode) eq "conflict"} {
        prepareConflict $top
        set ::diff($top,cleanup) "conflict"
    }
}

# Clean up after a diff
proc cleanupFiles {top} {
    switch $::diff($top,cleanup) {
        "rev"       {cleanupRev      $top}
        "conflict"  {cleanupConflict $top}
    }
}

# Redo Diff command
proc redoDiff {top} {
    # Note what rows are being displayed
    set w $::widgets($top,wDiff1)

    set width  [winfo width $w]
    set height [winfo height $w]

    set first [$w index @0,0]
    set last  [$w index @[expr {$width - 4}],[expr {$height - 4}]]
    
    set first [lindex [split $first .] 0]
    set last  [lindex [split $last  .] 0]

    # Narrow it 5 lines since seeText will try to view 5 lines extra
    incr first 5
    incr last -5
    if {$last < $first} {
        set last $first
    }

    doDiff $top

    # Restore view
    foreach item {wLine1 wDiff1 wLine2 wDiff2} {
        set w $::widgets($top,$item)
        seeText $w $first.0 $last.0
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
    resetEdit $top

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

    wm title $top "Eskil:"
    update idletasks

    if {$::diff($top,mode) eq "patch"} {
        disallowEdit $top
        displayPatch $top
        drawMap $top -1
        foreach item {wLine1 wLine2} {
            set w $::widgets($top,$item)
            $w configure -state disabled
        }
        update idletasks
        wm title $top "Eskil: [file tail $::diff($top,patchFile)]"
        $::widgets($top,wLine2) see 1.0
        normalCursor $top
        return
    } else {
        prepareFiles $top
    }

    set tail1 [file tail $::diff($top,rightLabel)]
    set tail2 [file tail $::diff($top,leftLabel)]
    if {$::diff($top,mode) ne "" || $tail1 eq $tail2} {
        if {$::diff($top,mode) eq "rev"} {
            set tail1 [file tail $::diff($top,RevFile)]
        } elseif {$::diff($top,mode) eq "conflict"} {
            set tail1 [file tail $::diff($top,conflictFile)]
        }
        wm title $top "Eskil: $tail1"
    } else {
        wm title $top "Eskil: $tail2 vs $tail1"
    }

    # Run diff and parse the result.
    set opts $Pref(ignore)
    if {$Pref(nocase)} {lappend opts -nocase}
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
    if {[llength $Pref(regsub)] > 0} {
        lappend opts -regsub $Pref(regsub)
    }
    # Apply nodigit after preprocess
    if {$Pref(nodigit)} {lappend opts -nodigit}

    set differr [catch {eval DiffUtil::diffFiles $opts \
            \$::diff($top,leftFile) \$::diff($top,rightFile)} diffres]

    # In conflict mode we can use the diff information collected when
    # parsing the conflict file. This makes sure the blocks in the conflict
    # file become change-blocks during merge.
    if {$::diff($top,mode) eq "conflict" && $::diff($top,modetype) eq "Pure"} {
        set diffres $::diff($top,conflictDiff)
    }

    if {$differr != 0} {
        $::widgets($top,wDiff1) insert end $diffres
        normalCursor $top
        return
    }
    if {[llength $diffres] == 0} {
        set ::widgets($top,eqLabel) "="
        # Automatically close if equal
        if {$::eskil(autoclose)} {
            after idle cleanupAndExit $top
            return
        }
    } else {
        set ::widgets($top,eqLabel) " "
    }
    # Update the equal label immediately for better feedback
    update idletasks

    set firstview 1

    set ch1 [open $::diff($top,leftFile)]
    set ch2 [open $::diff($top,rightFile)]
    set doingLine1 1
    set doingLine2 1

    # If there is a range, skip lines up to the range
    if {[llength $range] != 0} {
        disallowEdit $top
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
        set notvisible [doText $top $ch1 $ch2 $n1 $n2 $line1 $line2]
        if {$::diff($top,limitlines) && \
                ($::diff($top,mapMax) > $::diff($top,limitlines))} {
            break
        }
        if {$notvisible != 1} {
            bindHighlight $top
            incr ::HighLightCount
        }

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
            $w insert end \n padding
        }
    }

    close $ch1
    close $ch2

    # We can turn off editing in the text windows after everything
    # is displayed.
    noEdit $top

    # Mark aligned lines
    if {[info exists ::diff($top,aligns)] && \
            [llength $::diff($top,aligns)] > 0} {
        foreach {align1 align2} $::diff($top,aligns) {
            set i [$::widgets($top,wLine1) search -regexp "\\m$align1\\M" 1.0]
            if {$i != ""} {
                $::widgets($top,wLine1) tag add align \
                        "$i linestart" "$i lineend"
            }
            set i [$::widgets($top,wLine2) search -regexp "\\m$align2\\M" 1.0]
            if {$i != ""} {
                $::widgets($top,wLine2) tag add align \
                        "$i linestart" "$i lineend"
            }
        }
    }

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
    if {$::diff($top,mode) eq "conflict"} {
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
# Editing
#####################################

# FIXA: Use snit to adapt text widget instead of using wcb
# include seeText in such a snidget.

# Clear Editing state
proc resetEdit {top} {
    set ::diff($top,leftEdit) 0
    set ::diff($top,rightEdit) 0
    $top.m.mt entryconfigure "Edit Mode" -state normal

    resetEditW $::widgets($top,wDiff1)
    resetEditW $::widgets($top,wDiff2)
}

# Clear Editing state for a Text widget
proc resetEditW {w} {
    $w tag configure padding -background {}
    $w edit reset
    $w configure -undo 0

    set ::diff($w,allowChange) all

    wcb::callback $w before insert {}
    wcb::callback $w before delete {}
}

# Do not allow any editing
proc noEdit {top} {
    noEditW $::widgets($top,wDiff1)
    noEditW $::widgets($top,wDiff2)
}

# Do not allow any editing in a Text widget
proc noEditW {w} {
    set ::diff($w,allowChange) none

    wcb::callback $w before insert [list TextInterceptInsert $w]
    wcb::callback $w before delete [list TextInterceptDelete $w]
}

proc TextInterceptInsert {w ow index str args} {
    if {$::diff($w,allowChange) eq "none"} {
        wcb::cancel
        return
    }
    if {$::diff($w,allowChange) eq "all"} return

    #wcb::cancel - Cancel a widget command 
    #wcb::replace - Replace arguments of a widget command with new ones

    # Disallow all new lines
    if {[string first "\n" $str] >= 0} {
        wcb::cancel
        return
    }
    foreach {tag str2} $args {
        if {[string first "\n" $str2] >= 0} {
            wcb::cancel
            return
        }
    }
}

proc TextInterceptDelete {w ow from {to {}}} {
    if {$::diff($w,allowChange) eq "none"} {
        wcb::cancel
        return
    }
    if {$::diff($w,allowChange) eq "all"} return

    if {$to eq ""} {
        set to $from+1char
    }
    set text [$ow get $from $to]
    # Disallow all new lines
    if {[string first "\n" $text] >= 0} {
        wcb::cancel
        return
    }
}

# Turn on editing for a Text widget
proc turnOnEdit {w} {
    $w tag configure padding -background \#f0f0f0
    $w configure -undo 1

    set ::diff($w,allowChange) line
}

# Turn on editing on sides where it has not been disallowed
proc allowEdit {top} {
    $top.m.mt entryconfigure "Edit Mode" -state disable
    if {$::diff($top,leftEdit) == 0} {
        set ::diff($top,leftEdit) 1
        turnOnEdit $::widgets($top,wDiff1)
    }
    if {$::diff($top,rightEdit) == 0} {
        set ::diff($top,rightEdit) 1
        turnOnEdit $::widgets($top,wDiff2)
    }
}

# Turn off editing on sides that do not correspond to a file
proc disallowEdit {top {side 0}} {
    if {$side == 0 || $side == 1} {
        set ::diff($top,leftEdit) -1
    }
    if {$side == 0 || $side == 2} {
        set ::diff($top,rightEdit) -1
    }
    if {$::diff($top,leftEdit) == -1 && $::diff($top,rightEdit) == -1} {
        $top.m.mt entryconfigure "Edit Mode" -state disabled
    }
}

# Ask if editing is allowed on a side
proc mayEdit {top side} {
    if {$side == 1} {
        return [expr {$::diff($top,leftEdit) == 1}]
    } else {
        return [expr {$::diff($top,rightEdit) == 1}]
    }
}

# Start an undo block in a bunch of text widgets
proc startUndoBlock {args} {
    foreach w $args {
        $w configure -autoseparators 0
        # Open up editing for copy functions
        set ::diff($w,allowChange) all
    }
}

# End an undo block in a bunch of text widgets
proc endUndoBlock {args} {
    foreach w $args {
        $w configure -autoseparators 1
        $w edit separator
        set ::diff($w,allowChange) line
    }
}

# Copy a block
proc copyBlock {top from first last} {
    set to [expr {3 - $from}]

    set wfrom $::widgets($top,wDiff$from)
    set wto   $::widgets($top,wDiff$to)

    set tags ""
    set dump [$wfrom dump -all $first.0 $last.end+1c]

    startUndoBlock $wfrom $wto

    $wfrom delete $first.0 $last.end+1c
    $wto   delete $first.0 $last.end+1c

    foreach {key value index} $dump {
        switch -- $key {
            text {
                $wfrom insert $index $value $tags
                $wto   insert $index $value $tags
            }
            tagon {
                if {$value eq "padding"} {
                    set tags "padding"
                }
            }
            tagoff {
                if {$value eq "padding"} {
                    set tags 0
                }
            }
        }
    }
    endUndoBlock $wfrom $wto
}

# Copy a row between text widgets
proc copyRow {top from row} {
    set to [expr {3 - $from}]

    set wfrom $::widgets($top,wDiff$from)
    set wto   $::widgets($top,wDiff$to)

    set text [$wfrom get $row.0 $row.end+1c]

    startUndoBlock $wfrom $wto

    $wto delete $row.0 $row.end+1c
    $wto insert $row.0 $text ""
    # Rewrite the source row to remove any tags
    $wfrom delete $row.0 $row.end+1c
    $wfrom insert $row.0 $text ""

    endUndoBlock $wfrom $wto
}

# Delete a row filling it with padding
proc deleteBlock {top side from {to {}}} {
    set w $::widgets($top,wDiff$side)

    if {$to eq ""} {set to $from}
    startUndoBlock $w
    $w delete $from.0 $to.end+1c
    $w insert $from.0 [string repeat \n [expr {$to - $from + 1}]] padding
    endUndoBlock $w
}

# Get the lines involved in the display
proc getLinesFromRange {w range} {
    set from [lindex $range 0]
    set to   [lindex $range 1]
    foreach {fromr fromi} [split $from "."] break
    foreach {tor   toi}   [split $to   "."] break
    if {$toi == 0} {incr tor -1}

    # Get the corresponding lines in the file
    set t [$w get $fromr.0 $tor.end]
    set lines [lsort -integer [regexp -all -inline {\d+} $t]]
    set froml [lindex $lines 0]
    set tol [lindex $lines end]
    return [list $fromr $tor $froml $tol]
}

# Called by popup menus over row numbers to add commands for editing.
# Returns 1 if nothing was added.
proc editMenu {m top n hl x y} {

    if {![mayEdit $top $n]} {return 1}

    # Only copy when in a change block
    if {$hl ne ""} {
        set o [expr {3 - $n}]
        set editOther [mayEdit $top $o]

        set w $::widgets($top,wLine$n)
        set wo $::widgets($top,wLine$o)

        # Get the row that was clicked
        set index [$w index @$x,$y]
        set row [lindex [split $index "."] 0]

        set line  [regexp -inline {\d+} [$w  get $row.0 $row.end]]
        set lineo [regexp -inline {\d+} [$wo get $row.0 $row.end]]

        # Row copy
        if {$lineo ne ""} {
            $m add command -label "Copy Row from other side" \
                    -command [list copyRow $top $o $row]
        } else {
            $m add command -label "Delete Row" \
                    -command [list deleteBlock $top $n $row]
        }
        if {$line ne "" && $editOther} {
            $m add command -label "Copy Row to other side" \
                    -command [list copyRow $top $n $row]
        }

        # Get ranges for the change block
        set range  [$w tag ranges hl$hl]
        set rangeo [$wo tag ranges hl$hl]

        # Get the lines involved in the block
        foreach {from  to  froml  tol}  [getLinesFromRange $w  $range ] break
        foreach {fromo too fromlo tolo} [getLinesFromRange $wo $rangeo] break

        # More than one line in the block?
        set thisSize 0
        set otherSize 0
        if {$froml ne "" && $tol ne ""} {
            set thisSize [expr {$tol - $froml + 1}]
        }
        if {$fromlo ne "" && $tolo ne ""} {
            set otherSize [expr {$tolo - $fromlo + 1}]
        }
        if {$thisSize > 1 || $otherSize > 1} {
            if {$otherSize > 0} {
                $m add command -label "Copy Block from other side" \
                        -command [list copyBlock $top $o $fromo $too]
            } else {
                $m add command -label "Delete Block" \
                        -command [list deleteBlock $top $n $from $to]
            }
            if {$editOther && $thisSize > 0} {
                $m add command -label "Copy Block to other side" \
                        -command [list copyBlock $top $n $from $to]
            }
        }
    }

    $m add command -label "Save File" -command [list saveFile $top $n]

    return 0
}

proc saveFile {top side} {
    if {$side == 1} {
        if {!$::diff($top,leftEdit)} return
        set fileName $::diff($top,leftFile)
    } else {
        if {!$::diff($top,rightEdit)} return
        set fileName $::diff($top,rightFile)
    }

    set w $::widgets($top,wDiff$side)

    # Confirm dialog
    set apa [tk_messageBox -parent $top -icon question \
            -title "Overwrite file" -type yesnocancel -message \
            "Overwriting file [file tail $fileName]\nDo you want to\
            create a backup copy ?"]
    if {$apa eq "yes"} {
        set backup [file rootname $fileName].bak
        if {[catch {file copy -force $fileName $backup} result]} {
            tk_messageBox -parent $top -icon error \
                    -title "File error" -type ok -message \
                    "Error creating backup file $backup:\n$result"
            return
        }
    } elseif {$apa ne "no"} {
        return
    }

    set ch [open $fileName "w"]
    set save 1
    foreach {key value index} [$w dump -all 1.0 end-1c] {
        switch -- $key {
            text {
                if {$save} {
                    puts -nonewline $ch $value
                }
            }
            tagon {
                if {$value eq "padding"} {
                    set save 0
                }
            }
            tagoff {
                if {$value eq "padding"} {
                    set save 1
                }
            }
        }
    }
    close $ch
}

#####################################
# File dialog stuff
#####################################

# Check if a filename is a directory and handle starkits
proc FileIsDirectory {file} {
    # Skip directories
    if {[file isdirectory $file]} {return 1}

    # This detects .kit but how to detect starpacks?
    if {[file extension $file] eq ".kit"} {
        if {![catch {package require vfs::mk4}]} {
            vfs::mk4::Mount $file $file -readonly
            # Check for contents to ensure it is a kit
            if {[llength [glob -nocomplain $file/*]] == 0} {
                vfs::unmount $file
            }
        }
    }
    return [file isdirectory $file]
}

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
    if {!$forget && [info exists ::diff($top,leftDir)]} {
        set initDir $::diff($top,leftDir)
    } elseif {[info exists ::diff($top,rightDir)]} {
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
    if {!$forget && [info exists ::diff($top,rightDir)]} {
        set initDir $::diff($top,rightDir)
    } elseif {[info exists ::diff($top,leftDir)]} {
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
        startConflictDiff $top $::diff($top,rightFile)
        set ::diff($top,mergeFile) ""
        doDiff $top
    }
}

proc openPatch {top} {
    global Pref
    if {[doOpenLeft $top]} {
        set ::diff($top,mode) "patch"
        set Pref(ignore) " "
        set Pref(nocase) 0
        set ::diff($top,patchFile) $::diff($top,leftFile)
        doDiff $top
    }
}

proc openRev {top} {
    if {[doOpenRight $top]} {
        set rev [detectRevSystem $::diff($top,rightFile)]
        if {$rev eq ""} {
            tk_messageBox -icon error -title "Eskil Error" -message \
                    "Could not figure out which revison control system\
                    \"$::diff($top,rightFile)\" is under." -type ok
            return
        }
        startRevMode $top $rev $::diff($top,rightFile)
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

# Remove one or all alignment pairs
proc clearAlign {top {leftline {}}} {
    if {$leftline == ""} {
        set ::diff($top,aligns) {}
    } else {
        set i 0
        while 1 {
            set i [lsearch -integer -start $i $::diff($top,aligns) $leftline]
            if {$i < 0} break
            if {($i % 2) == 0} {
                set ::diff($top,aligns) [lreplace $::diff($top,aligns) \
                        $i [expr {$i + 1}]]
                break
            }
            incr i
        }
    }

    if {[llength $::diff($top,aligns)] == 0} {
        disableAlign $top
    }
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
    set cmd [list markAlign $top $n $line $text]
    if {![info exists ::diff($top,align$other)]} {
        set label "Mark line for alignment"
    } else {
        set label "Align with line $::diff($top,align$other) on other side"
    }

    if {[info exists ::diff($top,aligns)]} {
        foreach {align1 align2} $::diff($top,aligns) {
            if {$n == 1 && $line == $align1} {
                set label "Remove alignment with line $align2"
                set cmd [list clearAlign $top $align1]
            } elseif {$n == 2 && $line == $align2} {
                set label "Remove alignment with line $align1"
                set cmd [list clearAlign $top $align1]
            }
        }
    }

    $m add command -label $label -command $cmd

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

    if {![editMenu .lpm $top $n $hl $x $y]} {
        .lpm add separator
    }

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

# This is called when right clicking over the line numbers which are not
# marked for changes
proc rowPopup {w X Y x y} {
    set top [winfo toplevel $w]
    if {[info exists ::diff($top,nopopup)] && $::diff($top,nopopup)} return
    destroy .lpm
    menu .lpm

    regexp {(\d+)\D*$} $w -> n
    set tmp1 [editMenu  .lpm $top $n "" $x $y]
    if {!$tmp1} {.lpm add separator}
    set tmp2 [alignMenu .lpm $top $n $x $y]
    if {$tmp1 && $tmp2} {
        # Nothing in the menu
        return
    }
    if {!$tmp1 && $tmp2} {.lpm delete last}

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
        text $top.balloon.t$x -relief flat -font $font -bg \#ffffcc -fg black \
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

    wm title $top "Eskil:"
    wm protocol $top WM_DELETE_WINDOW [list cleanupAndExit $top]

    frame $top.f
    grid $top.f -row 0 -columnspan 4 -sticky nws
    lappend ::widgets(toolbars) $top.f
    if {!$::Pref(toolbar)} {
        grid remove $top.f
    }

    if {$tcl_platform(platform) eq "windows"} {
        #frame $top.f.line -height 1 -bg SystemButtonHighlight
        #pack $top.f.line -side bottom -fill x
    }

    menu $top.m
    $top configure -menu $top.m

    $top.m add cascade -label "File" -underline 0 -menu $top.m.mf
    menu $top.m.mf
    $top.m.mf add command -label "Redo Diff" -underline 5 \
            -command [list redoDiff $top] -state disabled
    if {$debug == 1} {
        $top.m.mf entryconfigure "Redo Diff" -state normal
    }
    $top.m.mf add separator
    $top.m.mf add command -label "Open Both..." -underline 0 \
            -command [list openBoth $top 0]
    $top.m.mf add command -label "Open Both (forget)..." \
            -command [list openBoth $top 1]
    $top.m.mf add command -label "Open Left File..." \
            -command [list openLeft $top]
    $top.m.mf add command -label "Open Right File..." \
            -command [list openRight $top]
    $top.m.mf add separator
    $top.m.mf add command -label "Open Conflict File..." \
            -command [list openConflict $top]
    $top.m.mf add command -label "Open Patch File..." \
            -command [list openPatch $top]
    $top.m.mf add command -label "Revision Diff..." -underline 0 \
            -command [list openRev $top]
    $top.m.mf add separator
    $top.m.mf add command -label "Print Ps..." -underline 0 \
            -command [list doPrint $top]
    $top.m.mf add command -label "Print Pdf..." \
            -command [list doPrint2 $top]
    $top.m.mf add separator
    $top.m.mf add command -label "Close" -underline 0 \
            -command [list cleanupAndExit $top]
    $top.m.mf add separator
    $top.m.mf add command -label "Quit" -underline 0 \
            -command {cleanupAndExit all}

    $top.m add cascade -label "Options" -underline 0 -menu $top.m.mo
    menu $top.m.mo
    $top.m.mo add cascade -label "Font" -underline 0 -menu $top.m.mo.f
    $top.m.mo add cascade -label "Ignore" -underline 0 -menu $top.m.mo.i
    $top.m.mo add command -label "Preprocess..." -underline 0 \
            -command [list EditPrefRegsub $top]
    $top.m.mo add cascade -label "Parse" -underline 1 -menu $top.m.mo.p
    $top.m.mo add command -label "Colours..." -underline 0 -command makePrefWin
    $top.m.mo add cascade -label "Context" -underline 1 -menu $top.m.mo.c
    $top.m.mo add separator
    $top.m.mo add checkbutton -label "Toolbar" -variable ::Pref(toolbar)
    $top.m.mo add separator
    $top.m.mo add command -label "Save default" \
            -command [list saveOptions $top]

    menu $top.m.mo.f
    $top.m.mo.f add command -label "Select..." -command makeFontWin \
            -underline 0
    $top.m.mo.f add radiobutton -label 6 -variable Pref(fontsize) -value 6 \
            -command chFont
    $top.m.mo.f add radiobutton -label 7 -variable Pref(fontsize) -value 7 \
            -command chFont
    $top.m.mo.f add radiobutton -label 8 -variable Pref(fontsize) -value 8 \
            -command chFont
    $top.m.mo.f add radiobutton -label 9 -variable Pref(fontsize) -value 9 \
            -command chFont
    $top.m.mo.f add radiobutton -label 10 -variable Pref(fontsize) -value 10 \
            -command chFont

    menu $top.m.mo.i
    $top.m.mo.i add radiobutton -label "No spaces" \
            -variable Pref(ignore) -value " "
    $top.m.mo.i add radiobutton -label "Space changes (-b)" \
            -variable Pref(ignore) -value "-b"
    $top.m.mo.i add radiobutton -label "All spaces (-w)" \
            -variable Pref(ignore) -value "-w"
    $top.m.mo.i add separator
    $top.m.mo.i add checkbutton -label "Case (-i)" \
            -variable Pref(nocase)
    $top.m.mo.i add checkbutton -label "Digits" \
            -variable Pref(nodigit)

    menu $top.m.mo.p
    $top.m.mo.p add radiobutton -label "Nothing" -variable Pref(parse) -value 0
    $top.m.mo.p add radiobutton -label "Lines" -variable Pref(parse) -value 1
    $top.m.mo.p add radiobutton -label "Blocks (small)" -variable Pref(parse) \
            -value 2
    $top.m.mo.p add radiobutton -label "Blocks" -variable Pref(parse) -value 3
    $top.m.mo.p add separator
    $top.m.mo.p add radiobutton -label "Characters" \
            -variable Pref(lineparsewords) -value "0"
    $top.m.mo.p add radiobutton -label "Words" \
            -variable Pref(lineparsewords) -value "1"
    $top.m.mo.p add separator
    $top.m.mo.p add checkbutton -label "Mark last" -variable Pref(marklast)

    menu $top.m.mo.c
    $top.m.mo.c add radiobutton -label "Show all lines" \
            -variable ::Pref(context) -value 0
    $top.m.mo.c add separator
    $top.m.mo.c add radiobutton -label "Context 2 lines" \
            -variable ::Pref(context) -value 2
    $top.m.mo.c add radiobutton -label "Context 5 lines" \
            -variable ::Pref(context) -value 5
    $top.m.mo.c add radiobutton -label "Context 10 lines" \
            -variable ::Pref(context) -value 10
    $top.m.mo.c add radiobutton -label "Context 20 lines" \
            -variable ::Pref(context) -value 20

    $top.m add cascade -label "Search" -underline 0 -menu $top.m.ms
    menu $top.m.ms
    if {[info procs textSearch::searchMenu] != ""} {
        textSearch::searchMenu $top.m.ms
    } else {
        $top.m.ms add command -label "Text search not available" \
                -state disabled
    }

    $top.m add cascade -label "Tools" -underline 0 -menu $top.m.mt
    menu $top.m.mt
    $top.m.mt add command -label "New Diff Window" -underline 0 \
            -command makeDiffWin
    $top.m.mt add command -label "Directory Diff" -underline 0 \
            -command makeDirDiffWin
    $top.m.mt add command -label "Clip Diff" -underline 0 \
            -command makeClipDiffWin
    $top.m.mt add command -label "Merge" -underline 0 \
            -command [list makeMergeWin $top] -state disabled
    $top.m.mt add command -label "Edit Mode" -underline 0 \
            -command [list allowEdit $top] -state disabled
    $top.m.mt add command -label "Clear Align" \
            -command [list clearAlign $top] -state disabled
    set ::widgets($top,enableAlignCmd) [list \
            $top.m.mt entryconfigure "Clear Align" -state normal]
    set ::widgets($top,disableAlignCmd) [list \
            $top.m.mt entryconfigure "Clear Align" -state disabled]

    if {$::tcl_platform(platform) eq "windows"} {
        if {![catch {package require registry}]} {
            $top.m.mt add separator
            $top.m.mt add command -label "Setup Registry" -underline 6 \
                    -command makeRegistryWin
        }
    }

    $top.m add cascade -label "Help" -underline 0 -menu $top.m.help
    menu $top.m.help
    $top.m.help add command -label "General" -command makeHelpWin -underline 0
    $top.m.help add command -label "Tutorial" -command makeTutorialWin \
            -underline 0
    foreach label {{Revision Control} {Edit Mode}} \
            file {revision.txt editmode.txt} {
        $top.m.help add command -label $label \
                -command [list makeDocWin $file] -underline 0
    }
    $top.m.help add separator
    $top.m.help add command -label "About" -command makeAboutWin -underline 0

    label $top.lr1 -text "Rev 1"
    addBalloon $top.lr1 "Revision number for CVS/RCS/ClearCase diff."
    entry $top.er1 -width 12 -textvariable diff($top,doptrev1)
    set ::widgets($top,rev1) $top.er1
    label $top.lr2 -text "Rev 2"
    addBalloon $top.lr2 "Revision number for CVS/RCS/ClearCase diff."
    entry $top.er2 -width 12 -textvariable diff($top,doptrev2)
    set ::widgets($top,rev2) $top.er2
    button $top.bcm -text Commit -padx 15 -command [list revCommit $top] \
            -state disabled -underline 0
    set ::widgets($top,commit) $top.bcm
    button $top.bfp -text "Prev Diff" -relief raised \
            -command [list findDiff $top -1] \
            -underline 0 -padx 15
    button $top.bfn -text "Next Diff" -relief raised \
            -command [list findDiff $top 1] \
            -underline 0 -padx 15
    bind $top <Alt-n> [list findDiff $top 1]
    bind $top <Alt-p> [list findDiff $top -1]
    bind $top <Alt-c> [list revCommit $top]

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
            not differ.\n! means a large block is being processed.\nBlank\
            means files differ."
    label $top.ls -width 1 -pady 0 -padx 0 \
            -textvariable ::widgets($top,isearchLabel)
    addBalloon $top.ls "Incremental search indicator"
    set map [createMap $top]

    applyColor
    foreach w [list $top.ft1.tt $top.ft2.tt] {
        # The last change in a row is underlined
        $w tag configure last -underline 1
        # Each file in a patch view starts with a block of this type
        $w tag configure patch -background gray
        # Make sure selection is visible
        $w tag raise sel
        bind $w <ButtonPress-3> "zoomRow %W %X %Y %x %y"
        bind $w <ButtonRelease-3> "unzoomRow %W"
    }
    foreach w [list $top.ft1.tl $top.ft2.tl] {
        $w tag configure align -underline 1
        bind $w <ButtonPress-3> "rowPopup %W %X %Y %x %y"
    }

    grid $top.l1   $top.le -        $top.l2   -row 1 -sticky news
    grid $top.ft1  $map    $top.sby $top.ft2  -row 2 -sticky news
    grid $top.sbx1 $top.ls -        $top.sbx2 -row 3 -sticky news
    grid columnconfigure $top {0 3} -weight 1
    grid rowconfigure $top 2 -weight 1
    grid $map -pady [expr {[$top.sby cget -width] + 2}]
    grid $top.ls -sticky ""

    bind $top <Key-Up>    [list scrollText $top -1 u]
    bind $top <Key-Down>  [list scrollText $top  1 u]
    bind $top <Key-Prior> [list scrollText $top -1 p]
    bind $top <Key-Next>  [list scrollText $top  1 p]
    bind $top <Key-Escape> [list focus $top]
    if {$debug == 0} {
        bind $top <Key> "backDoor %A"
    }

    pack $top.bfn -in $top.f -side right -padx {3 6}
    pack $top.bfp $top.bcm $top.er2 $top.lr2 $top.er1 $top.lr1 \
            -in $top.f -side right -padx 3
    if {$debug == 1} {
        $top.m add cascade -label "Debug" -menu $top.m.md -underline 0
        menu $top.m.md
        if {$tcl_platform(platform) eq "windows"} {
            $top.m.md add checkbutton -label "Console" -variable consolestate \
                    -onvalue show -offvalue hide \
                    -command {console $consolestate}
            $top.m.md add separator
        }
        $top.m.md add checkbutton -label "Wrap" -variable wrapstate \
                -onvalue char -offvalue none -command \
                "$top.ft1.tt configure -wrap \$wrapstate ;\
                $top.ft2.tt configure -wrap \$wrapstate"
        $top.m.md add command -label "Date Filter" \
                -command {set ::diff(filter) {^Date}}
        $top.m.md add separator
        $top.m.md add command -label "Reread Source" -underline 0 \
                -command {EskilRereadSource}
        $top.m.md add separator
        $top.m.md add command -label "Redraw Window" \
                -command [list makeDiffWin $top]
        $top.m.md add separator
        $top.m.md add command -label "Normal Cursor" \
                -command [list normalCursor $top]
        $top.m.md add separator
        $top.m.md add command -label "Evalstats" -command {evalstats}
        $top.m.md add command -label "_stats" -command {parray _stats}
        $top.m.md add command -label "Nuisance" -command [list makeNuisance \
                $top "It looks like you are trying out the debug menu."]
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

proc UpdateFontBox {lb} {
    $lb delete 0 end
    foreach {f fixed} $::FontCache {
        if {$fixed || !$::diff(fixedfont)} {
            $lb insert end $f
            if {[string equal -nocase $f $::Pref(fontfamily)]} {
                $lb selection set end
                $lb see end
            }
        }
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

    label .fo.le -text "Example\n0Ooi1Il" -anchor w -font tmpfont -width 1 \
            -justify left
    if {![info exists ::diff(fixedfont)]} {set ::diff(fixedfont) 1}
    checkbutton .fo.cb -text "Fixed" -variable ::diff(fixedfont) \
            -command [list UpdateFontBox $lb]
    button .fo.bo -text "Ok"    -padx 10 -command "applyFont $lb ; destroy .fo"
    button .fo.ba -text "Apply" -padx 10 -command "applyFont $lb"
    button .fo.bc -text "Close" -padx 10 -command "destroy .fo"

    if {![info exists FontCache]} {
        set fam [lsort -dictionary [font families]]
        font create testfont
        foreach f $fam {
            if {![string equal $f ""]} {
                font configure testfont -family $f
                lappend FontCache $f [font metrics testfont -fixed]
            }
        }
        font delete testfont
    }
    UpdateFontBox $lb

    destroy .fo.ltmp

    grid .fo.lf .fo.ls -sticky news -padx 3 -pady 3
    grid x      .fo.cb -sticky nwe  -padx 3 -pady 3
    grid x      .fo.bo -sticky we   -padx 3 -pady 3
    grid x      .fo.ba -sticky we   -padx 3 -pady 3
    grid x      .fo.bc -sticky we   -padx 3 -pady 3
    grid .fo.le -      -sticky nwe  -padx 3 -pady 3
    grid .fo.lf -sticky news -rowspan 5
    grid columnconfigure .fo 0 -weight 1
    grid rowconfigure .fo 1 -weight 1

    exampleFont $lb
}

###########################
# Editor for ::Pref(regsub)
###########################

proc EditPrefRegsubOk {top w} {
    set exa $::diff($top,prefregexa)

    set result {}
    for {set t 1} {[info exists ::diff($top,prefregexp$t)]} {incr t} {
        set RE $::diff($top,prefregexp$t)
        set Sub $::diff($top,prefregsub$t)
        if {$RE eq ""} continue

        if {[catch {regsub -all -- $RE $exa $Sub _} err]} {
            return
        }
        lappend result $RE $Sub
    }

    set ::Pref(regsub) $result
    destroy $w

    array unset ::diff $top,prefregexp*
    array unset ::diff $top,prefregsub*
}

proc EditPrefRegsubUpdate {top args} {
    set exa $::diff($top,prefregexa)
    set exa2 $::diff($top,prefregexa2)
    set ok $::widgets($top,prefRegsubOk)

    for {set t 1} {[info exists ::diff($top,prefregexp$t)]} {incr t} {
        set RE $::diff($top,prefregexp$t)
        set Sub $::diff($top,prefregsub$t)

        if {$RE eq ""} continue

        if {[catch {regsub -all -- $RE $exa $Sub result} err]} {
            set ::diff($top,prefregresult) "$t ERROR: $err"
            $ok configure -state disabled
            return
        } else {
            set exa $result
        }
        if {[catch {regsub -all -- $RE $exa2 $Sub result} err]} {
            set ::diff($top,prefregresult2) "$t ERROR: $err"
            $ok configure -state disabled
            return
        } else {
            set exa2 $result
        }
    }
    set ::diff($top,prefregresult2) $exa2
    set ::diff($top,prefregresult) $exa
    $ok configure -state normal
}

proc AddPrefRegsub {top parent} {
    for {set t 1} {[winfo exists $parent.fr$t]} {incr t} {
        #Empty
    }
    set w [frame $parent.fr$t -bd 2 -relief groove -padx 3 -pady 3]
    label $w.l1 -text "Regexp:" -anchor "w"
    entry $w.e1 -textvariable ::diff($top,prefregexp$t) -width 60
    label $w.l2 -text "Subst:" -anchor "w"
    entry $w.e2 -textvariable ::diff($top,prefregsub$t)

    grid $w.l1 $w.e1 -sticky we -padx 3 -pady 3
    grid $w.l2 $w.e2 -sticky we -padx 3 -pady 3
    grid columnconfigure $w 1 -weight 1

    pack $w -side "top" -fill x -padx 3 -pady 3

    trace add variable ::diff($top,prefregexp$t) write \
            [list EditPrefRegsubUpdate $top]
    trace add variable ::diff($top,prefregsub$t) write \
            [list EditPrefRegsubUpdate $top]
}

# Editor for ::Pref(regsub)
proc EditPrefRegsub {top} {
    set w $top.prefregsub

    if {[winfo exists $w] && [winfo toplevel $w] eq $w} {
        wm deiconify $w
        raise $w
        focus $w
    } else {
        toplevel $w -padx 3 -pady 3
        wm title $w "Preferences: Preprocess"
    }

    button $w.b -text "Add" -padx 15 -command [list AddPrefRegsub $top $w]

    # Result example part
    if {![info exists ::diff($top,prefregexa)]} {
        set ::diff($top,prefregexa) \
                "An example TextString FOR_REGSUB /* Comment */"
        set ::diff($top,prefregexa2) \
                "An example TextString FOR_REGSUB /* Comment */"
    }
    labelframe $w.res -text "Preprocessing result" -padx 3 -pady 3
    label $w.res.l3 -text "Example 1:" -anchor "w"
    entry $w.res.e3 -textvariable ::diff($top,prefregexa) -width 60
    label $w.res.l4 -text "Result 1:" -anchor "w"
    label $w.res.e4 -textvariable ::diff($top,prefregresult) \
            -anchor "w" -width 10
    label $w.res.l5 -text "Example 2:" -anchor "w"
    entry $w.res.e5 -textvariable ::diff($top,prefregexa2)
    label $w.res.l6 -text "Result 2:" -anchor "w"
    label $w.res.e6 -textvariable ::diff($top,prefregresult2) \
            -anchor "w" -width 10

    grid $w.res.l3 $w.res.e3 -sticky we -padx 3 -pady 3
    grid $w.res.l4 $w.res.e4 -sticky we -padx 3 -pady 3
    grid $w.res.l5 $w.res.e5 -sticky we -padx 3 -pady 3
    grid $w.res.l6 $w.res.e6 -sticky we -padx 3 -pady 3
    grid columnconfigure $w.res 1 -weight 1

    # Buttons
    frame $w.fb -padx 3 -pady 3
    button $w.fb.b1 -text "Ok"     -command [list EditPrefRegsubOk $top $w]
    button $w.fb.b2 -text "Cancel" -command [list destroy $w]
    set ::widgets($top,prefRegsubOk) $w.fb.b1

    grid $w.fb.b1 x $w.fb.b2 -sticky we
    grid columnconfigure $w.fb {0 2} -uniform a
    grid columnconfigure $w.fb 1 -weight 1

    # Top layout
    pack $w.b -side "top" -anchor "w" -padx 3 -pady 3
    pack $w.fb $w.res -side bottom -fill x -padx 3 -pady 3

    # Fill in existing
    set t 1
    foreach {RE Sub} $::Pref(regsub) {
        set ::diff($top,prefregexp$t) $RE
        set ::diff($top,prefregsub$t) $Sub
        AddPrefRegsub $top $w
        incr t
    }
    
    trace add variable ::diff($top,prefregexa) write \
            [list EditPrefRegsubUpdate $top]
    trace add variable ::diff($top,prefregexa2) write \
            [list EditPrefRegsubUpdate $top]
    EditPrefRegsubUpdate $top
}

proc defaultGuiOptions {} {
    catch {package require griffin}

    option add *Menu.tearOff 0
    option add *Button.padX 5
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

    # Use Tahoma 8 as default on Windows, which is the system default
    # on Win2K and WinXP.
    if { [tk windowingsystem] == "win32" } {
        set ASfont "Tahoma 8"
        option add *Button.font             $ASfont widgetDefault
        option add *Checkbutton.font        $ASfont widgetDefault
        option add *Label.font              $ASfont widgetDefault
        option add *Listbox.font            $ASfont widgetDefault
        option add *Menu.font               $ASfont widgetDefault
        option add *Menubutton.font         $ASfont widgetDefault
        option add *Message.font            $ASfont widgetDefault
        option add *Radiobutton.font        $ASfont widgetDefault
        option add *Spinbox.font            $ASfont widgetDefault
    }
}

#####################################
# Startup stuff
#####################################

proc printUsage {} {
    puts {Usage: eskil [options] [file1] [file2]
  [options]              See below.
  [file1],[file2]        Files to be compared
                         If no files are given, the program is
                         started anyway and you can select files
                         from within.
                         If only one file is given, the program
                         looks for version control of the file, and 
                         if found, runs in version control mode.
  Options:

  -nodiff     : Normally, if there are enough information on the
                command line to run diff, Eskil will do so unless
                this option is specified.
  -dir        : Start in directory diff mode. Ignores other args.
  -clip       : Start in clip diff mode. Ignores other args.
  -patch      : View patch file.
  -context <n>: Show only differences, with <n> lines of context.
  -foreach    : Open one diff window per file listed.
  -close      : Close windows with no changes.

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
  -nocase     : Ignore case changes.
  -nodigit    : Ignore digit changes.
  -nokeyword  : In directory diff, ignore $ Keywords: $
  -nonewline  : Try to ignore newline changes.
  -nonewline+ : Try to ignore newline changes, and don't display.

  -prefix <str> : Care mainly about words starting with "str".
  -preprocess <pair> : TBW

  -r <ver>    : Version info for version control mode.

  -conflict   : Treat file as a merge conflict file and enter merge
                mode.
  -o <file>   : Specify merge result output file.

  -browse     : Automatically bring up file dialog after starting.
  -server     : Set up Eskil to be controllable from the outside.

  -print <file> : Generate postscript and exit.

  -limit <lines> : Do not process more than <lines> lines.

To list all options matching a prefix, run 'eskil --query prefix'.
In tcsh use this line to get option completion:
complete eskil 'C/-/`eskil --query -`/'}
}

# Go through all command line arguments
proc parseCommandLine {} {
    global dirdiff Pref

    set ::eskil(autoclose) 0
    set ::eskil(ignorenewline) 0

    if {$::eskil(argc) == 0} {
        Init
        makeDiffWin
        return
    }
    
    set allOpts {
        -w --help -help -b -noignore -i -nocase -nodigit -nokeyword -prefix
        -noparse -line -smallblock -block -char -word -limit -nodiff -dir
        -clip -patch -browse -conflict -print -server -o -r -context
        -foreach -preprocess -close -nonewline
    }

    # If the first option is "--query", use it to ask about options.
    if {$::eskil(argc) == 2 && [lindex $::eskil(argv) 0] == "--query"} {
        set arg [lindex $::eskil(argv) 1]
        if {[lsearch -exact $allOpts $arg] < 0} {
            set match [lsearch -glob -all -inline $allOpts $arg*]
        } else {
            set match [list $arg]
        }
        puts [lsort -dictionary $match]
        exit
    }

    set noautodiff 0
    set autobrowse 0
    set dodir 0
    set doclip 0
    set files ""
    set nextArg ""
    set revNo 1
    set dopatch 0
    set foreach 0
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
            } elseif {$nextArg eq "context"} {
                set Pref(context) $arg
            } elseif {$nextArg eq "prefix"} {
                set RE [string map [list % $arg] {^.*?\m(%\w+).*$}]
                if {$Pref(nocase)} {
                    set RE "(?i)$RE"
                }
                lappend ::Pref(regsub) $RE {\1}
            } elseif {$nextArg eq "preprocess"} {
                if {[catch {llength $arg} len]} {

                } elseif {[llength $arg] % 2 == 1} {

                } else {
                    # FIXA: better validity check
                    foreach {RE sub} $arg {
                        lappend ::Pref(regsub) $RE $sub
                    }
                }
            }
            set nextArg ""
            continue
        }
        # Take care of the special case of RCS style -r<rev>
        if {[string range $arg 0 1] eq "-r" && [string length $arg] > 2} {
            set opts(doptrev$revNo) [string range $arg 2 end]
            incr revNo
            continue
        }
        # Try to see if it is an unique abbreviation of an option.
        # If not, let it fall through to the file check.
        if {[lsearch -exact $allOpts $arg] < 0} {
            set match [lsearch -glob -all -inline $allOpts $arg*]
            if {[llength $match] == 1} {
                set arg [lindex $match 0]
            }
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
        } elseif {$arg eq "-i"} {
            set Pref(nocase) 1
        } elseif {$arg eq "-nocase"} {
            set Pref(nocase) 1
        } elseif {$arg eq "-nodigit"} {
            set Pref(nodigit) 1
        } elseif {$arg eq "-nokeyword"} {
            set Pref(dir,ignorekey) 1
        } elseif {$arg eq "-prefix"} {
            set nextArg prefix
        } elseif {$arg eq "-preprocess"} {
            set nextArg preprocess
        } elseif {$arg eq "-context"} {
            set nextArg context
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
        } elseif {$arg eq "-2nd"} { # Deprecated
            #set Pref(extralineparse) 1
        } elseif {$arg eq "-no2nd"} { # Deprecated
            #set Pref(extralineparse) 0
        } elseif {$arg eq "-limit"} {
            set nextArg limitlines
        } elseif {$arg eq "-nodiff"} {
            set noautodiff 1
        } elseif {$arg eq "-dir"} {
            set dodir 1
        } elseif {$arg eq "-clip"} {
            set doclip 1
        } elseif {$arg eq "-patch"} {
            set dopatch 1
        } elseif {$arg eq "-browse"} {
            set autobrowse 1
        } elseif {$arg eq "-foreach"} {
            set foreach 1
        } elseif {$arg eq "-nonewline"} {
            set ::eskil(ignorenewline) 1
        } elseif {$arg eq "-nonewline+"} {
            set ::eskil(ignorenewline) 2
        } elseif {$arg eq "-close"} {
            set ::eskil(autoclose) 1
        } elseif {$arg eq "-conflict"} {
            set opts(mode) "conflict"
        } elseif {$arg eq "-print"} {
            set nextArg printFile
        } elseif {$arg eq "-server"} {
            if {$::tcl_platform(platform) eq "windows"} {
                catch {
                    package require dde
                    dde servername Eskil
                }
            } else {
                package require Tk
                tk appname Eskil
            }
        } elseif {$arg eq "-o"} {
            set nextArg mergeFile
        } elseif {$arg eq "-r"} {
            set nextArg revision
        } elseif {$arg eq "-debug"} {
            set ::debug 1
        } else {
            set apa [file normalize [file join [pwd] $arg]]
            if {![file exists $apa]} {
                puts "Bad argument: $arg"
                exit
            } else {
                lappend files $apa
            }
        }
    }

    Init

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
        if {[FileIsDirectory $fullname]} {
            set dirdiff(leftDir) $fullname
            set dirdiff(rightDir) $dirdiff(leftDir)
            makeDirDiffWin
            return
        }
    } elseif {$len >= 2} {
        set fullname1 [file join [pwd] [lindex $files 0]]
        set fullname2 [file join [pwd] [lindex $files 1]]
        if {[FileIsDirectory $fullname1] && [FileIsDirectory $fullname2]} {
            set dirdiff(leftDir) $fullname1
            set dirdiff(rightDir) $fullname2
            makeDirDiffWin
            after idle doDirCompare
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

    # It is preferable to see the end if the rev string is too long
    $::widgets($top,rev1) xview end
    $::widgets($top,rev2) xview end

    if {$len == 1 || $foreach} {
        set ReturnAfterLoop 0
        set first 1
        foreach file $files {
            if {$first} {
                set first 0
            } else {
                # Create new window for other files
                makeDiffWin
                update
                set top [lindex $::diff(diffWindows) end]
                # Copy the previously collected options
                foreach {item val} [array get opts] {
                    set ::diff($top,$item) $val
                }
                # It is preferable to see the end if the rev string is too long
                $::widgets($top,rev1) xview end
                $::widgets($top,rev2) xview end
            }
            set fullname [file join [pwd] $file]
            set fulldir [file dirname $fullname]
            if {$::diff($top,mode) eq "conflict"} {
                startConflictDiff $top $fullname
                after idle [list doDiff $top]
                set ReturnAfterLoop 1
                continue
            }
            if {!$autobrowse && !$dopatch} {
                # Check for revision control
                set rev [detectRevSystem $fullname]
                if {$rev ne ""} {
                    startRevMode $top $rev $fullname
                    if {$noautodiff} {
                        enableRedo $top
                    } else {
                        after idle [list doDiff $top]
                    }
                    set ReturnAfterLoop 1
                    continue
                }
            }
            # No revision control. Is it a patch file?
            set ::diff($top,leftDir) $fulldir
            set ::diff($top,leftFile) $fullname
            set ::diff($top,leftLabel) $fullname
            set ::diff($top,leftOK) 1
            if {$dopatch || [regexp {\.(diff|patch)$} $fullname]} {
                set ::diff($top,mode) "patch"
                set ::diff($top,patchFile) $fullname
                set autobrowse 0
                if {$noautodiff} {
                    enableRedo $top
                } else {
                    after idle [list doDiff $top]
                }
                set ReturnAfterLoop 1
                continue
            }
        }
        if {$ReturnAfterLoop} return
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
                set fullname $::diff($top,leftFile)
                set ::diff($top,leftOK) 0
                startRevMode $top "CVS" $fullname
                after idle [list doDiff $top]
            }
        }
    }
}

# Save options to file ~/.eskilrc
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
    set Pref(nocase) 0
    set Pref(nodigit) 0
    set Pref(parse) 2
    set Pref(lineparsewords) 0
    set Pref(colorchange) red
    set Pref(colornew1) darkgreen
    set Pref(colornew2) blue
    set Pref(bgchange) \#ffe0e0
    set Pref(bgnew1) \#a0ffa0
    set Pref(bgnew2) \#e0e0ff
    set Pref(context) 0
    set Pref(marklast) 1
    set Pref(linewidth) 80
    set Pref(lines) 60
    set Pref(editor) ""
    set Pref(regsub) {}
    set Pref(toolbar) 0
    
    # Print options
    set Pref(grayLevel1) 0.6
    set Pref(grayLevel2) 0.8
    set Pref(wideLines) 0
    set Pref(printHeaderSize) 10
    set Pref(printCharsPerLine) 80
    set Pref(printPaper) a4

    # Directory diff options
    set Pref(comparelevel) 1
    set Pref(dir,ignorekey) 0
    set Pref(recursive) 0
    set Pref(dir,onlydiffs) 0
    set Pref(nodir) 0
    set Pref(autocompare) 1
    set Pref(dir,incfiles) ""
    set Pref(dir,exfiles) "*.o"
    set Pref(dir,incdirs) ""
    set Pref(dir,exdirs) "CVS"
    set Pref(dir,onlyrev) 0
    

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

    # Set up reactions to some Pref settings
    if {![info exists ::widgets(toolbars)]} {
        set ::widgets(toolbars) {}
    }
    # FIXA: Move to procs, handle destroyed windows.
    trace add variable ::Pref(toolbar) write "
        foreach __ \$::widgets(toolbars) {
            if {\$::Pref(toolbar)} {
                grid configure \$__
            } else {
                grid remove \$__
            }
        }
    \# "
}

# Global code is only run the first time to be able to reread source
if {![info exists gurkmeja]} {
    set gurkmeja 1
    package require pstools
    namespace import -force pstools::*
    getOptions
    if {![info exists ::eskil_testsuite]} {
        parseCommandLine
    }
}
