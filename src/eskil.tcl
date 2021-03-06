#!/bin/sh
#---------------------------------------------------------- -*- tcl -*-
#
#  Eskil, a Graphical frontend to diff
#
#  Copyright (c) 1998-2011, Peter Spjuth  (peter.spjuth@gmail.com)
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
# the next line restarts using tclsh \
exec tclsh "$0" "$@"

package require Tcl 8.5

# Stop Tk from meddling with the command line by copying it first.
set ::eskil(argv) $::argv
set ::eskil(argc) $::argc
set ::argv {}
set ::argc 0

set ::eskil(debug) 0
set ::eskil(diffver) "Version 2.5+ 2011-10-15"
set ::eskil(thisScript) [file join [pwd] [info script]]

namespace import tcl::mathop::+
namespace import tcl::mathop::-
namespace import tcl::mathop::*
namespace import tcl::mathop::/

# Do initalisations for needed packages and globals.
# This is not run until needed to speed up command line error reporting.
proc Init {} {
    package require Tk 8.4
    catch {package require textSearch}
    package require wcb
    package require snit
    package require tablelist_tile

    if {[catch {package require psballoon}]} {
        # Add a dummy if it does not exist.
        proc addBalloon {args} {}
    } else {
        namespace import -force psballoon::addBalloon
    }

    set ::eskil(thisDir) [file dirname $::eskil(thisScript)]

    # Follow any link
    set tmplink $::eskil(thisScript)
    while {[file type $tmplink] eq "link"} {
        set tmplink [file readlink $tmplink]
        set tmplink [file normalize [file join $::eskil(thisDir) $tmplink]]
        set ::eskil(thisDir) [file dirname $tmplink]
    }

    # Get all other source files
    source $::eskil(thisDir)/clip.tcl
    source $::eskil(thisDir)/compare.tcl
    source $::eskil(thisDir)/map.tcl
    source $::eskil(thisDir)/merge.tcl
    source $::eskil(thisDir)/registry.tcl
    source $::eskil(thisDir)/dirdiff.tcl
    source $::eskil(thisDir)/help.tcl
    source $::eskil(thisDir)/plugin.tcl
    source $::eskil(thisDir)/printobj.tcl
    source $::eskil(thisDir)/print.tcl
    source $::eskil(thisDir)/rev.tcl
    source $::eskil(thisDir)/debug.tcl

    # Diff functionality is in the DiffUtil package.
    package require DiffUtil
    # Help DiffUtil to find a diff executable, if needed
    catch {DiffUtil::LocateDiffExe $::eskil(thisScript)}

    # Figure out a place to store temporary files.
    locateTmp ::eskil(tmpdir)

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

    if {[catch {package require Ttk}]} {
        if {[catch {package require tile}]} {
            if {[info exists ::eskil_testsuite]} {
                return
            } else {
                puts "Themed Tk not found"
                exit
            }
        }
    }
    # Reportedly, the ttk scrollbar looks bad on Aqua
    if {[tk windowingsystem] ne "aqua"} {
        interp alias {} scrollbar {} ttk::scrollbar
    }
    # Provide a ttk-friendly toplevel, fixing background and menubar
    if {[info commands ttk::toplevel] eq ""} {
        proc ttk::toplevel {w args} {
            tk::toplevel $w {*}$args
            place [ttk::frame $w.tilebg] -x 0 -y 0 -relwidth 1 -relheight 1
            # Menubar looks out of place on linux. This adjusts the background
            # Which is enough to make it reasonable.
            set bg [ttk::style configure . -background]
            option add *Menubutton.background $bg
            option add *Menu.background $bg
            return $w
        }
    }

    ::snit::widgetadaptor ttk::entryX {
        delegate method * to hull
        delegate option * to hull

        constructor {args} {
            installhull using ttk::entry
            $self configurelist $args
            # Make sure textvariable is initialised
            set varName [from args -textvariable ""]
            if {$varName ne ""} {
                upvar \#0 $varName var
                if {![info exists var]} {
                    set var ""
                }
            }
        }
        # Circumvent a bug in ttk::entry that "xview end" does not work.
        method xview {args} {
            if {[llength $args] == 1} {
                set ix [lindex $args 0]
                $hull xview [$hull index $ix]
            } else {
                $hull xview {*}$args
            }
        }
    }

    interp alias {} toplevel {} ttk::toplevel

    # Use demo images from Tablelist
    set dir $::eskil(thisDir)/../lib/tablelist/demos
    if {[catch {
        set ::img(clsd) [image create photo -file [file join $dir clsdFolder.gif]]
        set ::img(open) [image create photo -file [file join $dir openFolder.gif]]
        set ::img(file) [image create photo -file [file join $dir file.gif]]
    }]} then {
        set ::img(clsd) ""
        set ::img(open) ""
        set ::img(file) ""
    }
    # Local images
    set dir $::eskil(thisDir)/images
    set ::img(link) [image create photo -file [file join $dir link.gif]]
    set ::img(left) [image create photo -file [file join $dir arrow_left.gif]]
    set ::img(right) [image create photo -file [file join $dir arrow_right.gif]]
    set ::img(browse) [image create photo -file [file join $dir folderopen1.gif]]
    set ::img(up) [image create photo -file [file join $dir arrow_up.gif]]
    # Create a double up arrow
    set ih [image height $::img(up)]
    set iw [image width $::img(up)]
    set ::img(upup) [image create photo -height $ih -width [expr {2 * $iw}]]
    $::img(upup) copy $::img(up) -to 0 0 [expr {2 * $iw - 1}] [expr {$ih - 1}]

}

# Debug function to be able to reread the source even when wrapped in a kit.
proc EskilRereadSource {} {
    set this $::eskil(thisScript)

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
# If eskil is embedded, this should be used to close an eskil toplevel.
proc cleanupAndExit {top} {
    # A security thing to make sure we can exit.
    set cont 0
    if {[catch {
        if {$top != "all"} {
            set i [lsearch $::eskil(diffWindows) $top]
            if {$i >= 0} {
                set ::eskil(diffWindows) [lreplace $::eskil(diffWindows) $i $i]
            }
            set i [lsearch $::widgets(toolbars) $top.f]
            if {$i >= 0} {
                set ::widgets(toolbars) [lreplace $::widgets(toolbars) $i $i]
            }

            destroy $top
            array unset ::eskil $top,*

            # Any windows remaining?
            if {[llength $::eskil(diffWindows)] > 0} {
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

# If embedding, tell eskil about any other toplevel, then
# cleanupAndExit can be used to get rid of it.
proc eskilRegisterToplevel {top} {
    lappend ::eskil(diffWindows) $top
}

# Format a line number
proc myFormL {lineNo} {
    if {![string is integer -strict $lineNo]} {return "$lineNo\n"}
      return [format "%3d: \n" $lineNo]
}

# Get a name for a temporary file
# A tail can be given to make the file more recognisable.
proc tmpFile {{tail {}}} {
    if {[info exists ::tmpcnt]} {
        incr ::tmpcnt
    } else {
        set ::tmpcnt 0
    }
    set name "tmpd[pid]a$::tmpcnt"
    if {$tail ne ""} {
        append name " [file tail $tail]"
    }
    set name [file join $::eskil(tmpdir) $name]
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
            set i [lsearch -exact $::tmpfiles $f]
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
proc insertLine {top n line text {tag {equal}} {linetag {}}} {
    $::widgets($top,wDiff$n) insert end "$text\n" $tag
    if {$linetag ne ""} {
        append tag " $linetag"
    }
    if {$tag != "equal"} {
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
    if {$::eskil(filter) != ""} {
        if {[regexp $::eskil(filter) $line1]} {
            insertLine $top 1 $doingLine1 $line1
            insertLine $top 2 $doingLine2 $line2
            incr doingLine1
            incr doingLine2
            set ::eskil(filterflag) 1
            return
        }
        set ::eskil(filterflag) 0
    }

    if {$Pref(parse) != 0} {
        set opts $Pref(ignore)
        if {$Pref(nocase)} {lappend opts -nocase}
        if {$Pref(lineparsewords)} {lappend opts -words}
        set res [DiffUtil::diffStrings {*}$opts $line1 $line2]
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
# Returns 0 if the block in not handled here, non-zero if the block is done,
# negative if the block is considered not a change.
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
# No extra parsing at all.
proc insertMatchingBlocksNoParse {top block1 block2 line1 line2 details} {
    global doingLine1 doingLine2

    set n1 [llength $block1]
    set n2 [llength $block2]
    # Is this a change block, a delete block or an insert block?
    if {$n1 == 0} {set tag2 new2} else {set tag2 change}
    if {$n2 == 0} {set tag1 new1} else {set tag1 change}

    foreach line $block1 {
        insertLine $top 1 $doingLine1 $line $tag1
        incr doingLine1
    }
    foreach line $block2 {
        insertLine $top 2 $doingLine2 $line $tag2
        incr doingLine2
    }
    if {$n1 <= $n2} {
        for {set t $n1} {$t < $n2} {incr t} {
            emptyLine $top 1
        }
        addChange $top $n2 $tag2 $line1 $n1 $line2 $n2
        nextHighlight $top
    } elseif {$n2 < $n1} {
        for {set t $n2} {$t < $n1} {incr t} {
            emptyLine $top 2
        }
        addChange $top $n1 $tag1 $line1 $n1 $line2 $n2
        nextHighlight $top
    }
}

# Insert two blocks of lines in the compare windows.
proc insertMatchingBlocks {top block1 block2 line1 line2 details} {
    global doingLine1 doingLine2 Pref

    set n1 [llength $block1]
    set n2 [llength $block2]

    set large [expr {$n1 * $n2 > 1000}]

    if {$n1 == 0 || $n2 == 0 || $Pref(parse) < 2 || \
            ($large && $Pref(parse) < 3)} {
        # No extra parsing at all.
        insertMatchingBlocksNoParse $top $block1 $block2 $line1 $line2 $details
        return
    }

    # A large block may take time.  Give a small warning.
    if {$large} {
        set ::widgets($top,eqLabel) "!"
        update idletasks
    }
    
    # Detect if only newlines has changed within the block, e.g.
    # when rearranging newlines.
    if {$::eskil(ignorenewline)} {
        set res [ParseBlocksAcrossNewline $top $block1 $block2]
        if {$res != 0} {
            # FIXA: move this to ParseBlocksAcrossNewline ?
            if {$res > 0 && $details} {
                addChange $top $res change $line1 $n1 $line2 $n2
                nextHighlight $top
            } else {
                addMapLines $top [expr {abs($res)}]
            }
            return
        }
    }

    set apa [compareBlocks $block1 $block2]
    # Fine grained changes means that each line is considered its own
    # chunk. This is used for merging better to avoid the same decision
    # for an entire block.
    set finegrain [expr {$::Pref(finegrainchunks) && $details}]

    if {$finegrain && $::eskil($top,ancestorFile) ne ""} {
        # Avoid fine grain depending on relation to ancestor
        set leftChange 0
        set leftChangeOrAdd 0
        for {set t $line1} {$t < $line1 + $n1} {incr t} {
            if {[info exists ::eskil($top,ancestorLeft,$t)]} {
                set leftChangeOrAdd 1
                if {$::eskil($top,ancestorLeft,$t) eq "c"} {
                    set leftChange 1
                    break
                }
            }
        }
        set rightChange 0
        set rightChangeOrAdd 0
        for {set t $line2} {$t < $line2 + $n2} {incr t} {
            if {[info exists ::eskil($top,ancestorRight,$t)]} {
                set rightChangeOrAdd 1
                if {$::eskil($top,ancestorRight,$t) eq "c"} {
                    set rightChange 1
                    break
                }
            }
        }
        # Avoid fine grain if either side has no changes against ancestor
        if {!$leftChangeOrAdd || !$rightChangeOrAdd} {
            set finegrain 0
        }
        # Avoid fine grain if both sides have at most additions
        if {!$leftChange && !$rightChange} {
            set finegrain 0
        }
    }

    set t1 0
    set t2 0
    foreach c $apa {
        if {$c eq "c"} {
            set textline1 [lindex $block1 $t1]
            set textline2 [lindex $block2 $t2]
            insertMatchingLines $top $textline1 $textline2
            if {$finegrain} {
                addChange $top 1 change [expr {$line1 + $t1}] 1 \
                        [expr {$line2 + $t2}] 1
                nextHighlight $top
            }
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
            if {$finegrain} {
                addChange $top 1 change [expr {$line1 + $t1}] 1 \
                        [expr {$line2 + $t2}] 1
                nextHighlight $top
            }
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
            if {$finegrain} {
                addChange $top 1 new1 [expr {$line1 + $t1}] 1 \
                        [expr {$line2 + $t2}] 0
                nextHighlight $top
            }
            incr t1
        } elseif {$c eq "a"} {
            set bepa [lindex $block2 $t2]
            $::widgets($top,wLine2) insert end [myFormL $doingLine2] \
                    "hl$::HighLightCount change"
            $::widgets($top,wDiff2) insert end "$bepa\n" new2
            emptyLine $top 1
            incr doingLine2
            if {$finegrain} {
                addChange $top 1 new2 [expr {$line1 + $t1}] 0 \
                        [expr {$line2 + $t2}] 1
                nextHighlight $top
            }
            incr t2
        }
    }
    if {!$finegrain} {
        if {$details} {
            addChange $top [llength $apa] change $line1 $n1 $line2 $n2
            nextHighlight $top
        } else {
            addMapLines $top [llength $apa]
        }
    }
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
        # If "show all" is not on, just display a couple of context lines.
        set limit -1
        if {$Pref(context) >= 0} {
            set limit $Pref(context)
        }
	# Consider any total limit on displayed lines.
        if {$::eskil($top,limitlines)} {
            set limit [expr {$::eskil($top,limitlines) - $::eskil($top,mapMax)}]
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

    # Is this a change block, a delete block or an insert block?
    if {$n1 == 0} {set tag2 new2} else {set tag2 change}
    if {$n2 == 0} {set tag1 new1} else {set tag1 change}

    # Display all equal lines before next diff
    # If only diff is on, only skip a section if the blank
    # line replaces at least 3 lines.
    set limit -1
    if {$Pref(context) >= 0 && \
            ($line1 - $doingLine1 > (2 * $Pref(context) + 2))} {
        set limit $Pref(context)
    }
    if {$doingLine1 == 1} {
        set allowStartFill 0
    } else {
        set allowStartFill 1
    }
    set t 0
    while {$doingLine1 < $line1} {
        gets $ch1 apa
        gets $ch2 bepa
        if {$limit < 0 || ($t < $limit && $allowStartFill) || \
                ($line1 - $doingLine1) <= $limit} {
            insertLine $top 1 $doingLine1 $apa
            insertLine $top 2 $doingLine2 $bepa
            addMapLines $top 1
        } elseif {$t == $limit && $allowStartFill} {
            # If zero context is shown, skip the filler to keep display tight.
            if {$limit > 0} {
                emptyLine $top 1 0
                emptyLine $top 2 0
                addMapLines $top 1
            }
        }
        incr doingLine1
        incr doingLine2
        incr t
        if {$::eskil($top,limitlines) && \
                ($::eskil($top,mapMax) > $::eskil($top,limitlines))} {
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
        if {$::eskil(filter) != "" &&  $::eskil(filterflag)} {
            addMapLines $top $n1
        } else {
            addChange $top $n1 change $line1 $n1 $line2 $n2
            nextHighlight $top
        }
    } else {
        # Collect blocks
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
        insertMatchingBlocks $top $block1 $block2 $line1 $line2 1
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
    set ::eskil($top,mode) "conflict"
    set ::eskil($top,modetype) ""
    set ::eskil($top,conflictFile) $file
    set ::eskil($top,rightDir) [file dirname $file]
    set ::eskil($top,rightOK) 1
    set ::eskil($top,rightLabel) $file
    set ::eskil($top,leftLabel) $file
    set ::eskil($top,leftOK) 0

    # Turn off ignore
    set ::Pref(ignore) " "
    set ::Pref(nocase) 0
    set ::Pref(noempty) 0

    # Try to autodetect line endings in file
    set ch [open $file rb]
    set data [read $ch 10000]
    close $ch
    if {[string first \r\n $data] >= 0} {
        set ::eskil($top,mergetranslation) crlf
    } else {
        set ::eskil($top,mergetranslation) lf
    }
}

# Read a conflict file and extract the two versions.
proc prepareConflict {top} {
    global Pref

    disallowEdit $top
    set ::eskil($top,leftFile) [tmpFile]
    set ::eskil($top,rightFile) [tmpFile]

    set ch1 [open $::eskil($top,leftFile) w]
    set ch2 [open $::eskil($top,rightFile) w]
    set ch [open $::eskil($top,conflictFile) r]

    set ::eskil($top,conflictDiff) {}
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
            lappend ::eskil($top,conflictDiff) [list \
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
        set leftName "No Conflict: [file tail $::eskil($top,conflictFile)]"
        set rightName $leftName
    }
    set ::eskil($top,leftLabel) $leftName
    set ::eskil($top,rightLabel) $rightName
    update idletasks
}

# Clean up after a conflict diff.
proc cleanupConflict {top} {
    global Pref

    clearTmp $::eskil($top,rightFile) $::eskil($top,leftFile)
    set ::eskil($top,rightFile) $::eskil($top,conflictFile)
    set ::eskil($top,leftFile) $::eskil($top,conflictFile)
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
        lassign [lindex $leftLines $leftc]   lline lmode lstr
        lassign [lindex $rightLines $rightc] rline rmode rstr

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
            insertMatchingBlocks $top $lblock $rblock $lblockl $rblockl 0
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
    # If the patch ended with a change block, display it.
    if {[llength $lblock] > 0 || [llength $rblock] > 0} {
        set ::doingLine1 $lblockl
        set ::doingLine2 $rblockl
        insertMatchingBlocks $top $lblock $rblock $lblockl $rblockl 0
        set lblock {}
        set rblock {}
    }
}

# Read a patch file and display it
proc displayPatch {top} {
    global Pref

    set ::eskil($top,leftLabel) "Patch $::eskil($top,patchFile): old"
    set ::eskil($top,rightLabel) "Patch $::eskil($top,patchFile): new"
    update idletasks

    if {$::eskil($top,patchFile) eq ""} {
        if {$::eskil($top,patchData) eq ""} {
            set data [getFullPatch $top]
        } else {
            set data $::eskil($top,patchData)
        }
    } elseif {$::eskil($top,patchFile) eq "-"} {
        set data [read stdin]
    } else {
        set ch [open $::eskil($top,patchFile) r]
        set data [read $ch]
        close $ch
    }

    set style ""
    set divider "-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-"

    set leftLine 1
    set rightLine 1
    set leftLines {}
    set rightLines {}
    set state none
    foreach line [split $data \n] {
        # Detect a new file
        if {[string match ======* $line] || [string match "diff *" $line]} {
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
        if {[regexp {^--- } $line] && $state eq "none"} {
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
            # Look for c function annotation in -u style
            if {[regexp {^@@.*@@(.*)$} $line -> cfun]} {
                set cfun [string trim $cfun]
                if {$cfun ne ""} {
                    insertLine $top 1 "" $cfun     patch
                    insertLine $top 2 "" $cfun     patch
                }
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
            # Look for c function annotation in -c style
            if {[regexp {^\*\*\*\*\*\S*\s+(.*)$} $line -> cfun]} {
                set cfun [string trim $cfun]
                if {$cfun ne ""} {
                    insertLine $top 1 "" $cfun     patch
                    insertLine $top 2 "" $cfun     patch
                }
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
}

#####################################
# Main diff
#####################################

# Prepare for a diff by creating needed temporary files
proc prepareFiles {top} {
    set ::eskil($top,cleanup) {}
    if {$::eskil($top,mode) eq "rev"} {
        prepareRev $top
        lappend ::eskil($top,cleanup) "rev"
    } elseif {$::eskil($top,mode) eq "conflict"} {
        prepareConflict $top
        lappend ::eskil($top,cleanup) "conflict"
    }
    if {$::eskil($top,plugin) ne ""} {
        preparePlugin $top
        set ::eskil($top,cleanup) "plugin $::eskil($top,cleanup)"
    }
}

# Clean up after a diff
proc cleanupFiles {top} {
    foreach keyword $::eskil($top,cleanup) {
        switch $keyword {
            "rev"      {cleanupRev      $top}
            "conflict" {cleanupConflict $top}
            "plugin"   {cleanupPlugin   $top}
        }
    }
    set ::eskil($top,cleanup) {}
}

# Redo Diff command
proc redoDiff {top} {
    # Note what rows are being displayed
    set w $::widgets($top,wDiff1)

    set width  [winfo width $w]
    set height [winfo height $w]

    set first [$w index @0,0]
    set last  [$w index @[- $width 4],[- $height 4]]

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

# Make an appropriate tail for a window title, depending on mode and files.
proc TitleTail {top} {
    set tail1 [file tail $::eskil($top,rightLabel)]
    set tail2 [file tail $::eskil($top,leftLabel)]
    if {$::eskil($top,mode) ne "" || $tail1 eq $tail2} {
        if {$::eskil($top,mode) eq "rev"} {
            set tail1 [file tail $::eskil($top,RevFile)]
        } elseif {$::eskil($top,mode) eq "conflict"} {
            set tail1 [file tail $::eskil($top,conflictFile)]
        }
        return $tail1
    } else {
        return "$tail2 vs $tail1"
    }
}

# Main diff function.
proc doDiff {top} {
    global Pref
    global doingLine1 doingLine2

    if {$::eskil($top,mode) eq "" && ($::eskil($top,leftOK) == 0 || $::eskil($top,rightOK) == 0)} {
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

    if {$::eskil($top,mode) eq "patch"} {
        disallowEdit $top
        displayPatch $top
        drawMap $top -1
        foreach item {wLine1 wLine2} {
            set w $::widgets($top,$item)
            $w configure -state disabled
        }
        update idletasks
        wm title $top "Eskil: [file tail $::eskil($top,patchFile)]"
        $::widgets($top,wLine2) see 1.0
        normalCursor $top
        return
    } else {
        prepareFiles $top
    }

    wm title $top "Eskil: [TitleTail $top]"

    # Run diff and parse the result.
    set opts $Pref(ignore)
    if {$Pref(nocase)} {lappend opts -nocase}
    if {$Pref(noempty)} {lappend opts -noempty}
    if {[info exists ::eskil($top,aligns)] && \
            [llength $::eskil($top,aligns)] > 0} {
        lappend opts -align $::eskil($top,aligns)
    }
    set range {}
    if {[info exists ::eskil($top,range)] && \
            [llength $::eskil($top,range)] == 4} {
        set range $::eskil($top,range)
        lappend opts -range $range
    }
    if {[llength $Pref(regsub)] > 0} {
        lappend opts -regsub $Pref(regsub)
    }
    # Apply nodigit after preprocess
    if {$Pref(nodigit)} {lappend opts -nodigit}

    # If a special file for diffing is present, use it.
    if {[info exists ::eskil($top,leftFileDiff)]} {
        set dFile1 $::eskil($top,leftFileDiff)
    } else {
        set dFile1 $::eskil($top,leftFile)
    }
    if {[info exists ::eskil($top,rightFileDiff)]} {
        set dFile2 $::eskil($top,rightFileDiff)
    } else {
        set dFile2 $::eskil($top,rightFile)
    }

    set differr [catch {DiffUtil::diffFiles {*}$opts \
            $dFile1 $dFile2} diffres]

    # In conflict mode we can use the diff information collected when
    # parsing the conflict file. This makes sure the blocks in the conflict
    # file become change-blocks during merge.
    if {$::eskil($top,mode) eq "conflict" && $::eskil($top,modetype) eq "Pure"} {
        set diffres $::eskil($top,conflictDiff)
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

    if {$::eskil($top,ancestorFile) ne ""} {
        collectAncestorInfo $top $dFile1 $dFile2 $opts
    }

    set firstview 1

    set ch1 [open $::eskil($top,leftFile)]
    set ch2 [open $::eskil($top,rightFile)]
    set doingLine1 1
    set doingLine2 1

    # If there is a range, skip lines up to the range
    if {[llength $range] != 0} {
        disallowEdit $top
        lassign $range start1 end1 start2 end2
        while {$doingLine1 < $start1 && [gets $ch1 line] >= 0} {
            incr doingLine1
        }
        while {$doingLine2 < $start2 && [gets $ch2 line] >= 0} {
            incr doingLine2
        }
    }

    set t 0
    foreach i $diffres {
        lassign $i line1 n1 line2 n2
        doText $top $ch1 $ch2 $n1 $n2 $line1 $line2
        if {$::eskil($top,limitlines) && \
                ($::eskil($top,mapMax) > $::eskil($top,limitlines))} {
            break
        }

        # Get one update when the screen has been filled.
        # Show the first diff.
        if {$firstview && $::eskil($top,mapMax) > 100} {
            set firstview 0
            showDiff $top 0
            update idletasks
        }
    }

    # If there is a range, just display the range
    if {[llength $range] != 0} {
        lassign $range start1 end1 start2 end2
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
    if {[info exists ::eskil($top,aligns)] && \
            [llength $::eskil($top,aligns)] > 0} {
        foreach {align1 align2} $::eskil($top,aligns) {
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
    if {$::eskil($top,mode) eq "conflict"} {
        if {$::widgets($top,eqLabel) != "="} {
            makeMergeWin $top
        }
    } elseif {$::eskil($top,ancestorFile) ne ""} {
        if {$::widgets($top,eqLabel) != "="} {
            makeMergeWin $top
        }
    }
    if {$::eskil($top,printFile) ne ""} {
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
    showDiff $top [expr {$::eskil($top,currHighLight) + $delta}]
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
    if {[info exists ::eskil($top,currHighLight)] && \
            $::eskil($top,currHighLight) >= 0} {
        $::widgets($top,wLine1) tag configure hl$::eskil($top,currHighLight) \
                -background {}
        $::widgets($top,wLine2) tag configure hl$::eskil($top,currHighLight) \
                -background {}
    }
    set ::eskil($top,currHighLight) $n
    if {$::eskil($top,currHighLight) < 0} {
        set ::eskil($top,currHighLight) -1
    } elseif {$::eskil($top,currHighLight) >= [llength $::eskil($top,changes)]} {
        set ::eskil($top,currHighLight) [llength $::eskil($top,changes)]
    } else {
        $::widgets($top,wLine1) tag configure hl$::eskil($top,currHighLight) \
                -background yellow
        $::widgets($top,wLine2) tag configure hl$::eskil($top,currHighLight) \
                -background yellow
    }
}

# Highlight a diff and scroll windows to it.
proc showDiff {top num} {
    highLightChange $top $num

    set change [lindex $::eskil($top,changes) $::eskil($top,currHighLight)]
    set line1 [lindex $change 0]

    if {$::eskil($top,currHighLight) < 0} {
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
    set ::eskil($top,leftEdit) 0
    set ::eskil($top,rightEdit) 0
    $top.m.mt entryconfigure "Edit Mode" -state normal

    resetEditW $::widgets($top,wDiff1)
    resetEditW $::widgets($top,wDiff2)
}

# Clear Editing state for a Text widget
proc resetEditW {w} {
    $w tag configure padding -background {}
    $w edit reset
    $w configure -undo 0

    set ::eskil($w,allowChange) all

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
    set ::eskil($w,allowChange) none

    wcb::callback $w before insert [list TextInterceptInsert $w]
    wcb::callback $w before delete [list TextInterceptDelete $w]
}

proc TextInterceptInsert {w ow index str args} {
    if {$::eskil($w,allowChange) eq "none"} {
        wcb::cancel
        return
    }
    if {$::eskil($w,allowChange) eq "all"} return

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
    if {$::eskil($w,allowChange) eq "none"} {
        wcb::cancel
        return
    }
    if {$::eskil($w,allowChange) eq "all"} return

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

    set ::eskil($w,allowChange) line
}

# Turn on editing on sides where it has not been disallowed
proc allowEdit {top} {
    $top.m.mt entryconfigure "Edit Mode" -state disable
    if {$::eskil($top,leftEdit) == 0} {
        set ::eskil($top,leftEdit) 1
        turnOnEdit $::widgets($top,wDiff1)
    }
    if {$::eskil($top,rightEdit) == 0} {
        set ::eskil($top,rightEdit) 1
        turnOnEdit $::widgets($top,wDiff2)
    }
}

# Turn off editing on sides that do not correspond to a file
proc disallowEdit {top {side 0}} {
    if {$side == 0 || $side == 1} {
        set ::eskil($top,leftEdit) -1
    }
    if {$side == 0 || $side == 2} {
        set ::eskil($top,rightEdit) -1
    }
    if {$::eskil($top,leftEdit) == -1 && $::eskil($top,rightEdit) == -1} {
        $top.m.mt entryconfigure "Edit Mode" -state disabled
    }
}

# Ask if editing is allowed on a side
proc mayEdit {top side} {
    if {$side == 1} {
        return [expr {$::eskil($top,leftEdit) == 1}]
    } else {
        return [expr {$::eskil($top,rightEdit) == 1}]
    }
}

# Start an undo block in a bunch of text widgets
proc startUndoBlock {args} {
    foreach w $args {
        $w configure -autoseparators 0
        # Open up editing for copy functions
        set ::eskil($w,allowChange) all
    }
}

# End an undo block in a bunch of text widgets
proc endUndoBlock {args} {
    foreach w $args {
        $w configure -autoseparators 1
        $w edit separator
        set ::eskil($w,allowChange) line
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
    lassign [split $from "."] fromr fromi
    lassign [split $to   "."] tor   toi
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
        lassign [getLinesFromRange $w  $range ] from  to  froml  tol
        lassign [getLinesFromRange $wo $rangeo] fromo too fromlo tolo

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
        if {!$::eskil($top,leftEdit)} return
        set fileName $::eskil($top,leftFile)
    } else {
        if {!$::eskil($top,rightEdit)} return
        set fileName $::eskil($top,rightFile)
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
proc FileIsDirectory {file {kitcheck 0}} {
    # Skip directories
    if {[file isdirectory $file]} {return 1}

    # This detects .kit but how to detect starpacks?
    if {[file extension $file] eq ".kit" || $kitcheck} {
        if {![catch {package require vfs::mk4}]} {
            if {![catch {vfs::mk4::Mount $file $file -readonly}]} {
                # Check for contents to ensure it is a kit
                if {[llength [glob -nocomplain $file/*]] == 0} {
                    vfs::unmount $file
                }
            }
        }
    }
    return [file isdirectory $file]
}

# A wrapper for tk_getOpenFile
proc myOpenFile {args} {
    array set opts $args
    set isVfs 0
    if {[info exists opts(-initialdir)]} {
        if {[string match tclvfs* [file system $opts(-initialdir)]]} {
            set isVfs 1
        }
    }
    # When in a vfs, make sure the Tcl file dialog is used
    # to be able to access the files in a starkit.
    if {$isVfs} {
        # Only do this if tk_getOpenFile is not a proc.
        if {[info procs tk_getOpenFile] eq ""} {
            # If there is any problem, call the real one
            if {![catch {set res [::tk::dialog::file:: open {*}$args]}]} {
                return $res
            }
        }
    }
    return [tk_getOpenFile {*}$args]
}

proc doOpenLeft {top {forget 0}} {
    if {!$forget && [info exists ::eskil($top,leftDir)]} {
        set initDir $::eskil($top,leftDir)
    } elseif {[info exists ::eskil($top,rightDir)]} {
        set initDir $::eskil($top,rightDir)
    } else {
        set initDir [pwd]
    }

    set apa [myOpenFile -title "Select left file" -initialdir $initDir \
            -parent $top]
    if {$apa != ""} {
        set ::eskil($top,leftDir) [file dirname $apa]
        set ::eskil($top,leftFile) $apa
        set ::eskil($top,leftLabel) $apa
        set ::eskil($top,leftOK) 1
        return 1
    }
    return 0
}

proc doOpenRight {top {forget 0}} {
    if {!$forget && [info exists ::eskil($top,rightDir)]} {
        set initDir $::eskil($top,rightDir)
    } elseif {[info exists ::eskil($top,leftDir)]} {
        set initDir $::eskil($top,leftDir)
    } else {
        set initDir [pwd]
    }

    set apa [myOpenFile -title "Select right file" -initialdir $initDir \
            -parent $top]
    if {$apa != ""} {
        set ::eskil($top,rightDir) [file dirname $apa]
        set ::eskil($top,rightFile) $apa
        set ::eskil($top,rightLabel) $apa
        set ::eskil($top,rightOK) 1
        return 1
    }
    return 0
}

proc doOpenAncestor {top} {
    if {$::eskil($top,ancestorFile) ne ""} {
        set initDir [file dirname $::eskil($top,ancestorFile)]
    } elseif {[info exists ::eskil($top,leftDir)]} {
        set initDir $::eskil($top,leftDir)
    } elseif {[info exists ::eskil($top,rightDir)]} {
        set initDir $::eskil($top,rightDir)
    } else {
        set initDir [pwd]
    }
    set apa [myOpenFile -title "Select ancestor file" -initialdir $initDir \
            -parent $top]
    if {$apa != ""} {
        set ::eskil($top,ancestorFile) $apa
        return 1
    }
    return 0
}

proc openLeft {top} {
    if {[doOpenLeft $top]} {
        set ::eskil($top,mode) ""
        set ::eskil($top,mergeFile) ""
        doDiff $top
    }
}

proc openRight {top} {
    if {[doOpenRight $top]} {
        set ::eskil($top,mode) ""
        set ::eskil($top,mergeFile) ""
        doDiff $top
    }
}

proc openAncestor {top} {
    if {[doOpenAncestor $top]} {
        # Redo diff with ancestor
        doDiff $top
    }
}

proc openConflict {top} {
    global Pref
    if {[doOpenRight $top]} {
        startConflictDiff $top $::eskil($top,rightFile)
        set ::eskil($top,mergeFile) ""
        doDiff $top
    }
}

proc openPatch {top} {
    global Pref
    if {[doOpenLeft $top]} {
        set ::eskil($top,mode) "patch"
        set Pref(ignore) " "
        set Pref(nocase) 0
        set Pref(noempty) 0 
        set ::eskil($top,patchFile) $::eskil($top,leftFile)
        set ::eskil($top,patchData) ""
        doDiff $top
    }
}

# Get data from clipboard and display as a patch.
proc doPastePatch {top} {
    if {[catch {::tk::GetSelection $top CLIPBOARD} sel]} {
        tk_messageBox -icon error -title "Eskil Error" -parent $top \
                -message "Could not retreive clipboard" -type ok
        return
    }
    set ::eskil($top,mode) "patch"
    set ::Pref(ignore) " "
    set ::Pref(nocase) 0
    set ::Pref(noempty) 0 
    set ::eskil($top,patchFile) ""
    set ::eskil($top,patchData) $sel
    doDiff $top
}

proc openRev {top} {
    if {[doOpenRight $top]} {
        set rev [detectRevSystem $::eskil($top,rightFile)]
        if {$rev eq ""} {
            tk_messageBox -icon error -title "Eskil Error" -message \
                    "Could not figure out which revison control system\
                    \"$::eskil($top,rightFile)\" is under." -type ok
            return
        }
        startRevMode $top $rev $::eskil($top,rightFile)
        set ::eskil($top,mergeFile) ""
        doDiff $top
    }
}

proc openBoth {top forget} {
    if {[doOpenLeft $top]} {
        if {[doOpenRight $top $forget]} {
            set ::eskil($top,mode) ""
            set ::eskil($top,mergeFile) ""
            doDiff $top
        }
    }
}

# File drop using TkDnd
proc fileDrop {top side files} {
    # FIXA: Maybe single drop during rev mode should stay in rev mode?
    # Dropping two files mean set both
    if {[llength $files] >= 2} {
        set leftFile [lindex $files 0]
        set rightFile [lindex $files 1]
    } elseif {$side eq "left"} {
        set leftFile [lindex $files 0]
        set rightFile ""
    } else {
        set leftFile ""
        set rightFile [lindex $files 0]
    }
    if {$leftFile ne ""} {
        set ::eskil($top,leftDir) [file dirname $leftFile]
        set ::eskil($top,leftFile) $leftFile
        set ::eskil($top,leftLabel) $leftFile
        set ::eskil($top,leftOK) 1
        set ::eskil($top,mode) ""
        set ::eskil($top,mergeFile) ""
    }
    if {$rightFile ne ""} {
        set ::eskil($top,rightDir) [file dirname $rightFile]
        set ::eskil($top,rightFile) $rightFile
        set ::eskil($top,rightLabel) $rightFile
        set ::eskil($top,rightOK) 1
        set ::eskil($top,mode) ""
        set ::eskil($top,mergeFile) ""
    }
    if {$::eskil($top,leftOK) && $::eskil($top,rightOK)} {
        doDiff $top
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

    ttk::frame $w
    $class $w.s {*}$args

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
        set ::eskil($top,aligns) {}
    } else {
        set i 0
        while 1 {
            set i [lsearch -integer -start $i $::eskil($top,aligns) $leftline]
            if {$i < 0} break
            if {($i % 2) == 0} {
                set ::eskil($top,aligns) [lreplace $::eskil($top,aligns) \
                        $i [+ $i 1]]
                break
            }
            incr i
        }
    }

    if {[llength $::eskil($top,aligns)] == 0} {
        disableAlign $top
    }
}

proc NoMarkAlign {top} {
    unset -nocomplain ::eskil($top,align1)
    unset -nocomplain ::eskil($top,align2)
    unset -nocomplain ::eskil($top,aligntext1)
    unset -nocomplain ::eskil($top,aligntext2)
}

# Mark a line as aligned.
proc markAlign {top n line text} {
    set ::eskil($top,align$n) $line
    set ::eskil($top,aligntext$n) $text

    if {[info exists ::eskil($top,align1)] && [info exists ::eskil($top,align2)]} {
        if {![string equal $::eskil($top,aligntext1) $::eskil($top,aligntext2)]} {
            set apa [tk_messageBox -icon question -title "Align" -type yesno \
                    -message "Those lines are not equal.\nReally align them?"]
            if {$apa != "yes"} {
                return 0
            }
        }

        lappend ::eskil($top,aligns) $::eskil($top,align1) $::eskil($top,align2)
        enableAlign $top

        NoMarkAlign $top
        return 1
    }
    return 0
}

# Called by popup menus over row numbers to add command for alignment.
# Returns 1 if nothing was added.
proc alignMenu {m top n x y} {
    # Get the row that was clicked
    set w $::widgets($top,wLine$n)
    set index [$w index @$x,$y]
    set row [lindex [split $index "."] 0]

    set data [$w get $row.0 $row.end]
    # Must be a line number
    if {![regexp {\d+} $data line]} {
        return 1
    }
    set text [$::widgets($top,wDiff$n) get $row.0 $row.end]

    set other [expr {$n == 1 ? 2 : 1}]
    set cmd [list markAlign $top $n $line $text]
    if {![info exists ::eskil($top,align$other)]} {
        set label "Mark line for alignment"
    } else {
        set label "Align with line $::eskil($top,align$other) on other side"
    }

    if {[info exists ::eskil($top,aligns)]} {
        foreach {align1 align2} $::eskil($top,aligns) {
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

# Set up bindings to allow setting alignment using drag
proc SetupAlignDrag {top left right} {
    bind $left <ButtonPress-1> [list startAlignDrag $top 1 %x %y %X %Y]\;break
    bind $left <B1-Motion> [list motionAlignDrag $top 1 0 %x %y %X %Y]\;break
    bind $left <Shift-B1-Motion> [list motionAlignDrag $top 1 1 %x %y %X %Y]\;break
    bind $left <ButtonRelease-1> [list endAlignDrag $top 1 %x %y %X %Y]\;break
    bind $left <B1-Leave> break
    bind $right <ButtonPress-1> [list startAlignDrag $top 2 %x %y %X %Y]\;break
    bind $right <B1-Motion> [list motionAlignDrag $top 2 0 %x %y %X %Y]\;break
    bind $right <Shift-B1-Motion> [list motionAlignDrag $top 2 1 %x %y %X %Y]\;break
    bind $right <ButtonRelease-1> [list endAlignDrag $top 2 %x %y %X %Y]\;break
    bind $right <B1-Leave> break
}

# Button has been pressed over line window
proc startAlignDrag {top n x y X Y} {
    # Get the row that was clicked
    set w $::widgets($top,wLine$n)
    set index [$w index @$x,$y]
    set row [lindex [split $index "."] 0]

    set data [$w get $row.0 $row.end]
    set ::eskil($top,alignDrag,state) none
    # Must be a line number
    if {![regexp {\d+} $data line]} {
        return 1
    }
    # Set up information about start of drag
    set text [$::widgets($top,wDiff$n) get $row.0 $row.end]
    set other [expr {$n == 1 ? 2 : 1}]
    set ::eskil($top,alignDrag,X) $X
    set ::eskil($top,alignDrag,Y) $Y
    set ::eskil($top,alignDrag,from) $n
    set ::eskil($top,alignDrag,line$n) $line
    set ::eskil($top,alignDrag,text$n) $text
    set ::eskil($top,alignDrag,line$other) "?"
    set ::eskil($top,alignDrag,state) press
}

# Mouse moves with button down
proc motionAlignDrag {top n shift x y X Y} {
    if {$::eskil($top,alignDrag,state) eq "press"} {
        # Have we moved enough to call it dragging?
        set dX [expr {abs($X - $::eskil($top,alignDrag,X))}]
        set dY [expr {abs($Y - $::eskil($top,alignDrag,Y))}]
        if {$dX + $dY > 3} {
            # Start a drag action
            set w $top.alignDrag
            destroy $w
            toplevel $w
            wm overrideredirect $w 1
            label $w.l -borderwidth 1 -relief solid -justify left
            pack $w.l
            set ::eskil($top,alignDrag,W) $w
            set ::eskil($top,alignDrag,state) "drag"
        }
    }
    if {$::eskil($top,alignDrag,state) eq "drag"} {
        set w $::eskil($top,alignDrag,W)
        # Move drag label with cursor
        wm geometry $w +[expr {$X + 1}]+[expr {$Y + 1}]
        
        set n $::eskil($top,alignDrag,from)
        set other [expr {$n == 1 ? 2 : 1}]
        set w2 $::widgets($top,wLine$other)
        # Are we over the other line window?
        if {[winfo containing $X $Y] eq $w2} {
            set x [expr {$X - [winfo rootx $w2]}]
            set y [expr {$Y - [winfo rooty $w2]}]
            set index [$w2 index @$x,$y]
            set row [lindex [split $index "."] 0]
            set data [$w2 get $row.0 $row.end]
            if {![regexp {\d+} $data line]} {
                set ::eskil($top,alignDrag,line$other) "?"
            } else {
                set ::eskil($top,alignDrag,line$other) $line
                set text [$::widgets($top,wDiff$other) get $row.0 $row.end]
                set ::eskil($top,alignDrag,text$other) $text
            }
        } else {
            set ::eskil($top,alignDrag,line$other) "?"
        }
        set txt "Align Left $::eskil($top,alignDrag,line1)"
        append txt "\nwith Right $::eskil($top,alignDrag,line2)"
        set ::eskil($top,alignDrag,shift) $shift
        if {$shift} {
            append txt "\nAnd Redo Diff"
        }
        $w.l configure -text $txt
    }
}

# Button has been released
proc endAlignDrag {top n x y X Y} {
    if {$::eskil($top,alignDrag,state) eq "drag"} {
        destroy $::eskil($top,alignDrag,W)
        # Are both line numbers valid? I.e. is this a full align operation?
        if {$::eskil($top,alignDrag,line1) ne "?" && \
                $::eskil($top,alignDrag,line2) ne "?"} {
            NoMarkAlign $top
            markAlign $top 1 $::eskil($top,alignDrag,line1) \
                    $::eskil($top,alignDrag,text1)
            set marked [markAlign $top 2 $::eskil($top,alignDrag,line2) \
                    $::eskil($top,alignDrag,text2)]
            if {$::eskil($top,alignDrag,shift) && $marked} {
                redoDiff $top
            }
        }
    }
    set ::eskil($top,alignDrag,state) none
}

###################
# Diff highlighting
###################

proc hlSelect {top hl} {
    highLightChange $top $hl
}

proc hlSeparate {top n hl} {
    set ::eskil($top,separate$n) $hl
    set wd $::widgets($top,wDiff$n)
    set wl $::widgets($top,wLine$n)

    if {$hl eq ""} {
        set range [$wd tag ranges sel]
    } else {
        set range [$wl tag ranges hl$::eskil($top,separate$n)]
    }
    set text [$wd get {*}$range]
    set ::eskil($top,separatetext$n) $text

    # Get the lines involved in the display
    set from [lindex $range 0]
    set to   [lindex $range 1]
    lassign [split $from "."] froml fromi
    lassign [split $to   "."] tol   toi
    if {$toi == 0} {incr tol -1}
    # Get the corresponding lines in the file
    set t [$wl get $froml.0 $tol.end]
    set lines [lsort -integer [regexp -all -inline {\d+} $t]]
    set froml [lindex $lines 0]
    set tol [lindex $lines end]
    set ::eskil($top,separatelines$n) [list $froml $tol]

    if {[info exists ::eskil($top,separate1)] && \
            [info exists ::eskil($top,separate2)]} {
        if {1} {
            cloneDiff $top [concat $::eskil($top,separatelines1) \
                    $::eskil($top,separatelines2)]
        } else {
            set f1 [tmpFile]
            set f2 [tmpFile]
            set ch [open $f1 w]
            puts $ch $::eskil($top,separatetext1)
            close $ch
            set ch [open $f2 w]
            puts $ch $::eskil($top,separatetext2)
            close $ch

            newDiff $f1 $f2
        }
        unset ::eskil($top,separate1)
        unset ::eskil($top,separate2)
    }
}

proc hlPopup {top n hl X Y x y} {
    if {[info exists ::eskil($top,nopopup)] && $::eskil($top,nopopup)} return
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
    if {![info exists ::eskil($top,separate$other)]} {
        set label "Mark for Separate Diff"
    } else {
        set label "Separate Diff"
    }

    .lpm add command -label $label -command [list hlSeparate $top $n $hl]
    alignMenu .lpm $top $n $x $y

    set ::eskil($top,nopopup) 1
    tk_popup .lpm $X $Y
    after idle [list after 1 [list set ::eskil($top,nopopup) 0]]

    return
}

# This is called when right clicking over the line numbers which are not
# marked for changes
proc rowPopup {w X Y x y} {
    set top [winfo toplevel $w]
    if {[info exists ::eskil($top,nopopup)] && $::eskil($top,nopopup)} return
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

    set ::eskil($top,nopopup) 1
    tk_popup .lpm $X $Y
    after idle [list after 1 [list set ::eskil($top,nopopup) 0]]
}

proc nextHighlight {top} {
    set tag hl$::HighLightCount
    foreach n {1 2} {
        $::widgets($top,wLine$n) tag bind $tag <ButtonPress-3> \
                "hlPopup $top $n $::HighLightCount %X %Y %x %y ; break"
        $::widgets($top,wLine$n) tag bind $tag <ButtonPress-1> \
                "hlSelect $top $::HighLightCount"
    }
    incr ::HighLightCount
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
    toplevel $top.balloon -background black
    wm withdraw $top.balloon
    wm overrideredirect $top.balloon 1

    set wid 0
    foreach x {1 2} {
        text $top.balloon.t$x -relief flat -font $font -background \#ffffcc \
            -foreground black -padx 2 -pady 0 -height 1 -wrap word
        $top.balloon.t$x tag configure new1 -foreground $Pref(colornew1) \
                -background $Pref(bgnew1)
        $top.balloon.t$x tag configure change -foreground $Pref(colorchange) \
                -background $Pref(bgchange)
        $top.balloon.t$x tag configure new2 -foreground $Pref(colornew2) \
                -background $Pref(bgnew2)
        $top.balloon.t$x tag configure equal -foreground $Pref(colorequal) \
                -background $Pref(bgequal)
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

    foreach top $::eskil(diffWindows) {
        if {$top eq ".clipdiff"} continue
        if {$top != ".dirdiff"} {
            foreach item {wLine1 wDiff1 wLine2 wDiff2} {
                if {![info exists ::widgets($top,$item)]} continue
                set w $::widgets($top,$item)

                $w tag configure equal -foreground $Pref(colorequal) \
                        -background $Pref(bgequal)
                $w tag configure new1 -foreground $Pref(colornew1) \
                        -background $Pref(bgnew1)
                $w tag configure change -foreground $Pref(colorchange) \
                        -background $Pref(bgchange)
                $w tag configure new2 -foreground $Pref(colornew2) \
                        -background $Pref(bgnew2)
            }
            continue
        }

#        $dirdiff(wLeft) tag configure new1 -foreground $Pref(colornew1) \
#                -background $Pref(bgnew1)
#        $dirdiff(wLeft) tag configure change -foreground $Pref(colorchange) \
#                -background $Pref(bgchange)
#        $dirdiff(wLeft) tag configure changed -foreground $Pref(colorchange)
#        $dirdiff(wLeft) tag configure invalid -background #a9a9a9
#        $dirdiff(wRight) tag configure new2 -foreground $Pref(colornew2) \
#                -background $Pref(bgnew2)
#        $dirdiff(wRight) tag configure change -foreground $Pref(colorchange) \
#                -background $Pref(bgchange)
#        $dirdiff(wRight) tag configure changed -foreground $Pref(colorchange)
#        $dirdiff(wRight) tag configure invalid -background #a9a9a9

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

# Emulate a label that:
# 1 : Displays the right part of the text if there isn't enough room
# 2 : Justfify text to the left if there is enough room.
# 3 : Does not try to allocate space according to its contents
proc fileLabel {w args} {
    ttk::entryX $w -style TLabel
    $w configure {*}$args

    $w configure -takefocus 0 -state readonly ;#-readonlybackground $bg

    set i [lsearch $args -textvariable]
    if {$i >= 0} {
	set var [lindex $args [+ $i 1]]
	uplevel \#0 "trace variable $var w \
		{after idle {$w xview end} ;#}"
    }
}

# Fill in default data for a diff window
proc initDiffData {top} {
    set ::eskil($top,leftOK) 0
    set ::eskil($top,rightOK) 0
    set ::eskil($top,mode) ""
    set ::eskil($top,printFile) ""
    set ::eskil($top,mergeFile) ""
    set ::eskil($top,ancestorFile) ""
    set ::eskil($top,conflictFile) ""
    set ::eskil($top,limitlines) 0
    set ::eskil($top,plugin) ""
}

# Create a new diff window and diff two files
proc newDiff {file1 file2 {range {}}} {
    set top [makeDiffWin]
    update

    set ::eskil($top,leftDir) [file dirname $file1]
    set ::eskil($top,leftFile) $file1
    set ::eskil($top,leftLabel) $file1
    set ::eskil($top,leftOK) 1
    set ::eskil($top,rightDir) [file dirname $file2]
    set ::eskil($top,rightFile) $file2
    set ::eskil($top,rightLabel) $file2
    set ::eskil($top,rightOK) 1
    set ::eskil($top,mode) ""
    set ::eskil($top,range) $range
    wm deiconify $top
    raise $top
    update
    doDiff $top
    return $top
}


# Create a new diff window equal to another, except for possibly a range
proc cloneDiff {other {range {}}} {
    set top [makeDiffWin]
    update

    foreach item [array names ::diff $other,*] {
        regsub {^[^,]*,} $item {} item
        set ::eskil($top,$item) $::eskil($other,$item)
    }
    if {[llength $range] != 0} {
        set ::eskil($top,range) $range
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
        set ::eskil(debug) 1
        catch {console show}
        set ::eskil(backdoor) ""
    }
}

# Build the main window
proc makeDiffWin {{top {}}} {
    global Pref tcl_platform

    if {$top != "" && [winfo exists $top] && [winfo toplevel $top] eq $top} {
        # Reuse the old window
        destroy {*}[winfo children $top]
    } else {
        # Locate a free toplevel name
        if {[info exists ::eskil(topDiffCnt)]} {
            set t $::eskil(topDiffCnt)
        } else {
            set t 0
        }
        while {[winfo exists .diff$t]} {
            incr t
        }
        set top .diff$t
        toplevel $top
        eskilRegisterToplevel $top
    }

    wm title $top "Eskil:"
    wm protocol $top WM_DELETE_WINDOW [list cleanupAndExit $top]

    ttk::frame $top.f
    grid $top.f -row 0 -columnspan 4 -sticky nws
    lappend ::widgets(toolbars) $top.f
    if {!$::Pref(toolbar)} {
        grid remove $top.f
    }

    menu $top.m
    $top configure -menu $top.m

    $top.m add cascade -label "File" -underline 0 -menu $top.m.mf
    menu $top.m.mf
    $top.m.mf add command -label "Redo Diff" -underline 5 \
            -command [list redoDiff $top] -state disabled
    if {$::eskil(debug) == 1} {
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
    $top.m.mf add command -label "Open Ancestor File..." \
            -command [list openAncestor $top]
    $top.m.mf add command -label "Open Conflict File..." \
            -command [list openConflict $top]
    $top.m.mf add command -label "Open Patch File..." \
            -command [list openPatch $top]
    $top.m.mf add command -label "Revision Diff..." -underline 0 \
            -command [list openRev $top]
    $top.m.mf add separator
    $top.m.mf add command -label "Print Pdf..." -underline 0 \
            -command [list doPrint $top]
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
    $top.m.mo add command -label "Plugins..." -underline 1 \
            -command [list EditPrefPlugins $top]
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
    $top.m.mo.i add checkbutton -label "Empty" \
            -variable Pref(noempty)
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
    $top.m.mo.p add checkbutton -label "Fine chunks" -variable Pref(finegrainchunks)
    $top.m.mo.p add separator
    $top.m.mo.p add checkbutton -label "Mark last" -variable Pref(marklast)

    menu $top.m.mo.c
    $top.m.mo.c add radiobutton -label "Show all lines" \
            -variable ::Pref(context) -value -1
    $top.m.mo.c add radiobutton -label "Show only diffs" \
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
    $top.m.mt add command -label "Paste Patch" -underline 0 \
            -command [list doPastePatch $top]
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
    foreach label {{Revision Control} {Edit Mode} {Plugins}} \
            file {revision.txt editmode.txt plugins.txt} {
        $top.m.help add command -label $label \
                -command [list makeDocWin $file] -underline 0
    }
    $top.m.help add separator
    $top.m.help add command -label "About" -command makeAboutWin -underline 0

    ttk::label $top.lr1 -text "Rev 1"
    addBalloon $top.lr1 "Revision number for version diff."
    ttk::entryX $top.er1 -width 12 -textvariable ::eskil($top,doptrev1)
    set ::widgets($top,rev1) $top.er1
    bind $top.er1 <Key-Return> [list redoDiff $top]

    ttk::label $top.lr2 -text "Rev 2"
    addBalloon $top.lr2 "Revision number for version diff."
    ttk::entryX $top.er2 -width 12 -textvariable ::eskil($top,doptrev2)
    set ::widgets($top,rev2) $top.er2
    bind $top.er2 <Key-Return> [list redoDiff $top]

    ttk::button $top.bcm -text Commit -command [list revCommit $top] \
            -state disabled -underline 0
    set ::widgets($top,commit) $top.bcm
    ttk::button $top.blg -text Log -command [list revLog $top] \
        -state disabled -underline 0
    set ::widgets($top,log) $top.blg
    ttk::button $top.bfp -text "Prev Diff" \
            -command [list findDiff $top -1] \
            -underline 0
    ttk::button $top.bfn -text "Next Diff" \
            -command [list findDiff $top 1] \
            -underline 0
    bind $top <Alt-n> [list findDiff $top 1]
    bind $top <Alt-p> [list findDiff $top -1]
    bind $top <Alt-c> [list revCommit $top]
    bind $top <Alt-l> [list revLog $top]

    catch {font delete myfont}
    font create myfont -family $Pref(fontfamily) -size $Pref(fontsize)

    fileLabel $top.l1 -textvariable ::eskil($top,leftLabel)
    fileLabel $top.l2 -textvariable ::eskil($top,rightLabel)

    ttk::frame $top.ft1 -borderwidth 2 -relief sunken
    text $top.ft1.tl -height $Pref(lines) -width 5 -wrap none \
            -font myfont -borderwidth 0 -padx 0 -highlightthickness 0 \
            -takefocus 0
    text $top.ft1.tt -height $Pref(lines) -width $Pref(linewidth) -wrap none \
            -xscrollcommand [list $top.sbx1 set] \
            -font myfont -borderwidth 0 -padx 1 \
            -highlightthickness 0
    $top.ft1.tt configure -tabstyle wordprocessor
    tk::frame $top.ft1.f -width 2 -height 2 -background lightgray
    pack $top.ft1.tl -side left -fill y
    pack $top.ft1.f -side left -fill y
    pack $top.ft1.tt -side right -fill both -expand 1
    scrollbar $top.sby -orient vertical
    scrollbar $top.sbx1 -orient horizontal -command [list $top.ft1.tt xview]
    set ::widgets($top,wLine1) $top.ft1.tl
    set ::widgets($top,wDiff1) $top.ft1.tt

    ttk::frame $top.ft2 -borderwidth 2 -relief sunken
    text $top.ft2.tl -height $Pref(lines) -width 5 -wrap none \
            -font myfont -borderwidth 0 -padx 0 -highlightthickness 0 \
            -takefocus 0
    text $top.ft2.tt -height $Pref(lines) -width $Pref(linewidth) -wrap none \
            -xscrollcommand [list $top.sbx2 set] \
            -font myfont -borderwidth 0 -padx 1 \
            -highlightthickness 0
    $top.ft2.tt configure -tabstyle wordprocessor
    tk::frame $top.ft2.f -width 2 -height 2 -background lightgray
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

    # Set up file dropping in text windows if TkDnd is available
    if {![catch {package require tkdnd}]} {
        dnd bindtarget $top.ft1.tt text/uri-list <Drop> "fileDrop $top left %D"
        dnd bindtarget $top.ft2.tt text/uri-list <Drop> "fileDrop $top right %D"
    }

    ttk::label $top.le -textvariable ::widgets($top,eqLabel) -width 1
    addBalloon $top.le "* means external diff is running.\n= means files do\
            not differ.\n! means a large block is being processed.\nBlank\
            means files differ."
    # FIXA: verify that this label is ok after Tile migration
    ttk::label $top.ls -width 1 \
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
    SetupAlignDrag $top $top.ft1.tl $top.ft2.tl

    grid $top.l1   $top.le -        $top.l2   -row 1 -sticky news
    grid $top.ft1  $map    $top.sby $top.ft2  -row 2 -sticky news
    grid $top.sbx1 $top.ls -        $top.sbx2 -row 3 -sticky news
    grid columnconfigure $top {0 3} -weight 1
    grid rowconfigure $top 2 -weight 1
    grid $map -pady [expr {[winfo reqwidth $top.sby] - 2}]
    grid $top.ls -sticky ""

    bind $top <Key-Up>    [list scrollText $top -1 u]
    bind $top <Key-Down>  [list scrollText $top  1 u]
    bind $top <Key-Prior> [list scrollText $top -1 pa]
    bind $top <Key-Next>  [list scrollText $top  1 pa]
    bind $top <Key-Escape> [list focus $top]
    if {$::eskil(debug) == 0} {
        bind $top <Key> "backDoor %A"
    }

    pack $top.bfn -in $top.f -side right -padx {3 6}
    pack $top.bfp $top.bcm $top.blg \
            $top.er2 $top.lr2 $top.er1 $top.lr1 \
            -in $top.f -side right -padx 3
    pack $top.bfn $top.bfp $top.bcm -ipadx 15

    if {$::eskil(debug) == 1} {
        set dMenu [DebugMenu $top.m]
        $dMenu add checkbutton -label "Wrap" -variable wrapstate \
                -onvalue char -offvalue none -command \
                "$top.ft1.tt configure -wrap \$wrapstate ;\
                $top.ft2.tt configure -wrap \$wrapstate"
        $dMenu add command -label "Date Filter" \
                -command {set ::eskil(filter) {^Date}}
        $dMenu add separator
        $dMenu add command -label "Reread Source" -underline 0 \
                -command {EskilRereadSource}
        $dMenu add separator
        $dMenu add command -label "Redraw Window" \
                -command [list makeDiffWin $top]
        $dMenu add separator
        $dMenu add command -label "Normal Cursor" \
                -command [list normalCursor $top]
        $dMenu add separator
        $dMenu add command -label "Evalstats" -command {evalstats}
        $dMenu add command -label "_stats" -command {parray _stats}
        $dMenu add command -label "Nuisance" -command [list makeNuisance \
                $top "It looks like you are trying out the debug menu."]
    }

    initDiffData $top
    return $top
}

proc ValidateNewColors {} {
    global TmpPref
    foreach item {colorchange bgchange colornew1 bgnew1
        colornew2 bgnew2 colorequal bgequal} {
        if {![info exists TmpPref($item)]} continue
        set col $TmpPref($item)
        if {$col eq ""} continue
        if {[catch {winfo rgb . $col}]} {
            # FIXA: Error message
            # Just restore for now
            set TmpPref($item) $::Pref($item)
        }
    }
}

# Set new preferences.
proc applyPref {} {
    global Pref TmpPref

    ValidateNewColors
    array set Pref [array get TmpPref]
    applyColor
}

# Update test color fields.
proc testColor {} {
    global TmpPref


    ValidateNewColors
    .pr.fc.t1 tag configure change -foreground $TmpPref(colorchange) \
            -background $TmpPref(bgchange)
    .pr.fc.t2 tag configure new1 -foreground $TmpPref(colornew1) \
            -background $TmpPref(bgnew1)
    .pr.fc.t3 tag configure new2 -foreground $TmpPref(colornew2) \
            -background $TmpPref(bgnew2)
    .pr.fc.t4 tag configure equal -foreground $TmpPref(colorequal) \
            -background $TmpPref(bgequal)
}

# Color dialog.
proc selColor {name} {
    global TmpPref

    set old $TmpPref($name)
    if {$old eq ""} {
        set t [tk_chooseColor -parent .pr]
    } else {
        set t [tk_chooseColor -parent .pr -initialcolor $old]
    }
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

    ttk::frame .pr.fc -borderwidth 1 -relief solid
    ttk::label .pr.fc.l1 -text "Colours" -anchor w
    ttk::label .pr.fc.l2 -text "Text" -anchor w
    ttk::label .pr.fc.l3 -text "Background" -anchor w

    ttk::entryX .pr.fc.e1 -textvariable "TmpPref(colorchange)" -width 10
    ttk::entryX .pr.fc.e2 -textvariable "TmpPref(colornew1)" -width 10
    ttk::entryX .pr.fc.e3 -textvariable "TmpPref(colornew2)" -width 10
    ttk::entryX .pr.fc.e4 -textvariable "TmpPref(colorequal)" -width 10

    ttk::button .pr.fc.b1 -text "Sel" -command "selColor colorchange"
    ttk::button .pr.fc.b2 -text "Sel" -command "selColor colornew1"
    ttk::button .pr.fc.b3 -text "Sel" -command "selColor colornew2"
    ttk::button .pr.fc.b4 -text "Sel" -command "selColor colorequal"

    ttk::entryX .pr.fc.e5 -textvariable "TmpPref(bgchange)" -width 10
    ttk::entryX .pr.fc.e6 -textvariable "TmpPref(bgnew1)" -width 10
    ttk::entryX .pr.fc.e7 -textvariable "TmpPref(bgnew2)" -width 10
    ttk::entryX .pr.fc.e8 -textvariable "TmpPref(bgequal)" -width 10

    ttk::button .pr.fc.b5 -text "Sel" -command "selColor bgchange"
    ttk::button .pr.fc.b6 -text "Sel" -command "selColor bgnew1"
    ttk::button .pr.fc.b7 -text "Sel" -command "selColor bgnew2"
    ttk::button .pr.fc.b8 -text "Sel" -command "selColor bgequal"

    text .pr.fc.t1 -width 12 -height 1 -font myfont -takefocus 0
    text .pr.fc.t2 -width 12 -height 1 -font myfont -takefocus 0
    text .pr.fc.t3 -width 12 -height 1 -font myfont -takefocus 0
    text .pr.fc.t4 -width 12 -height 1 -font myfont -takefocus 0
    .pr.fc.t1 tag configure change -foreground $TmpPref(colorchange) \
            -background $TmpPref(bgchange)
    .pr.fc.t2 tag configure new1 -foreground $TmpPref(colornew1) \
            -background $TmpPref(bgnew1)
    .pr.fc.t3 tag configure new2 -foreground $TmpPref(colornew2) \
            -background $TmpPref(bgnew2)
    .pr.fc.t4 tag configure equal -foreground $TmpPref(colorequal) \
            -background $TmpPref(bgequal)
    .pr.fc.t1 insert end "Changed text" change
    .pr.fc.t2 insert end "Deleted text" new1
    .pr.fc.t3 insert end "Added text" new2
    .pr.fc.t4 insert end "Equal text" equal

    .pr.fc.t1 configure -state disabled
    .pr.fc.t2 configure -state disabled
    .pr.fc.t3 configure -state disabled
    .pr.fc.t4 configure -state disabled

    ttk::button .pr.b1 -text "Apply" -command applyPref
    ttk::button .pr.b2 -text "Test"  -command testColor
    ttk::button .pr.b3 -text "Close" -command {destroy .pr}

    grid .pr.fc.l1 .pr.fc.l2 x .pr.fc.l3 x -row 0 -sticky ew -padx 1 -pady 1
    grid .pr.fc.t1 .pr.fc.e1 .pr.fc.b1 .pr.fc.e5 .pr.fc.b5 -row 1 \
            -sticky nsew -padx 1 -pady 1
    grid .pr.fc.t2 .pr.fc.e2 .pr.fc.b2 .pr.fc.e6 .pr.fc.b6 -row 2 \
            -sticky nsew -padx 1 -pady 1
    grid .pr.fc.t3 .pr.fc.e3 .pr.fc.b3 .pr.fc.e7 .pr.fc.b7 -row 3 \
            -sticky nsew -padx 1 -pady 1
    grid .pr.fc.t4 .pr.fc.e4 .pr.fc.b4 .pr.fc.e8 .pr.fc.b8 -row 4 \
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
        if {$fixed || !$::eskil(fixedfont)} {
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

    ttk::label .fo.ltmp -text "Searching for fonts..."
    pack .fo.ltmp -padx {10 50} -pady {10 50}
    update

    catch {font delete tmpfont}
    font create tmpfont

    array set TmpPref [array get Pref]
    ttk::labelframe .fo.lf -text "Family" -padding 3
    set lb [Scroll y listbox .fo.lf.lb -width 15 -height 10 \
            -exportselection no -selectmode single]
    bind $lb <<ListboxSelect>> [list exampleFont $lb]
    pack .fo.lf.lb -fill both -expand 1

    ttk::labelframe .fo.ls -text "Size" -padding 3
    spinbox .fo.ls.sp -from 1 -to 30 -increment 1 -width 3 -state readonly \
            -textvariable TmpPref(fontsize) -command [list exampleFont $lb]
    pack .fo.ls.sp -fill both -expand 1

    ttk::label .fo.le -text "Example\n0Ooi1Il" -anchor w -font tmpfont \
            -width 1 -justify left
    if {![info exists ::eskil(fixedfont)]} {set ::eskil(fixedfont) 1}
    ttk::checkbutton .fo.cb -text "Fixed" -variable ::eskil(fixedfont) \
            -command [list UpdateFontBox $lb]
    ttk::button .fo.bo -text "Ok"    -command "applyFont $lb ; destroy .fo"
    ttk::button .fo.ba -text "Apply" -command "applyFont $lb"
    ttk::button .fo.bc -text "Close" -command "destroy .fo"

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
    grid x      .fo.bo -sticky we   -padx 3 -pady 3 -ipadx 10
    grid x      .fo.ba -sticky we   -padx 3 -pady 3 -ipadx 10
    grid x      .fo.bc -sticky we   -padx 3 -pady 3 -ipadx 10
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
    set exa $::eskil($top,prefregexa)

    set result {}
    for {set t 1} {[info exists ::eskil($top,prefregexp$t)]} {incr t} {
        set RE $::eskil($top,prefregexp$t)
        set Sub $::eskil($top,prefregsub$t)
        if {$RE eq ""} continue

        if {[catch {regsub -all -- $RE $exa $Sub _} err]} {
            return
        }
        lappend result $RE $Sub
    }

    set ::Pref(regsub) $result
    destroy $w

    array unset ::eskil $top,prefregexp*
    array unset ::eskil $top,prefregsub*
}

proc EditPrefRegsubUpdate {top args} {
    set exa $::eskil($top,prefregexa)
    set exa2 $::eskil($top,prefregexa2)
    set ok $::widgets($top,prefRegsubOk)

    for {set t 1} {[info exists ::eskil($top,prefregexp$t)]} {incr t} {
        set RE $::eskil($top,prefregexp$t)
        set Sub $::eskil($top,prefregsub$t)

        if {$RE eq ""} continue

        if {[catch {regsub -all -- $RE $exa $Sub result} err]} {
            set ::eskil($top,prefregresult) "$t ERROR: $err"
            $ok configure -state disabled
            return
        } else {
            set exa $result
        }
        if {[catch {regsub -all -- $RE $exa2 $Sub result} err]} {
            set ::eskil($top,prefregresult2) "$t ERROR: $err"
            $ok configure -state disabled
            return
        } else {
            set exa2 $result
        }
    }
    set ::eskil($top,prefregresult2) $exa2
    set ::eskil($top,prefregresult) $exa
    $ok configure -state normal
}

proc AddPrefRegsub {top parent} {
    for {set t 1} {[winfo exists $parent.fr$t]} {incr t} {
        #Empty
    }
    set w [ttk::frame $parent.fr$t -borderwidth 2 -relief groove -padding 3]
    ttk::label $w.l1 -text "Regexp:" -anchor "w"
    ttk::entryX $w.e1 -textvariable ::eskil($top,prefregexp$t) -width 60
    ttk::label $w.l2 -text "Subst:" -anchor "w"
    ttk::entryX $w.e2 -textvariable ::eskil($top,prefregsub$t)

    grid $w.l1 $w.e1 -sticky we -padx 3 -pady 3
    grid $w.l2 $w.e2 -sticky we -padx 3 -pady 3
    grid columnconfigure $w 1 -weight 1

    pack $w -side "top" -fill x -padx 3 -pady 3

    trace add variable ::eskil($top,prefregexp$t) write \
            [list EditPrefRegsubUpdate $top]
    trace add variable ::eskil($top,prefregsub$t) write \
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

    ttk::button $w.b -text "Add" -command [list AddPrefRegsub $top $w]

    # Result example part
    if {![info exists ::eskil($top,prefregexa)]} {
        set ::eskil($top,prefregexa) \
                "An example TextString FOR_REGSUB /* Comment */"
        set ::eskil($top,prefregexa2) \
                "An example TextString FOR_REGSUB /* Comment */"
    }
    ttk::labelframe $w.res -text "Preprocessing result" -padding 3
    ttk::label $w.res.l3 -text "Example 1:" -anchor "w"
    ttk::entryX $w.res.e3 -textvariable ::eskil($top,prefregexa) -width 60
    ttk::label $w.res.l4 -text "Result 1:" -anchor "w"
    ttk::label $w.res.e4 -textvariable ::eskil($top,prefregresult) \
            -anchor "w" -width 10
    ttk::label $w.res.l5 -text "Example 2:" -anchor "w"
    ttk::entryX $w.res.e5 -textvariable ::eskil($top,prefregexa2)
    ttk::label $w.res.l6 -text "Result 2:" -anchor "w"
    ttk::label $w.res.e6 -textvariable ::eskil($top,prefregresult2) \
            -anchor "w" -width 10

    grid $w.res.l3 $w.res.e3 -sticky we -padx 3 -pady 3
    grid $w.res.l4 $w.res.e4 -sticky we -padx 3 -pady 3
    grid $w.res.l5 $w.res.e5 -sticky we -padx 3 -pady 3
    grid $w.res.l6 $w.res.e6 -sticky we -padx 3 -pady 3
    grid columnconfigure $w.res 1 -weight 1

    # Buttons
    ttk::frame $w.fb -padding 3
    ttk::button $w.fb.b1 -text "Ok"     -command [list EditPrefRegsubOk $top $w]
    ttk::button $w.fb.b2 -text "Cancel" -command [list destroy $w]
    set ::widgets($top,prefRegsubOk) $w.fb.b1

    grid $w.fb.b1 x $w.fb.b2 -sticky we
    grid columnconfigure $w.fb {0 2} -uniform a
    grid columnconfigure $w.fb 1 -weight 1

    # Top layout
    pack $w.b -side "top" -anchor "w" -padx 3 -pady 3 -ipadx 15
    pack $w.fb $w.res -side bottom -fill x -padx 3 -pady 3

    # Fill in existing or an empty line
    if {[llength $::Pref(regsub)] == 0} {
        AddPrefRegsub $top $w
    } else {
        set t 1
        foreach {RE Sub} $::Pref(regsub) {
            set ::eskil($top,prefregexp$t) $RE
            set ::eskil($top,prefregsub$t) $Sub
            AddPrefRegsub $top $w
            incr t
        }
    }

    trace add variable ::eskil($top,prefregexa) write \
            [list EditPrefRegsubUpdate $top]
    trace add variable ::eskil($top,prefregexa2) write \
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
  -           : Read patch file from standard input, to allow pipes.
  -review     : View revision control tree as a patch.
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
  -cvs        : Detect CVS first, if multiple version systems are used.
  -svn        : Detect SVN first, if multiple version systems are used.

  -a <file>   : Give anscestor file for three way merge.
  -conflict   : Treat file as a merge conflict file and enter merge
                mode.
  -o <file>   : Specify merge result output file.
  -fine       : Use fine grained chunks. Useful for merging.

  -browse     : Automatically bring up file dialog after starting.
  -server     : Set up Eskil to be controllable from the outside.

  -print <file>          : Generate PDF and exit.
  -printCharsPerLine <n> : Adapt font size for this line length and wrap. (80)
  -printPaper <paper>    : Select paper size (a4)
  -printHeaderSize <n>   : Font size for page header (10)
  -printColorChange <RGB> : Color for change   (1.0 0.7 0.7)
  -printColorOld <RGB>    : Color for old text (0.7 1.0 0.7)
  -printColorNew <RGB     : Color for new text (0.8 0.8 1.0)

  -plugin <name>       : Preprocess files using plugin.
  -plugininfo <info>   : Pass info to plugin (plugin specific)
  -pluginlist          : List known plugins
  -plugindump <plugin> : Dump plugin source to stdout

  -limit <lines> : Do not process more than <lines> lines.

To list all options matching a prefix, run 'eskil --query prefix'.
In tcsh use this line to get option completion:
complete eskil 'C/-/`eskil --query -`/'}
}

# Helper to validate command line option for color
proc ValidatePdfColor {arg opt} {
    set fail 0
    if {![string is list $arg] || [llength $arg] != 3} {
        set fail 1
    } else {
        foreach val $arg {
            if {![string is double -strict $val] || $val < 0.0 || $val > 1.0} {
                set fail 1
            }
        }
    }
    if {$fail} {
        puts "Argument $opt must be a list of RBG values from 0.0 to 1.0"
        exit
    }
}

# Go through all command line arguments and start the appropriate
# diff window.
# Returns the created toplevel.
# This can be used as an entry point if embedding eskil.
# In that case fill in ::eskil(argv) and ::eskil(argc) before calling.
proc parseCommandLine {} {
    global dirdiff Pref

    set ::eskil(autoclose) 0
    set ::eskil(ignorenewline) 0

    if {$::eskil(argc) == 0} {
        Init
        return [makeDiffWin]
    }

    set allOpts {
        -w --help -help -b -noignore -i -nocase -nodigit -nokeyword -prefix
        -noparse -line -smallblock -block -char -word -limit -nodiff -dir
        -clip -patch -browse -conflict -print
        -printHeaderSize -printCharsPerLine -printPaper
        -printColorChange -printColorOld -printColorNew
        -server -o -a -fine -r -context -cvs -svn -review
        -foreach -preprocess -close -nonewline -plugin -plugininfo
        -plugindump -pluginlist
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
    set doreview 0
    set foreach 0
    set preferedRev "GIT"
    set plugin ""
    set plugininfo ""
    set plugindump ""
    set pluginlist 0

    foreach arg $::eskil(argv) {
        if {$nextArg != ""} {
            if {$nextArg eq "mergeFile"} {
                set opts(mergeFile) [file join [pwd] $arg]
            } elseif {$nextArg eq "ancestorFile"} {
                set opts(ancestorFile) [file join [pwd] $arg]
            } elseif {$nextArg eq "printFile"} {
                set opts(printFile) [file join [pwd] $arg]
            } elseif {$nextArg eq "printHeaderSize"} {
                if {![string is double -strict $arg] || $arg <= 0} {
                    puts "Argument -printHeaderSize must be a positive number"
                    exit
                }
                set Pref(printHeaderSize) $arg
            } elseif {$nextArg eq "printCharsPerLine"} {
                if {![string is integer -strict $arg] || $arg <= 0} {
                    puts "Argument -printCharsPerLine must be a positive number"
                    exit
                }
                set Pref(printCharsPerLine) $arg
            } elseif {$nextArg eq "printPaper"} {
                package require pdf4tcl
                if {[llength [pdf4tcl::getPaperSize $arg]] != 2} {
                    puts "Argument -printPaper must be a valid paper size"
                    puts "Valid paper sizes:"
                    puts [join [lsort -dictionary [pdf4tcl::getPaperSizeList]] \n]
                    exit
                }
                set Pref(printPaper) $arg
            } elseif {$nextArg eq "printColorChange"} {
                ValidatePdfColor $arg -printColorChange
                set Pref(printColorChange) $arg
            } elseif {$nextArg eq "printColorOld"} {
                ValidatePdfColor $arg -printColorOld
                set Pref(printColorNew1) $arg
            } elseif {$nextArg eq "printColorNew"} {
                ValidatePdfColor $arg -printColorNew
                set Pref(printColorNew2) $arg
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
            } elseif {$nextArg eq "plugin"} {
                set plugin $arg
            } elseif {$nextArg eq "plugininfo"} {
                set plugininfo $arg
            } elseif {$nextArg eq "plugindump"} {
                set plugindump $arg
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
        if {$arg ne "-review" && [string range $arg 0 1] eq "-r" && \
                [string length $arg] > 2} {
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
        } elseif {$arg eq "-noempty"} {
            set Pref(noempty) 1
        } elseif {$arg eq "-nodigit"} {
            set Pref(nodigit) 1
        } elseif {$arg eq "-nokeyword"} {
            set Pref(dir,ignorekey) 1
        } elseif {$arg eq "-prefix"} {
            set nextArg prefix
        } elseif {$arg eq "-preprocess"} {
            set nextArg preprocess
        } elseif {$arg eq "-plugin"} {
            set nextArg "plugin"
        } elseif {$arg eq "-plugininfo"} {
            set nextArg "plugininfo"
        } elseif {$arg eq "-plugindump"} {
            set nextArg "plugindump"
        } elseif {$arg eq "-pluginlist"} {
            set pluginlist 1
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
        } elseif {$arg eq "-review"} {
            set doreview 1
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
            # Conflict implies foreach
            set foreach 1
        } elseif {$arg eq "-print" || $arg eq "-printpdf"} {
            set nextArg printFile
        } elseif {$arg in {-printHeaderSize -printCharsPerLine -printPaper \
                -printColorChange -printColorOld -printColorNew}} {
            set nextArg [string range $arg 1 end]
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
        } elseif {$arg eq "-a"} {
            set nextArg ancestorFile
            # Default is no ignore on three-way merge
            set Pref(ignore) " "
        } elseif {$arg eq "-fine"} {
            set Pref(finegrainchunks) 1
        } elseif {$arg eq "-r"} {
            set nextArg revision
        } elseif {$arg eq "-debug"} {
            set ::eskil(debug) 1
        } elseif {$arg eq "-svn"} {
            set preferedRev "SVN"
        } elseif {$arg eq "-cvs"} {
            set preferedRev "CVS"
        } elseif {$arg eq "-"} {
            # Allow "-" for stdin patch processing
            lappend files "-"
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

    if {$pluginlist} {
        printPlugins
        exit
    }
    if {$plugindump ne ""} {
        printPlugin $plugindump
        exit
    }
    if {$plugin ne ""} {
        set pinterp [createPluginInterp $plugin $plugininfo]
        if {$pinterp eq ""} {
            puts "Bad plugin: $plugin"
            printPlugins
            exit
        }
        set opts(plugin) $pinterp
        set opts(pluginname) $plugin
        set opts(plugininfo) $plugininfo
    }

    # Do we start in clip diff mode?
    if {$doclip} {
        return [makeClipDiffWin]
    }

    # Figure out if we start in a diff or dirdiff window.
    set len [llength $files]

    if {$len == 0 && $dodir} {
        set dirdiff(leftDir) [pwd]
        set dirdiff(rightDir) [pwd]
        return [makeDirDiffWin]
    }
    if {$len == 1} {
        set fullname [lindex $files 0]
        if {[FileIsDirectory $fullname 1]} {
            set dirdiff(leftDir) $fullname
            set dirdiff(rightDir) $dirdiff(leftDir)
            return [makeDirDiffWin]
        }
    } elseif {$len >= 2} {
        set fullname1 [lindex $files 0]
        set fullname2 [lindex $files 1]
        if {[FileIsDirectory $fullname1 1] && [FileIsDirectory $fullname2 1]} {
            set dirdiff(leftDir) $fullname1
            set dirdiff(rightDir) $fullname2
            return [makeDirDiffWin]
        }
    }

    # Ok, we have a normal diff
    set top [makeDiffWin]
    update
    # Copy the previously collected options
    foreach {item val} [array get opts] {
        set ::eskil($top,$item) $val
    }

    # It is preferable to see the end if the rev string is too long
    $::widgets($top,rev1) xview end
    $::widgets($top,rev2) xview end

    if {$doreview} {
        set rev [detectRevSystem "" $preferedRev]
        set ::eskil($top,modetype) $rev
        set ::eskil($top,mode) "patch"
        set ::eskil($top,patchFile) ""
        set ::eskil($top,patchData) ""
        set ::eskil($top,reviewFiles) $files
        set ::Pref(toolbar) 1
        after idle [list doDiff $top]
        return $top
    }
    if {$len == 1 || $foreach} {
        set ReturnAfterLoop 0
        set first 1
        foreach file $files {
            if {$first} {
                set first 0
            } else {
                # Create new window for other files
                set top [makeDiffWin]
                update
                # Copy the previously collected options
                foreach {item val} [array get opts] {
                    set ::eskil($top,$item) $val
                }
                # It is preferable to see the end if the rev string is too long
                $::widgets($top,rev1) xview end
                $::widgets($top,rev2) xview end
            }
            set fullname $file
            set fulldir [file dirname $fullname]
            if {$::eskil($top,mode) eq "conflict"} {
                startConflictDiff $top $fullname
                after idle [list doDiff $top]
                set ReturnAfterLoop 1
                continue
            }
            if {!$autobrowse && !$dopatch} {
                # Check for revision control
                set rev [detectRevSystem $fullname $preferedRev]
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
            set ::eskil($top,leftDir) $fulldir
            set ::eskil($top,leftFile) $fullname
            set ::eskil($top,leftLabel) $fullname
            set ::eskil($top,leftOK) 1
            if {$dopatch                                 || \
                    [regexp {\.(diff|patch)$} $fullname] || \
                    $fullname eq "-"} {
                set ::eskil($top,mode) "patch"
                set ::eskil($top,patchFile) $fullname
                set ::eskil($top,patchData) ""
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
        if {$ReturnAfterLoop} {return $top}
    } elseif {$len >= 2} {
        set fullname [file join [pwd] [lindex $files 0]]
        set fulldir [file dirname $fullname]
        set ::eskil($top,leftDir) $fulldir
        set ::eskil($top,leftFile) $fullname
        set ::eskil($top,leftLabel) $fullname
        set ::eskil($top,leftOK) 1
        set fullname [file join [pwd] [lindex $files 1]]
        set fulldir [file dirname $fullname]
        set ::eskil($top,rightDir) $fulldir
        set ::eskil($top,rightFile) $fullname
        set ::eskil($top,rightLabel) $fullname
        set ::eskil($top,rightOK) 1
        if {$noautodiff} {
            enableRedo $top
        } else {
            after idle [list doDiff $top]
        }
    }
    if {$autobrowse && (!$::eskil($top,leftOK) || !$::eskil($top,rightOK))} {
        if {!$::eskil($top,leftOK) && !$::eskil($top,rightOK)} {
            openBoth $top 0
        } elseif {!$::eskil($top,leftOK)} {
            openLeft $top
        } elseif {!$::eskil($top,rightOK)} {
            openRight $top
        }
        # If we cancel the second file and detect CVS, ask about it.
        if {$::eskil($top,leftOK) && !$::eskil($top,rightOK) && \
                [llength [glob -nocomplain [file join $fulldir CVS]]]} {

            if {[tk_messageBox -title Diff -icon question \
                    -message "Do CVS diff?" -type yesno] eq "yes"} {
                set fullname $::eskil($top,leftFile)
                set ::eskil($top,leftOK) 0
                startRevMode $top "CVS" $fullname
                after idle [list doDiff $top]
            }
        }
    }
    return $top
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
        # Skip unchanged options.
        if {[info exists ::DefaultPref($i)]} {
            if {$::DefaultPref($i) eq $Pref($i)} {
                continue
            }
        }
        puts $ch [list set Pref($i) $Pref($i)]
    }
    close $ch

    tk_messageBox -icon info -title "Saved" -message \
            "Preferences saved to:\n[file nativename $rcfile]"
}

proc getOptions {} {
    global Pref

    set Pref(fontsize) 8
    # Maybe change to TkFixedFont in 8.5 ?
    set Pref(fontfamily) Courier
    set Pref(ignore) "-b"
    set Pref(nocase) 0
    set Pref(noempty) 0
    set Pref(nodigit) 0
    set Pref(parse) 2
    set Pref(lineparsewords) 0
    set Pref(colorequal) ""
    set Pref(colorchange) red
    set Pref(colornew1) darkgreen
    set Pref(colornew2) blue
    set Pref(bgequal) ""
    set Pref(bgchange) \#ffe0e0
    set Pref(bgnew1) \#a0ffa0
    set Pref(bgnew2) \#e0e0ff
    set Pref(context) -1
    set Pref(finegrainchunks) 0
    set Pref(marklast) 1
    set Pref(linewidth) 80
    set Pref(lines) 60
    set Pref(editor) ""
    set Pref(regsub) {}
    set Pref(toolbar) 0
    set Pref(wideMap) 0 ;# Not settable in GUI yet

    # Print options
    set Pref(printHeaderSize) 10
    set Pref(printCharsPerLine) 80
    set Pref(printPaper) a4
    set Pref(printColorChange) "1.0 0.7 0.7"
    set Pref(printColorNew1)   "0.7 1.0 0.7"
    set Pref(printColorNew2)   "0.8 0.8 1.0"

    # Directory diff options
    set Pref(dir,comparelevel) 1
    set Pref(dir,ignorekey) 0
    set Pref(dir,incfiles) ""
    set Pref(dir,exfiles) "*.o"
    set Pref(dir,incdirs) ""
    set Pref(dir,exdirs) "RCS CVS .git .svn .hg"
    set Pref(dir,onlyrev) 0

    # Store default preferences, to filter saved preferences
    array set ::DefaultPref [array get Pref]

    # Backward compatibilty option
    set Pref(onlydiffs) -1

    set ::eskil(filter) ""

    if {![info exists ::eskil_testsuite] && [file exists "~/.eskilrc"]} {
        safeLoad "~/.eskilrc" Pref
    }

    if {$Pref(editor) ne ""} {
        set ::util(editor) $Pref(editor)
    }

    # If the user's file has this old option, translate it to the new
    if {$Pref(onlydiffs) == 0} {
        set Pref(context) -1
    }
    unset Pref(onlydiffs)

    # Set up reactions to some Pref settings
    if {![info exists ::widgets(toolbars)]} {
        set ::widgets(toolbars) {}
    }
    trace add variable ::Pref(toolbar) write TraceToolbar
}

proc TraceToolbar {args} {
    # FIXA: Handle destroyed windows ?
    foreach __ $::widgets(toolbars) {
        if {$::Pref(toolbar)} {
            grid configure $__
        } else {
            grid remove $__
        }
    }
}

# Global code is only run the first time to be able to reread source
if {![info exists ::eskil(gurkmeja)]} {
    set ::eskil(gurkmeja) 1

    package require pstools
    namespace import -force pstools::*
    getOptions
    if {![info exists ::eskil_testsuite]} {
        parseCommandLine
    }
}
