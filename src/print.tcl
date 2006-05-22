#----------------------------------------------------------------------
#  Eskil, Printing
#
#  Copyright (c) 1998-2005, Peter Spjuth  (peter.spjuth@space.se)
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

# Helpers, replace with sugar macros or mathops if available in 8.5
proc + {a b} { expr {$a + $b} }
proc - {a b} { expr {$a - $b} }

# Format a line number for printing
# It will always be 5 chars wide.
proc FormatLineno {lineno} {
    if {[string is integer -strict $lineno]} {
        set res [format "%3d: " $lineno]
    } else {
        # Non-numerical linenumbers might turn up in some cases
        set res [format "%-5s" $lineno]
    }
    if {[string length $res] > 5} {
        set res [string range $res end-5 end-1]
    }
    return $res
}

# Process the line numbers from the line number widget into a list
# of "linestarters"
proc ProcessLineno {w} {
    set tdump [$w dump -tag -text 1.0 end]
    set tag ""
    set line ""
    set lines {}
    foreach {key value index} $tdump {
        if {$key eq "tagon"} {
            if {$value eq "change" || [string match "new*" $value]} {
                set tag $value
            }
        } elseif {$key eq "tagoff"} {
            if {$value eq "change" || [string match "new*" $value]} {
                set tag ""
            }
        } elseif {$key eq "text"} {
            append line $value
            # Collect until end of line
            if {[string index $value end] eq "\n"} {
                # Clean everything but the line number
                set line [string trim [string trim $line] :]
                if {$line eq ""} {
                    lappend lines {}
                } else {
                    lappend lines [list [FormatLineno $line] $tag]
                }
                set line ""
            }
        }
    }
    return $lines
}

# Prepare a text block for printing
# Index denotes where in the text widget this text starts. It is used to get
# tab expansion right.
proc FixTextBlock {text index} {
    # Remove any form feed
    set text [string map {\f {}} $text]

    # Extract column number from index
    regexp {\d+\.(\d+)} $index -> index

    # Expand tabs to 8 chars
    while 1 {
        set i [string first \t $text]
        if {$i == -1} break
        set n [expr {(- $i - $index - 1) % 8 + 1}]
        set text [string replace $text $i $i [format %*s $n ""]]
    }
    return $text
}

# Format a line of text/tag pairs to enscript code
proc FormatLine {line} {
    set result ""
    foreach {text tag} $line {
        if {$tag eq ""} {
            append result $text
        } else {
            if {$tag eq "change"} {
                set gray $::Pref(grayLevel1)
            } elseif {[string match "new*" $tag]} {
                set gray $::Pref(grayLevel2)
            } else {
                # Should not happen
                set gray 1.0
                puts stderr "Bad tag in FormatLine: '$tag'"
            }
            append result "\0bggray\{$gray\}$text\0bggray\{1.0\}"
        }
    }
    return $result
}

# Main print function
proc PrintDiffs {top {quiet 0}} {
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
    if {$::Pref(wideLines)} {
        set wraplength 100
        set linesPerPage 74
    } else {
        set wraplength 85
        set linesPerPage 66
    }

    set tdump1 [$::widgets($top,wDiff1) dump -tag -text 1.0 end]
    set tdump2 [$::widgets($top,wDiff2) dump -tag -text 1.0 end]
    set lineNo1 [ProcessLineno $::widgets($top,wLine1)]
    set lineNo2 [ProcessLineno $::widgets($top,wLine2)]

    # Loop over left and right displays, collecting lines from each.
    # Line numbers and text are put together and lines are wrapped if needed.

    foreach tdump [list $tdump1 $tdump2] \
            lineName {lines1 lines2} wrapName {wrap1 wrap2} \
            lineNo [list $lineNo1 $lineNo2] {
        ##nagelfar variable lineName varName
        ##nagelfar variable wrapName varName
        set lines {}
        set wraps {}
        set line [lindex $lineNo 0]
        set newline 0
        set tag   {}
        set chars 0
        set wrapc 0
        foreach {key value index} $tdump {
            if {$key != "tagoff" && $newline == 1} {
                lappend lines $line
                lappend wraps $wrapc
                set newline 0
                set line [lindex $lineNo [llength $lines]]
                set chars 0
                set wrapc 0
            }
            switch $key {
                text {
                    set value [FixTextBlock $value $index]
                    if {[string index $value end] eq "\n"} {
                        set newline 1
                        set value [string trimright $value "\n"]
                    }
                    set len [string length $value]
                    while {$chars + $len > $wraplength} {
                        set wrap [expr {$wraplength - $chars}]
                        set val1 [string range $value 0 [expr {$wrap - 1}]]
                        set value [string range $value $wrap end]
                        # The newline has its own element to simplify finding
                        # it later.
                        lappend line $val1 $tag "\n" {} "     " {}
                        set chars 5
                        incr wrapc
                        set len [string length $value]
                    }
                    lappend line $value $tag
                    incr chars $len
                }
                tagon {
                    if {$value eq "change" || [string match "new*" $value]} {
                        set tag $value
                    }
                }
                tagoff {
                    if {$value eq "change" || [string match "new*" $value]} {
                        set tag {}
                    }
                }
            }
        }
        set $lineName $lines
        set $wrapName $wraps
    }

    # Go through both lists and put each wrapped line as one element.
    # Pad with empty lines as needed to accomodate for wrapped lines
    # in the other side.

    set wraplines1 {}
    set wraplines2 {}

    foreach l1 $lines1 l2 $lines2 w1 $wrap1 w2 $wrap2 {
        if {$w1 > 0} {
            while {[set i [lsearch $l1 "\n"]] >= 0} {
                lappend wraplines1 [lrange $l1 0 [- $i 1]]
                set l1 [lrange $l1 [+ $i 2] end]
            }
        }
        lappend wraplines1 $l1

        if {$w2 > 0} {
            while {[set i [lsearch $l2 "\n"]] >= 0} {
                lappend wraplines2 [lrange $l2 0 [- $i 1]]
                set l2 [lrange $l2 [+ $i 2] end]
            }
        }
        lappend wraplines2 $l2

        if {$w1 > $w2} {
            for {set t $w2} {$t < $w1} {incr t} {
                lappend wraplines2 {}
            }
        } elseif {$w2 > $w1} {
            for {set t $w1} {$t < $w2} {incr t} {
                lappend wraplines1 {}
            }
        }
    }

    # Write all lines to a file, taking one page at a time from each
    # side.

    set ch [open $tmpFile "w"]
    fconfigure $ch -encoding binary

    set len1 [llength $wraplines1]
    set len2 [llength $wraplines2]

    set i1 0
    set i2 0

    while {$i1 < $len1 && $i2 < $len2} {
        for {set i 0} {$i < $linesPerPage && $i1 < $len1} {incr i ; incr i1} {
            puts $ch [FormatLine [lindex $wraplines1 $i1]]
        }
        if {$i < $linesPerPage} {puts -nonewline $ch "\f"}
        for {set i 0} {$i < $linesPerPage && $i2 < $len2} {incr i ; incr i2} {
            puts $ch [FormatLine [lindex $wraplines2 $i2]]
        }
        if {$i < $linesPerPage} {puts -nonewline $ch "\f"}
    }

    close $ch

    # Run enscript to generate postscript

    if {$::tcl_platform(platform) eq "windows" &&\
            ![info exists ::env(ENSCRIPT_LIBRARY)]} {
        set ::env(ENSCRIPT_LIBRARY) [pwd]
    }
    if {[auto_execok enscript.bin] ne ""} {
        set enscriptCmd [list enscript.bin]
    } else {
        set enscriptCmd [list enscript]
    }

    lappend enscriptCmd -2jcre -L $linesPerPage -M A4

    if {$::Pref(wideLines)} {
        lappend enscriptCmd  -f Courier6
    }
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
    if {$::diff(prettyPrint) != ""} {
        lappend enscriptCmd -E$::diff(prettyPrint)
    }
    lappend enscriptCmd -p $tmpFile2 $tmpFile

    if {[catch {eval exec $enscriptCmd} result]} {
        if {[string index $result 0] != "\["} {
            tk_messageBox -message "Enscript error: $result\ncmd: $enscriptCmd"
            return
        }
    }

    # Finished

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
    if {![info exists ::diff(prettyPrint)]} {
        set ::diff(prettyPrint) ""
    }
    if {$quiet} {
        PrintDiffs $top 1
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
            -to 1.0 -variable Pref(grayLevel1)
    scale .pr.s2 -orient horizontal -resolution 0.1 -showvalue 1 -from 0.0 \
            -to 1.0 -variable Pref(grayLevel2)
    frame .pr.f
    radiobutton .pr.r1 -text "No Syntax" -variable diff(prettyPrint) -value ""
    radiobutton .pr.r2 -text "VHDL" -variable diff(prettyPrint) -value "vhdl"
    radiobutton .pr.r3 -text "Tcl"  -variable diff(prettyPrint) -value "tcl"
    radiobutton .pr.r4 -text "C"    -variable diff(prettyPrint) -value "c"

    frame .pr.fs
    radiobutton .pr.fs.r1 -text "80 char" -variable Pref(wideLines) -value 0
    radiobutton .pr.fs.r2 -text "95 char" -variable Pref(wideLines) -value 1
    pack .pr.fs.r1 .pr.fs.r2 -side left -padx 10

    button .pr.b1 -text "Print to File" -padx 5\
            -command "destroy .pr; update; PrintDiffs $top"
    button .pr.b2 -text "Cancel" -padx 5 \
            -command {destroy .pr}

    grid .pr.l1 - - -sticky we
    grid .pr.l2 - - -sticky we
    grid .pr.s1 - - -sticky we
    grid .pr.s2 - - -sticky we
    grid .pr.f  - - -sticky we
    grid .pr.fs - - -sticky we
    grid .pr.b1 x .pr.b2 -sticky we -padx 5 -pady 5
    grid columnconfigure .pr {0 2} -uniform a
    grid columnconfigure .pr 1 -weight 1
    pack .pr.r1 .pr.r2 .pr.r3 .pr.r4 -in .pr.f -side left -fill x -expand 1

}
