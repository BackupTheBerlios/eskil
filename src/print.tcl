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

# Format a line number for printing
# It will always be 5 chars wide.
proc FormatLineno {lineno gray} {
    if {[string is integer -strict $lineno]} {
        set res [format "%3d: " $lineno]
    } else {
        # Non-numerical linenumbers might turn up in some cases
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
proc ProcessLineno {w} {
    set tdump [$w dump -tag -text 1.0 end]
    set gray 1.0
    set line ""
    set lines {}
    foreach {key value index} $tdump {
        if {$key eq "tagon"} {
            if {$value eq "change"} {
                set gray $::Pref(grayLevel1)
            } elseif {[string match "new*" $value]} {
                set gray $::Pref(grayLevel2)
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
                    lappend lines [FormatLineno $line $gray]
                }
                set line ""
            }
        }
    }
    return $lines
}

# Handle wrapping of a too long line for printing
# The indentation of the wrapped line is 5 chars, same as a line number.
proc LineWrap {gray} {
    if {$gray eq "1.0"} {
        return "\n     "
    } else {
        return "\0bggray\{1.0\}\n     \0bggray\{$gray\}"
    }
}

# Prepare a text block for printing
proc FixTextBlock {text index} {
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

    foreach tdump [list $tdump1 $tdump2] \
            lineName {lines1 lines2} wrapName {wrap1 wrap2} \
            lineNo [list $lineNo1 $lineNo2] {
        ##nagelfar variable lineName varName
        ##nagelfar variable wrapName varName
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
                        append line $val1
                        append line [LineWrap $gray]
                        set chars 5
                        incr wrapc
                        set len [string length $value]
                    }
                    append line $value
                    incr chars $len
                }
                tagon {
                    if {$value eq "change"} {
                        set gray $::Pref(grayLevel1)
                        append line "\0bggray\{$gray\}"
                    } elseif {$value != "last"} {
                        set gray $::Pref(grayLevel2)
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
        for {set i 0} {$i < $linesPerPage && $i1 < $len1} {incr i ; incr i1} {
            puts $ch [lindex $wraplines1 $i1]
        }
        if {$i < $linesPerPage} {puts -nonewline $ch "\f"}
        for {set i 0} {$i < $linesPerPage && $i2 < $len2} {incr i ; incr i2} {
            puts $ch [lindex $wraplines2 $i2]
        }
        if {$i < $linesPerPage} {puts -nonewline $ch "\f"}
    }

    close $ch

    if {$::tcl_platform(platform) eq "windows" &&\
            ![info exists ::env(ENSCRIPT_LIBRARY)]} {
        set ::env(ENSCRIPT_LIBRARY) [pwd]
    }
    set enscriptCmd [list enscript -2jcre -L $linesPerPage -M A4]
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
