#----------------------------------------------------------------------
#  Eskil, Printing
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

# Format a line number for printing
# It will always be maxlen chars wide.
proc FormatLineno {lineno maxlen} {
    if {[string is integer -strict $lineno]} {
        set res [format "%d: " $lineno]
    } else {
        # Non-numerical linenumbers might turn up in some cases
        set res $lineno
        if {[string length $res] > $maxlen} {
            set res [string range $res 0 [- $maxlen 1]]
        }
    }
    if {[string length $res] < $maxlen} {
        set res [format "%*s" $maxlen $res]
    }
    return $res
}

# Process the line numbers from the line number widget into a list
# of "linestarters"
proc ProcessLineno {w maxlen} {
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
                    set formatline [FormatLineno $line $maxlen]
                    lappend lines [list $formatline $tag]
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

# Find the lastnumber in a text widget
proc FindLastNumber {w} {
    set index [$w search -backwards -regexp {\d} end]
    set line [$w get "$index linestart" "$index lineend"]
    #puts "X '$line' '$index'"
    regexp {\d+} $line number
    return $number
}

# Main print function
proc PrintDiffs {top {quiet 0} {pdfprint 0}} {
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
    if {$pdfprint && [info exists ::Pref(printCharsPerLine)]} {
        set wraplength $::Pref(printCharsPerLine)
    } elseif {$::Pref(wideLines)} {
        set wraplength 100
        set linesPerPage 74
    } else {
        set wraplength 85
        set linesPerPage 66
    }

    set tdump1 [$::widgets($top,wDiff1) dump -tag -text 1.0 end]
    set tdump2 [$::widgets($top,wDiff2) dump -tag -text 1.0 end]

    # Figure out how many chars are needed for line numbers
    set len1 [string length [FindLastNumber $::widgets($top,wLine1)]]
    set len2 [string length [FindLastNumber $::widgets($top,wLine2)]]
    # Find maximum value (at least 3)
    set maxlen [lindex [lsort -integer [list 3 $len1 $len2]] end]
    # Add space for a colon and space
    incr maxlen 2

    set lineNo1 [ProcessLineno $::widgets($top,wLine1) $maxlen]
    set lineNo2 [ProcessLineno $::widgets($top,wLine2) $maxlen]

    set linepad [string repeat " " $maxlen]

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
                        set val1 [string range $value 0 [- $wrap 1]]
                        set value [string range $value $wrap end]
                        # The newline has its own element to simplify finding
                        # it later.
                        lappend line $val1 $tag "\n" {} $linepad {}
                        set chars $maxlen
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

    if {$pdfprint} {
        PdfPrint $top $wraplength $maxlen $wraplines1 $wraplines2
    } else {
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

        if {[catch {exec {*}$enscriptCmd} result]} {
            if {[string index $result 0] != "\["} {
                tk_messageBox -message "Enscript error: $result\ncmd: $enscriptCmd"
                return
            }
        }
    }

    # Finished

    normalCursor $top
    if {!$pdfprint && !$quiet} {
        destroy .dp
        toplevel .dp
        wm title .dp "Eskil Print"
        ttk::button .dp.b -text "Close" -command {destroy .dp}
        ttk::label .dp.l -anchor w -justify left \
                -text "The following files have\
                been created:\n\n$tmpFile\nInput file to enscript.\
                \n\n$tmpFile2\nCreated with\
                '[lrange $enscriptCmd 0 end-3] \\\n             \
                [lrange $enscriptCmd end-2 end]'" \
                -font "Courier 8"
        pack .dp.b -side bottom
        pack .dp.l -side "top"
    }
}

proc PdfPrint {top cpl cpln wraplines1 wraplines2} {

    if {$::diff($top,printFile) != ""} {
        set pdfFile $::diff($top,printFile)
    } else {
        set pdfFile ~/eskil.pdf
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

    set pdf [eskilprint %AUTO% -file $pdfFile -cpl $cpl -cpln $cpln \
                     -headleft $lfile -headright $rfile -headsize 10]
    set linesPerPage [$pdf getNLines]
    $pdf setTag change $::Pref(printColorChange)
    $pdf setTag new1   $::Pref(printColorNew1)
    $pdf setTag new2   $::Pref(printColorNew2)

    set len1 [llength $wraplines1]
    set len2 [llength $wraplines2]

    set max [expr {$len1 > $len2 ? $len1 : $len2}]
    set npages [expr {($max + $linesPerPage - 1) / $linesPerPage}]
    $pdf configure -headnpages $npages

    set i1 0
    set i2 0

    while {$i1 < $len1 && $i2 < $len2} {
        $pdf newPage
        $pdf setHalf left
        for {set i 0} {$i < $linesPerPage && $i1 < $len1} {incr i ; incr i1} {
            $pdf drawTextLine [lindex $wraplines1 $i1]
            $pdf newLine
        }
        $pdf setHalf right
        for {set i 0} {$i < $linesPerPage && $i2 < $len2} {incr i ; incr i2} {
            $pdf drawTextLine [lindex $wraplines2 $i2]
            $pdf newLine
        }
    }
    $pdf endPrint
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

    ttk::label .pr.l1 -justify left -anchor w \
            -text "The print function is just on an\
            experimental level. It will use 'enscript' to write a postcript\
            file \"eskil.ps\" in your home directory."
    ttk::label .pr.l2 -justify left -anchor w \
            -text "Below you can adjust the gray scale\
            levels that are used on the background to mark changes.\
            The first value is used for changed text. The second for\
            new/deleted text."
    .pr.l1 configure -wraplength 400
    .pr.l2 configure -wraplength 400

    ttk::scale .pr.s1 -orient horizontal -from 0.0 \
            -to 1.0 -variable Pref(grayLevel1)
    ttk::scale .pr.s2 -orient horizontal -from 0.0 \
            -to 1.0 -variable Pref(grayLevel2)
    ttk::frame .pr.f
    ttk::radiobutton .pr.r1 -text "No Syntax" -variable diff(prettyPrint) \
            -value ""
    ttk::radiobutton .pr.r2 -text "VHDL" -variable diff(prettyPrint) \
            -value "vhdl"
    ttk::radiobutton .pr.r3 -text "Tcl"  -variable diff(prettyPrint) \
            -value "tcl"
    ttk::radiobutton .pr.r4 -text "C"    -variable diff(prettyPrint) \
            -value "c"

    ttk::frame .pr.fs
    ttk::radiobutton .pr.fs.r1 -text "80 char" -variable Pref(wideLines) \
            -value 0
    ttk::radiobutton .pr.fs.r2 -text "95 char" -variable Pref(wideLines) \
            -value 1
    pack .pr.fs.r1 .pr.fs.r2 -side left -padx 10

    ttk::button .pr.b1 -text "Print to File" \
            -command "destroy .pr; update; PrintDiffs $top"
    ttk::button .pr.b2 -text "Cancel" -command {destroy .pr}

    grid .pr.l1 - - -sticky we
    grid .pr.l2 - - -sticky we
    grid .pr.s1 - - -sticky we
    grid .pr.s2 - - -sticky we
    grid .pr.f  - - -sticky we
    grid .pr.fs - - -sticky we
    grid .pr.b1 x .pr.b2 -sticky we -padx 5 -pady 5 -ipadx 5
    grid columnconfigure .pr {0 2} -uniform a
    grid columnconfigure .pr 1 -weight 1
    pack .pr.r1 .pr.r2 .pr.r3 .pr.r4 -in .pr.f -side left -fill x -expand 1

}

# Count the length of a line during a text dump
proc AccumulateMax {key value index} {
    set index [lindex [split $index "."] 1]
    set len [expr {[string length $value] + $index - 1}]
    if {$len > $::diff(currentCharsPerLine)} {
        set ::diff(currentCharsPerLine) $len
    }
}

# Count the longest line length in the current display
proc CountCharsPerLine {top} {
    set ::diff(currentCharsPerLine) 0
    $::widgets($top,wDiff1) dump -text -command AccumulateMax 1.0 end
    $::widgets($top,wDiff2) dump -text -command AccumulateMax 1.0 end
    return $::diff(currentCharsPerLine)
}

proc BrowsePrintFileName {top entry} {
    set prev $::diff($top,printFile)
    set dir [file dirname $prev]

    set apa [tk_getSaveFile -initialdir $dir -initialfile [file tail $prev] \
                     -parent [winfo toplevel $entry] -title "PDF file"]
    if {$apa ne ""} {
        set ::diff($top,printFile) $apa
        $entry xview end
    }
}

# Create a print dialog for PDF.
proc doPrint2 {top {quiet 0}} {
    if {$quiet} {
        PrintDiffs $top 1 1
        return
    }

    destroy .pr
    toplevel .pr -padx 3 -pady 3
    wm title .pr "Print diffs to PDF"

    ttk::label .pr.hsl -anchor w -text "Header Size"
    spinbox .pr.hss -textvariable ::Pref(printHeaderSize) \
        -from 5 -to 16 -width 3

    ttk::label .pr.cll -anchor w -text "Chars per line"
    ttk::entryX .pr.cle -textvariable ::Pref(printCharsPerLine) -width 4
    ttk::frame .pr.clf
    set values [list 80]
    set cpl [CountCharsPerLine $top]
    if {$cpl != 0} {
        lappend values $cpl
    }
    if {[string is digit -strict $::Pref(printCharsPerLine)]} {
        lappend values $::Pref(printCharsPerLine)
    }
    set values [lsort -unique -integer $values]
    foreach value $values {
        ttk::radiobutton .pr.clf.$value -variable ::Pref(printCharsPerLine) \
            -value $value -text $value
        pack .pr.clf.$value -side left -padx 3 -pady 3
    }

    # Select paper size
    set paperlist [lsort -dictionary [pdf4tcl::getPaperSizeList]]
    ttk::label .pr.psl -anchor w -text "Paper Size"
    ttk::combobox .pr.psc -values $paperlist -textvariable ::Pref(printPaper) \
            -width 6 -state readonly

    # FIXA: Select colours
    #set Pref(printColorChange) "1.0 0.6 0.6"
    #set Pref(printColorNew1) "0.6 1.0 0.6"
    #set Pref(printColorNew2) "0.6 0.6 1.0"

    ttk::label .pr.fnl -anchor w -text "File name"
    ttk::entryX .pr.fne -textvariable ::diff($top,printFile) -width 30
    ttk::button .pr.fnb -text Browse \
            -command [list BrowsePrintFileName $top .pr.fne]

    if {$::diff($top,printFile) eq ""} {
        set ::diff($top,printFile) "~/eskil.pdf"
    }

    ttk::frame .pr.fb
    ttk::button .pr.b1 -text "Print to File" \
            -command "destroy .pr; update; PrintDiffs $top 0 1"
    ttk::button .pr.b2 -text "Cancel" -command {destroy .pr}
    pack .pr.b1 -in .pr.fb -side left  -padx 3 -pady 3 -ipadx 5
    pack .pr.b2 -in .pr.fb -side right -padx 3 -pady 3 -ipadx 5

    grid .pr.hsl .pr.hss         -sticky we -padx 3 -pady 3
    grid .pr.psl .pr.psc         -sticky we -padx 3 -pady 3
    grid .pr.cll .pr.cle .pr.clf -sticky we -padx 3 -pady 3
    grid .pr.fnl .pr.fne - .pr.fnb -sticky we -padx 3 -pady 3
    grid .pr.fb  -       - -       -sticky we -padx 3 -pady 3

    grid columnconfigure .pr 2 -weight 1
}

