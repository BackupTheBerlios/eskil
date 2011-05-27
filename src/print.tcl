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

# Find the lastnumber in a text widget
proc FindLastNumber {w} {
    set index [$w search -backwards -regexp {\d} end]
    set line [$w get "$index linestart" "$index lineend"]
    #puts "X '$line' '$index'"
    regexp {\d+} $line number
    return $number
}

# Main print function
proc PrintDiffs {top {quiet 0}} {
    busyCursor $top
    update idletasks

    set lines1 {}
    set lines2 {}
    if {[info exists ::Pref(printCharsPerLine)]} {
        set wraplength $::Pref(printCharsPerLine)
    } else {
        set wraplength 85
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

    PdfPrint $top $wraplength $maxlen $wraplines1 $wraplines2

    # Finished

    normalCursor $top
}

proc PdfPrint {top cpl cpln wraplines1 wraplines2} {

    if {$::eskil($top,printFile) != ""} {
        set pdfFile $::eskil($top,printFile)
    } else {
        set pdfFile ~/eskil.pdf
    }

    if {![regexp {^(.*)( \(.*?\))$} $::eskil($top,leftLabel) -> lfile lrest]} {
        set lfile $::eskil($top,leftLabel)
        set lrest ""
    }
    set lfile [file tail $lfile]$lrest
    if {![regexp {^(.*)( \(.*?\))$} $::eskil($top,rightLabel) -> rfile rrest]} {
        set rfile $::eskil($top,rightLabel)
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

# Count the length of a line during a text dump
proc AccumulateMax {key value index} {
    set index [lindex [split $index "."] 1]
    set len [expr {[string length $value] + $index - 1}]
    if {$len > $::eskil(currentCharsPerLine)} {
        set ::eskil(currentCharsPerLine) $len
    }
}

# Count the longest line length in the current display
proc CountCharsPerLine {top} {
    set ::eskil(currentCharsPerLine) 0
    $::widgets($top,wDiff1) dump -text -command AccumulateMax 1.0 end
    $::widgets($top,wDiff2) dump -text -command AccumulateMax 1.0 end
    return $::eskil(currentCharsPerLine)
}

proc BrowsePrintFileName {top entry} {
    set prev $::eskil($top,printFile)
    set dir [file dirname $prev]

    set apa [tk_getSaveFile -initialdir $dir -initialfile [file tail $prev] \
                     -parent [winfo toplevel $entry] -title "PDF file"]
    if {$apa ne ""} {
        set ::eskil($top,printFile) $apa
        $entry xview end
    }
}

# Create a print dialog for PDF.
proc doPrint {top {quiet 0}} {
    if {$quiet} {
        PrintDiffs $top 1
        return
    }

    destroy .pr
    toplevel .pr -padx 3 -pady 3
    wm title .pr "Print diffs to PDF"

    # Layout

    ttk::label .pr.hsl -anchor w -text "Header Size"
    tk::spinbox .pr.hss -textvariable ::Pref(printHeaderSize) \
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
    
    # Color
    foreach {::TmpPref(chr) ::TmpPref(chg) ::TmpPref(chb)} \
            $::Pref(printColorChange) break
    foreach {::TmpPref(n1r) ::TmpPref(n1g) ::TmpPref(n1b)} \
            $::Pref(printColorNew1) break
    foreach {::TmpPref(n2r) ::TmpPref(n2g) ::TmpPref(n2b)} \
            $::Pref(printColorNew2) break
    trace add variable ::TmpPref write {
        set ::Pref(printColorChange) [list $::TmpPref(chr) $::TmpPref(chg) $::TmpPref(chb)]
        set ::Pref(printColorNew1)   [list $::TmpPref(n1r) $::TmpPref(n1g) $::TmpPref(n1b)]
        set ::Pref(printColorNew2)   [list $::TmpPref(n2r) $::TmpPref(n2g) $::TmpPref(n2b)]
    list}

    ttk::labelframe .pr.cf -text "Color" -padding 3

    ttk::label .pr.cf.l1 -text "Change"
    tk::spinbox .pr.cf.s1r -from 0.0 -to 1.0 -increment 0.1 -format %.1f \
            -width 4 -textvariable ::TmpPref(chr)
    tk::spinbox .pr.cf.s1g -from 0.0 -to 1.0 -increment 0.1 -format %.1f \
            -width 4 -textvariable ::TmpPref(chg)
    tk::spinbox .pr.cf.s1b -from 0.0 -to 1.0 -increment 0.1 -format %.1f \
            -width 4 -textvariable ::TmpPref(chb)

    ttk::label .pr.cf.l2 -text "Old"
    tk::spinbox .pr.cf.s2r -from 0.0 -to 1.0 -increment 0.1 -format %.1f \
            -width 4 -textvariable ::TmpPref(n1r)
    tk::spinbox .pr.cf.s2g -from 0.0 -to 1.0 -increment 0.1 -format %.1f \
            -width 4 -textvariable ::TmpPref(n1g)
    tk::spinbox .pr.cf.s2b -from 0.0 -to 1.0 -increment 0.1 -format %.1f \
            -width 4 -textvariable ::TmpPref(n1b)

    ttk::label .pr.cf.l3 -text "New"
    tk::spinbox .pr.cf.s3r -from 0.0 -to 1.0 -increment 0.1 -format %.1f \
            -width 4 -textvariable ::TmpPref(n2r)
    tk::spinbox .pr.cf.s3g -from 0.0 -to 1.0 -increment 0.1 -format %.1f \
            -width 4 -textvariable ::TmpPref(n2g)
    tk::spinbox .pr.cf.s3b -from 0.0 -to 1.0 -increment 0.1 -format %.1f \
            -width 4 -textvariable ::TmpPref(n2b)

    grid .pr.cf.l1 .pr.cf.s1r .pr.cf.s1g .pr.cf.s1b -sticky w -padx 3 -pady 3
    grid .pr.cf.l2 .pr.cf.s2r .pr.cf.s2g .pr.cf.s2b -sticky w -padx 3 -pady 3
    grid .pr.cf.l3 .pr.cf.s3r .pr.cf.s3g .pr.cf.s3b -sticky w -padx 3 -pady 3

    # File

    ttk::label .pr.fnl -anchor w -text "File name"
    ttk::entryX .pr.fne -textvariable ::eskil($top,printFile) -width 30
    ttk::button .pr.fnb -text Browse \
            -command [list BrowsePrintFileName $top .pr.fne]

    if {$::eskil($top,printFile) eq ""} {
        set ::eskil($top,printFile) "~/eskil.pdf"
    }

    ttk::frame .pr.fb
    ttk::button .pr.b1 -text "Print to File" \
            -command "destroy .pr; update; PrintDiffs $top"
    ttk::button .pr.b2 -text "Cancel" -command {destroy .pr}
    pack .pr.b1 -in .pr.fb -side left  -padx 3 -pady 3 -ipadx 5
    pack .pr.b2 -in .pr.fb -side right -padx 3 -pady 3 -ipadx 5

    grid .pr.hsl .pr.hss         -sticky we -padx 3 -pady 3
    grid .pr.psl .pr.psc         -sticky we -padx 3 -pady 3
    grid .pr.cll .pr.cle .pr.clf -sticky we -padx 3 -pady 3
    grid .pr.cf  -       - -     -sticky w  -padx 3 -pady 3
    grid .pr.fnl .pr.fne - .pr.fnb -sticky we -padx 3 -pady 3
    grid .pr.fb  -       - -       -sticky we -padx 3 -pady 3

    grid columnconfigure .pr 2 -weight 1
}

