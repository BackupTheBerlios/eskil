#!/bin/sh
# the next line restarts using wish \
exec wish "$0" "$@"

set thisScript [file join [pwd] [info script]]

proc busyCursor {} {
    global oldcursor oldcursor2
    if {![info exists oldcursor]} {
        set oldcursor [. cget -cursor]
        set oldcursor2 [.e1 cget -cursor]
    }
    . config -cursor watch
    foreach w {.e1 .e2} {
	$w config -cursor watch
    }
}

proc normalCursor {} {
    global oldcursor oldcursor2
    . config -cursor $oldcursor
    foreach w {.e1 .e2} {
	$w config -cursor $oldcursor2
    }
}

proc browse {varName} {
    upvar $varName file

    if {$file == ""} {
	set initdir [pwd]
    } else {
	set initdir [file dirname $file]
    }
    set apa [tk_getOpenFile -title "Select file" -initialdir $initdir]
    if {$apa != ""} {
	set file [file join $initdir $apa]
	cd [file dirname $file]
    }
}

proc doComp {{extra 0}} {
    global compRes file1 file2

    busyCursor
    update idletasks

    file stat $file1 stat1
    file stat $file2 stat2

    set compRes ""
    if {$stat1(size) == $stat2(size) && $stat1(mtime) == $stat2(mtime)} {
	set compRes "Size&Time "
    }

    update idletasks

    set eqbut 0
    set bufsz 65536
    set eq 1
    set ch1 [open $file1 r]
    set ch2 [open $file2 r]
    fconfigure $ch1 -translation binary
    fconfigure $ch2 -translation binary
    while {![eof $ch1] && ![eof $ch2]} {
	set f1 [read $ch1 $bufsz]
	set f2 [read $ch2 $bufsz]
	if {![string equal $f1 $f2]} {
	    set eq 0
	    set len1 [string length $f1]
	    set len2 [string length $f2]
	    if {$len1 != $len2} {
		set len [expr {$len1 < $len2 ? $len1 : $len2}]
		if {[string equal -length $len $f1 $f2]} {
		    set eqbut [expr {$len1 < $len2 ? 2 : 1}]
		}
	    }
	    break
	}
    }
    if {([eof $ch1] + [eof $ch2]) < 2} {
	set eq 0
    }
    close $ch1
    close $ch2

    if {$eq} {
	append compRes Equal
    } else {
	append compRes "Not Equal"
    }

    if {$eqbut} {
	append compRes " but [expr {abs($stat1(size) - $stat2(size))}]($eqbut)"
    }
    
    if {!$extra || $eq || $eqbut} {
	normalCursor
	return
    }

    update idletasks
    set ch1 [open $file1 r]
    set ch2 [open $file2 r]
    fconfigure $ch1 -translation binary -buffersize 524288
    fconfigure $ch2 -translation binary -buffersize 524288
    set data1 [read $ch1]
    set data2 [read $ch2]
    close $ch1
    close $ch2
    set len1 [string length $data1]
    set len2 [string length $data2]
    
    if {$len1 < 2000 || $len2 < 2000} {
	normalCursor
	return
    }
    
    set mid1 [expr {$len1 / 2 - 500}]
    set midstr1 [string range $data1 $mid1 [expr {$mid1 + 999}]]
    set places {}
    for {set i2 0} {$i2 < $len2} {incr i2} {
	set i2 [string first $midstr1 $data2 $i2]
	if {$i2 == -1} break
	lappend places $i2
    }
    if {[llength $places] > 1} {
	append compRes " multiple parts"
    } elseif {[llength $places] == 1} {
	set i2 [lindex $places 0]
	append compRes " s"
	if {$mid1 < $i2} {
	    set start1 0
	    set start2 [expr {$i2 - $mid1}]
	} else {
	    set start1 [expr {$mid1 - $i2}]
	    set start2 0
	}
	if {($len1 - $mid1) > ($len2 - $i2)} {
	    set end1 [expr {$mid1 + ($len2 - $i2) - 1}]
	    set end2 [expr {$len2 - 1}]
	} else {
	    set end1 [expr {$len1 - 1}]
	    set end2 [expr {$i2 + ($len1 - $mid1) - 1}]
	}
	if {$end2 - $start2 != $end1 - $start1} {
	    append compRes " ($mid1=$i2 '$start1-$end1' '$start2-$end2')"
	}
	for {set s1 $start1 ; set s2 $start2} {$s1 < $mid1} {incr s1 1000 ; incr s2 1000} {
	    if {[string equal [string range $data1 $s1 [expr {$s1 + 999}]] \
		    [string range $data2 $s2 [expr {$s2 + 999}]]]} {
		break
	    }
	}
	for {set e1 $end1 ; set e2 $end2} {$e1 > $mid1} {incr e1 -1000 ; incr e2 -1000} {
	    if {[string equal [string range $data1 [expr {$e1 - 999}] $e1] \
		    [string range $data2 [expr {$e2 - 999}] $e2]]} {
		break
	    }
	}
	set eql [expr {$e1 - $s1 + 1}]
	append compRes " '$s1 - $e1' == '$s2 - $e2' ($eql)($len1)($len2)"
    } else {
	append compRes " no"
    }
    normalCursor
}

# File drop using TkDnd
proc fileDrop {var files} {
    set $var [lindex $files 0]
}

proc makeWin {} {
    global tcl_platform
    eval destroy [winfo children .]

    frame .fm

    menubutton .md -text Debug -menu .md.m -relief ridge
    menu .md.m
    if {$tcl_platform(platform) == "windows"} {
	.md.m add checkbutton -label Console -variable consolestate \
		-onvalue show -offvalue hide -command {console $consolestate}
	.md.m add separator
    }
    .md.m add command -label "Stack trace" -command {bgerror Debug}
    .md.m add separator
    .md.m add command -label "Reread Source" -command {source $thisScript}
    .md.m add separator
    .md.m add command -label "Redraw Window" -command {makeWin}
    .md.m add separator
    .md.m add command -label "Extra Comp" -command {doComp 1}
    
    pack .md -in .fm -side left

    button .bd -text Comp -command doComp
    label .l -textvariable compRes

    entry .e1 -width 50 -textvariable file1
    entry .e2 -width 50 -textvariable file2
    button .b1 -text Browse -command "browse file1"
    button .b2 -text Browse -command "browse file2"

    # Set up file dropping in entries if TkDnd is available
    if {![catch {package require tkdnd}]} {
        dnd bindtarget .e1 text/uri-list <Drop> {fileDrop ::file1 %D}
        dnd bindtarget .e2 text/uri-list <Drop> {fileDrop ::file2 %D}
    }

    grid .fm .l .bd -sticky wns
    grid .e1 -  .b1 -sticky news
    grid .e2 -  .b2 -sticky news
    grid .l .bd -sticky news
    grid columnconfigure . 1 -weight 1
}

if {![winfo exists .fm]} {
    makeWin
}
