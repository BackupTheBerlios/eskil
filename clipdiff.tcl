#!/bin/sh
# the next line restarts using wish \
exec wish "$0" "$@"

set thisscript [file join [pwd] [info script]]
set thisdir [file dirname $thisscript]
set diffpath [file join $thisdir diff.tcl]
set debug 1

if {$tcl_platform(platform) == "windows"} {
    package require dde
}

proc doDiff {} {
    set f1 [file join $::thisdir clipdiffleft.tmp]
    set f2 [file join $::thisdir clipdiffright.tmp]

    set ch [open $f1 w]
    puts $ch [string trimright [.t1 get 1.0 end] \n]
    close $ch
    set ch [open $f2 w]
    puts $ch [string trimright [.t2 get 1.0 end] \n]
    close $ch

    remoteDiff $f1 $f2
}    

proc remoteDiff {file1 file2} {
    set cmd [list remoteDiff $file1 $file2]

    if {$::tcl_platform(platform) == "unix"} {
#	send -async Diff $cmd
        exec [info nameofexecutable] $::diffpath $file1 $file2 &
    } else {
	if {[catch {dde eval -async Diff $cmd}]} {
	    catch {exec [info nameofexecutable] $::diffpath -server &}
	    after 500
#	    catch {dde eval -async Diff $cmd}
	    dde eval -async Diff $cmd
	}
    }
}

proc makeWin {} {
    eval destroy [winfo children .]
    text .t1 -width 60 -height 35 \
	    -yscrollcommand {.sby1 set} -xscrollcommand {.sbx1 set}
    text .t2 -width 60 -height 35 \
	    -yscrollcommand {.sby2 set} -xscrollcommand {.sbx2 set}
    scrollbar .sbx1 -orient horiz -command {.t1 xview}
    scrollbar .sbx2 -orient horiz -command {.t2 xview}
    scrollbar .sby1 -orient vert  -command {.t1 yview}
    scrollbar .sby2 -orient vert  -command {.t2 yview}

    bind .t1 <Control-o> {focus .t2}
    bind .t2 <Control-o> {focus .t1}

    frame .f
    button .b -text Diff -command doDiff
    button .b2 -text "Left Clear" -command {.t1 delete 1.0 end}
    button .b3 -text "Right Clear" -command {.t2 delete 1.0 end}
    button .b4 -text "Left Clear&Paste" -command {.t1 delete 1.0 end ; event generate .t1 <<Paste>>}
    button .b5 -text "Right Clear&Paste" -command {.t2 delete 1.0 end ; event generate .t2 <<Paste>>}

    pack .b .b2 .b3 .b4 .b5 -in .f -side left

    if {$::debug == 1} {
        menubutton .md -text Debug -menu .md.m -relief ridge
        menu .md.m
        if {$::tcl_platform(platform) == "windows"} {
            .md.m add checkbutton -label Console -variable consolestate \
                    -onvalue show -offvalue hide -command {console $consolestate}
            .md.m add separator
        }
        .md.m add command -label "Stack trace" -command {bgerror Debug}
        .md.m add separator
        .md.m add command -label "Reread Source" -command {source $thisscript}
        .md.m add separator
        .md.m add command -label "Redraw Window" -command {makeWin}
        
        pack .md -in .f -side left
    }

    grid .f    -     -     -     -sticky w
    grid .t1   .sby1 .t2   .sby2 -sticky news
    grid .sbx1 x     .sbx2 x     -sticky we
    grid rowconfigure . 1 -weight 1
    grid columnconfigure . {0 2} -weight 1
}

if {![winfo exists .t1]} {
    makeWin
    update idletasks
}
