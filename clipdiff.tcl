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

proc doClipDiff {} {
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

proc makeClipDiffWin {{redraw 0}} {
    global diff

    set top .clipdiff
    if {[winfo exists $top] && [winfo toplevel $top] == $top} {
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
        lappend diff(diffWindows) $top
    }
    wm title $top "Clip Diff"
    wm protocol $top WM_DELETE_WINDOW exit
    text $top.t1 -width 60 -height 35 \
	    -yscrollcommand "$top.sby1 set" -xscrollcommand "$top.sbx1 set"
    text $top.t2 -width 60 -height 35 \
	    -yscrollcommand "$top.sby2 set" -xscrollcommand "$top.sbx2 set"
    scrollbar $top.sbx1 -orient horiz -command "$top.t1 xview"
    scrollbar $top.sbx2 -orient horiz -command "$top.t2 xview"
    scrollbar $top.sby1 -orient vert  -command "$top.t1 yview"
    scrollbar $top.sby2 -orient vert  -command "$top.t2 yview"

    bind $top.t1 <Control-o> "focus $top.t2"
    bind $top.t2 <Control-o> "focus $top.t1"

    frame $top.f
    button $top.b -text "Diff" -command doClipDiff -underline 0
    bind $top <Alt-d> [list $top.b invoke]
    button $top.b2 -text "Left Clear" -command "$top.t1 delete 1.0 end"
    button $top.b3 -text "Right Clear" -command "$top.t2 delete 1.0 end"
    button $top.b4 -text "Left Clear&Paste" -command \
            "$top.t1 delete 1.0 end ; event generate $top.t1 <<Paste>>"
    button $top.b5 -text "Right Clear&Paste" -command \
            "$top.t2 delete 1.0 end ; event generate $top.t2 <<Paste>>"

    pack $top.b $top.b2 $top.b3 $top.b4 $top.b5 -in $top.f -side left

    if {$::debug == 1} {
        menubutton $top.md -text Debug -menu $top.md.m -relief ridge
        menu $top.md.m
        if {$::tcl_platform(platform) == "windows"} {
            $top.md.m add checkbutton -label Console -variable consolestate \
                    -onvalue show -offvalue hide -command {console $consolestate}
            $top.md.m add separator
        }
        $top.md.m add command -label "Stack trace" -command {bgerror Debug}
        $top.md.m add separator
        $top.md.m add command -label "Reread Source" -command {source $thisscript}
        $top.md.m add separator
        $top.md.m add command -label "Redraw Window" -command {makeWin}
        
        pack $top.md -in $top.f -side left
    }

    grid $top.f    -         -         -         -sticky w
    grid $top.t1   $top.sby1 $top.t2   $top.sby2 -sticky news
    grid $top.sbx1 x         $top.sbx2 x         -sticky we
    grid rowconfigure    $top 1     -weight 1
    grid columnconfigure $top {0 2} -weight 1
}

if {![info exists gurkmeja]} {
    set gurkmeja 1
    wm withdraw .
    makeClipDiffWin
    update idletasks
}
