#----------------------------------------------------------------------
#  Eskil, Clip diff
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

# Remove things pasted that can disturb
proc ClipClean {data} {
    set data [string map [list \r \n] $data]
    return [string trimright $data \n]
}

proc DoClipDiff {} {
    set f1 [tmpFile]
    set f2 [tmpFile]

    set ch [open $f1 w]
    set data1 [$::diff(wClip1) get 1.0 end]
    set data1 [ClipClean $data1]
    puts $ch $data1
    close $ch

    set ch [open $f2 w]
    set data2 [$::diff(wClip2) get 1.0 end]
    set data2 [ClipClean $data2]
    puts $ch $data2
    close $ch

    #set line1 [split $data1 \n]
    #set len1  [llength $line1]
    #set line2 [split $data2 \n]
    #set len2  [llength $line2]

    set top [newDiff $f1 $f2]

    # Try to shrink the diff win?
    set t1 $::widgets($top,wDiff1)
    set t2 $::widgets($top,wDiff2)
    set lines1 [lindex [split [$t1 index end] "."] 0]
    set lines2 [lindex [split [$t2 index end] "."] 0]
    puts "$lines1 $lines2"
    if {$lines1 < 30 && $lines2 < 30} {
        $t1 configure -height $lines1
        $::widgets($top,wLine1) configure -height 1
        $t2 configure -height $lines2
        $::widgets($top,wLine2) configure -height 1
    }
}

proc ArmCatch {} {
    if {$::diff(armcatch)} {
        bind .clipdiff <FocusOut> {
            if {[string equal %W .clipdiff]} {
                after 50 CatchFromWin
            }
        }
    } else {
        bind .clipdiff <FocusOut> {}
    }
}

proc CatchFromWin {} {
    set ::diff(armcatch) 0
    ArmCatch
    set win [twapi::get_foreground_window]
    if {$win eq ""} {
        #puts "No fg window"
        return
    }
    #puts "Locating windows"
    lassign [twapi::get_window_coordinates $win] x1 y1 x2 y2
    set width  [expr {$x2 - $x1}]
    set height [expr {$y2 - $y1}]

    set windows {}
    foreach x [list [expr {$x1 + $width / 4}] [expr {$x1 + 3*$width /4}]] {
        foreach y [list [expr {$y1 + $height / 4}] [expr {$y1 + 3*$height /4}]] {
            lappend windows [twapi::get_window_at_location $x $y]
        }
    }
    set windows [lsort -unique $windows]
    #puts $windows
    after 50 "set ::CatchFromWinWait 1" ; vwait ::CatchFromWinWait
    set capturedData {}
    foreach win $windows {
        clipboard clear
        clipboard append ""
        twapi::set_focus $win
        after 50 "set ::CatchFromWinWait 1" ; vwait ::CatchFromWinWait
        twapi::send_keys ^(ac)
        after 50 "set ::CatchFromWinWait 1" ; vwait ::CatchFromWinWait
        lassign [twapi::get_window_coordinates $win] x1 y1 x2 y2
        if {[catch {clipboard get} text]} continue
        if {$text eq ""} continue
        lappend capturedData [list $x1 $text]
    }
    $::diff(wClip1) delete 1.0 end
    $::diff(wClip2) delete 1.0 end
    if {[llength $capturedData] == 0} return
    # Set it up left-to-right
    set capturedData [lsort -index 0 -integer $capturedData]
    if {[llength $capturedData] >= 1} {
        set text [lindex $capturedData 0 1]
        $::diff(wClip1) insert end $text
    }
    if {[llength $capturedData] >= 2} {
        set text [lindex $capturedData 1 1]
        $::diff(wClip2) insert end $text
        after idle DoClipDiff
    }
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
    lappend ::diff(diffWindows) $top

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

    ttk::frame $top.f
    menubutton $top.f.mf -menu $top.f.mf.m -text "File" -underline 0
    menu $top.f.mf.m
    $top.f.mf.m add command -label "Close" -underline 0 \
            -command [list cleanupAndExit $top]
    $top.f.mf.m add separator
    $top.f.mf.m add command -label "Quit" -underline 0 \
            -command [list cleanupAndExit all]

    ttk::button $top.f.b -text "Diff" -command DoClipDiff -underline 0 -width 8
    bind $top <Alt-d> [list $top.f.b invoke]
    ttk::button $top.f.b2 -text "Left Clear" -command "$t1 delete 1.0 end" \
            -underline 0
    bind $top <Alt-l> "[list $top.f.b2 invoke] ; [list focus $t1]"

    ttk::button $top.f.b3 -text "Right Clear" -command "$t2 delete 1.0 end" \
            -underline 0
    bind $top <Alt-r> "[list $top.f.b3 invoke] ; [list focus $t2]"
    ttk::button $top.f.b4 -text "Left Clear&Paste" -command \
            "$t1 delete 1.0 end ; event generate $t1 <<Paste>>"
    ttk::button $top.f.b5 -text "Right Clear&Paste" -command \
            "$t2 delete 1.0 end ; event generate $t2 <<Paste>>"
    #foreach w [list $top.f.b2 $top.f.b4 $top.f.b $top.f.b3 $top.f.b5] {
    #    raise $w
    #}
    grid $top.f.mf $top.f.b2 $top.f.b4 x $top.f.b x $top.f.b3 $top.f.b5 x \
            -padx 4 -pady 2 -sticky "w"
    grid $top.f.mf -sticky nw -pady 0 -padx 0
    grid columnconfigure $top.f {0 3 5 8} -weight 1
    grid columnconfigure $top.f 8 -minsize [winfo reqwidth $top.f.mf]

    if {![catch {package require twapi}]} {
        ttk::checkbutton $top.f.b6 -text "Capture" -command ArmCatch \
                -underline 0 -variable ::diff(armcatch)
        bind $top <Alt-c> [list $top.f.b6 invoke]
        #raise $top.f.b6
        place $top.f.b6 -anchor e -relx 1.0 -rely 0.5
    }

    grid $top.f    -       -sticky we
    grid $top.t1   $top.t2 -sticky news
    grid $top.t2 -padx {2 0}
    grid rowconfigure    $top 1     -weight 1
    grid columnconfigure $top {0 1} -weight 1
}
