#----------------------------------------------------------------------
#  Eskil, Clip diff
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

# Remove things pasted that can disturb
proc ClipClean {data} {
    set data [string map [list \r \n] $data]
    return [string trimright $data \n]
}

proc DoClipDiff {} {
    set f1 [tmpFile]
    set f2 [tmpFile]

    set ch [open $f1 w]
    set data [$::diff(wClip1) get 1.0 end]
    set data [ClipClean $data]
    puts $ch $data
    close $ch

    set ch [open $f2 w]
    set data [$::diff(wClip2) get 1.0 end]
    set data [ClipClean $data]
    puts $ch $data
    close $ch

    newDiff $f1 $f2
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

    frame $top.f
    menubutton $top.f.mf -menu $top.f.mf.m -text "File" -underline 0
    menu $top.f.mf.m
    $top.f.mf.m add command -label "Close" -underline 0 \
            -command [list cleanupAndExit $top]
    $top.f.mf.m add separator
    $top.f.mf.m add command -label "Quit" -underline 0 \
            -command [list cleanupAndExit all]

    button $top.f.b -text "Diff" -command DoClipDiff -underline 0 -width 8
    bind $top <Alt-d> [list $top.f.b invoke]
    button $top.f.b2 -text "Left Clear" -command "$t1 delete 1.0 end" \
            -underline 0
    bind $top <Alt-l> "[list $top.f.b2 invoke] ; [list focus $t1]"

    button $top.f.b3 -text "Right Clear" -command "$t2 delete 1.0 end" \
            -underline 0
    bind $top <Alt-r> "[list $top.f.b3 invoke] ; [list focus $t2]"
    button $top.f.b4 -text "Left Clear&Paste" -command \
            "$t1 delete 1.0 end ; event generate $t1 <<Paste>>"
    button $top.f.b5 -text "Right Clear&Paste" -command \
            "$t2 delete 1.0 end ; event generate $t2 <<Paste>>"

    foreach w [list $top.f.b2 $top.f.b4 $top.f.b $top.f.b3 $top.f.b5] {
        raise $w
    }
    grid $top.f.mf $top.f.b2 $top.f.b4 x $top.f.b x $top.f.b3 $top.f.b5 x \
            -padx 4 -pady 2 -sticky "w"
    grid $top.f.mf -sticky nw -pady 0 -padx 0
    grid columnconfigure $top.f {0 3 5 8} -weight 1
    grid columnconfigure $top.f 8 -minsize [winfo reqwidth $top.f.mf]

    grid $top.f    -       -sticky we
    grid $top.t1   $top.t2 -sticky news
    grid $top.t2 -padx {2 0}
    grid rowconfigure    $top 1     -weight 1
    grid columnconfigure $top {0 1} -weight 1
}
