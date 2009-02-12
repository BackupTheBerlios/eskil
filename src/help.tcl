#----------------------------------------------------------------------
#  Eskil, Help functions
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

# Silly experiment...
proc makeNuisance {top {str {Hi there!}}} {
    if {[lsearch [image names] nuisance] < 0} {
        set file [file join $::thisDir .. Nuisance.gif]
        if {![file exists $file]} return
        image create photo nuisance -file $file
    }

    destroy $top.nui
    toplevel $top.nui
    wm transient $top.nui $top
    wm geometry $top.nui +400+400
    wm title $top.nui ""
    ttk::label $top.nui.l -image nuisance
    pack $top.nui.l
    wm protocol $top.nui WM_DELETE_WINDOW [list destroy $top.nui2 $top.nui]
    update

    destroy $top.nui2
    toplevel $top.nui2 -background yellow
    wm transient $top.nui2 $top.nui
    wm overrideredirect $top.nui2 1
    wm title $top.nui2 ""
    ttk::label $top.nui2.l -text "$str\nDo you want help?" -justify left \
            -background yellow
    button $top.nui2.b -text "No, get out of my face!" \
            -command [list destroy $top.nui2 $top.nui] -background yellow
    pack $top.nui2.l $top.nui2.b -side "top" -fill x
    wm geometry $top.nui2 +[expr {405 + [winfo width $top.nui]}]+400
}

# A simple window for displaying e.g. help.
# Returns the frame where things can be put.
proc helpWin {w title} {
    destroy $w

    toplevel $w -padx 2 -pady 2
    wm title $w $title
    bind $w <Key-Return> [list destroy $w]
    bind $w <Key-Escape> [list destroy $w]
    ttk::frame $w.f
    ttk::button $w.b -text "Close" -command [list destroy $w] -width 10 \
            -default active
    pack $w.b -side bottom -pady 2
    pack $w.f -side top -expand y -fill both -padx 2 -pady 2
    focus $w
    return $w.f
}

proc makeAboutWin {} {
    global diffver

    set w [helpWin .ab "About Eskil"]

    set bg [ttk::style configure . -background]
    text $w.t -width 45 -height 11 -wrap none -relief flat \
            -background $bg
    pack $w.t -side top -expand y -fill both

    $w.t insert end "A graphical frontend to diff\n\n"
    $w.t insert end "$diffver\n\n"
    $w.t insert end "Made by Peter Spjuth\n"
    $w.t insert end "E-Mail: peter.spjuth@gmail.com\n"
    $w.t insert end "\nURL: http://eskil.berlios.de\n"
    $w.t insert end "\nTcl version: [info patchlevel]\n"

    set du [package provide DiffUtil]
    if {[info procs DiffUtil::LocateDiffExe] ne ""} {
        append du " (tcl)"
    } else {
        append du " (c)"
    }
    $w.t insert end "DiffUtil version: $du\n"
    $w.t insert end "\nCredits:\n"
    $w.t insert end "Ideas for scrollbar map and merge function\n"
    $w.t insert end "taken from TkDiff"

    set last [lindex [split [$w.t index end] "."] 0]
    $w.t configure -height $last
    $w.t configure -state disabled
}

# Insert a text file into a text widget.
# Any XML-style tags in the file are used as tags in the text window.
proc insertTaggedText {w file} {
    set ch [open $file r]
    set data [read $ch]
    close $ch

    set tags {}
    while {$data != ""} {
        if {[regexp {^([^<]*)<(/?)([^>]+)>(.*)$} $data -> pre sl tag post]} {
            $w insert end [subst -nocommands -novariables $pre] $tags
            set i [lsearch $tags $tag]
            if {$sl != ""} {
                # Remove tag
                if {$i >= 0} {
                    set tags [lreplace $tags $i $i]
                }
            } else {
                # Add tag
                lappend tags $tag
            }
            set data $post
        } else {
            $w insert end [subst -nocommands -novariables $data] $tags
            set data ""
        }
    }
}

proc makeHelpWin {} {
    global Pref

    set doc [file join $::thisDir .. doc/eskil.txt]
    if {![file exists $doc]} return

    set w [helpWin .he "Eskil Help"]
    set t [Scroll y text $w.t -width 85 -height 35]
    pack $w.t -side top -expand 1 -fill both

    configureDocWin $t

    # Set up tags for change marks
    $t tag configure new1 -foreground $Pref(colornew1) \
            -background $Pref(bgnew1)
    $t tag configure new2 -foreground $Pref(colornew2) \
            -background $Pref(bgnew2)
    $t tag configure change -foreground $Pref(colorchange) \
            -background $Pref(bgchange)
    $t tag configure ul -underline 1

    set width [font measure [$t cget -font] [string repeat x 20]]
    $t configure -tabs [list $width [expr {$width * 3/2}] [expr {$width * 2}]]

    set width [font measure docFontP [string repeat x 36]]
    $t tag configure example -tabs [list $width] -wrap none

    insertTaggedText $t $doc
    $t configure -state disabled
}

proc createDocFonts {} {
    if {[catch {font create docFont -family Helvetica -size -16}]} return
    font create docFontB {*}[font configure docFont] -weight bold

    set h [font metrics docFont -linespace]
    # Use negative size to get pixels. Search from small to bigger
    set t [expr {-$h + 4}]
    font create docFontP -family Courier -size $t
    for {} {$t > -20} {incr t -1} {
        font configure docFontP -size $t
        if {[font metrics docFontP -linespace] >= $h} break
    }
}

# Configure a text window as Doc viewer
proc configureDocWin {w} {
    createDocFonts
    $w configure -font docFont -wrap word
    $w tag configure ul -underline 1
    $w tag configure b -font docFontB
    $w tag configure bullet -tabs "1c" -lmargin2 "1c"
    $w tag configure pre -font docFontP

    set top [winfo toplevel $w]
    foreach event {<Key-Prior> <Key-Next>} {
        bind $top $event [string map [list "%W" $w] [bind Text $event]]
    }
}

proc makeDocWin {fileName} {
    set w [helpWin .doc "Eskil Help"]
    set t [Scroll y text $w.t -width 80 -height 25]
    pack $w.t -side top -expand 1 -fill both

    configureDocWin $t

    if {![file exists $::thisDir/../doc/$fileName]} {
        $t insert end "ERROR: Could not find doc file "
        $t insert end \"$fileName\"
        return
    }
    insertTaggedText $t $::thisDir/../doc/$fileName

    #focus $t
    $t configure -state disabled
}

proc makeTutorialWin {} {
    global Pref

    set doc [file join $::thisDir .. doc/tutorial.txt]
    if {![file exists $doc]} return

    if {[catch {cd [file join $::thisDir .. examples]}]} {
        tk_messageBox -icon error -title "Eskil Error" -message \
                "Could not locate examples directory." \
                -type ok
        return
    }
    #set ::diff(tutorial) 1

    # Start up a dirdiff in the examples directory
    set ::dirdiff(leftDir) [file join [pwd] dir1]
    set ::dirdiff(rightDir) [file join [pwd] dir2]
    makeDirDiffWin

    set w [helpWin .ht "Eskil Tutorial"]

    text $w.t -width 82 -height 35 -yscrollcommand "$w.sb set"
    scrollbar $w.sb -orient vert -command "$w.t yview"
    pack $w.sb -side right -fill y
    pack $w.t -side left -expand 1 -fill both

    configureDocWin $w.t

    # Move border properties to frame
    set bw [$w.t cget -borderwidth]
    set relief [$w.t cget -relief]
    $w configure -relief $relief -borderwidth $bw
    $w.t configure -borderwidth 0

    insertTaggedText $w.t $doc
    $w.t configure -state disabled
}
