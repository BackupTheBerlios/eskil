#----------------------------------------------------------------------
#  Eskil, Registry
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

proc MakeRegistryFrame {w label key newvalue} {
    set old {}
    catch {set old [registry get $key {}]}

    set l [labelframe $w -text $label -padx 4 -pady 4]

    label $l.key1 -text "Key:"
    label $l.key2 -text $key
    label $l.old1 -text "Old value:"
    label $l.old2 -text $old
    label $l.new1 -text "New value:"
    label $l.new2 -text $newvalue

    button $l.change -text "Change" -width 10 -command \
            "[list registry set $key {} $newvalue] ; \
             [list $l.change configure -state disabled]"
    button $l.delete -text "Delete" -width 10 -command \
            "[list registry delete $key] ; \
             [list $l.delete configure -state disabled]"
    if {[string equal $newvalue $old]} {
        $l.change configure -state disabled
    }
    if {[string equal "" $old]} {
        $l.delete configure -state disabled
    }
    grid $l.key1 $l.key2 -     -sticky "w" -padx 4 -pady 4
    grid $l.old1 $l.old2 -     -sticky "w" -padx 4 -pady 4
    grid $l.new1 $l.new2 -     -sticky "w" -padx 4 -pady 4
    grid $l.delete - $l.change -sticky "w" -padx 4 -pady 4
    grid $l.change -sticky "e"
    grid columnconfigure $l 2 -weight 1
}

proc makeRegistryWin {} {
    global thisScript

    # Locate executable for this program
    set exe [info nameofexecutable]
    if {[regexp {^(.*wish)\d+\.exe$} $exe -> pre]} {
        set alt $pre.exe
        if {[file exists $alt]} {
            set a [tk_messageBox -icon question -title "Which Wish" -message \
                    "Would you prefer to use the executable\n\
                    \"$alt\"\ninstead of\n\
                    \"$exe\"\nin the registry settings?" -type yesno]
            if {$a eq "yes"} {
                set exe $alt
            }
        }
    }

    set top .reg
    destroy $top
    toplevel $top
    wm title $top "Register Eskil"

    # Registry keys

    #set keyg {HKEY_CLASSES_ROOT\Folder\shell\Grep\command}
    set keydd {HKEY_CLASSES_ROOT\Folder\shell\Diff\command}
    set keyd {HKEY_CLASSES_ROOT\*\shell\Diff\command}
    set keyc {HKEY_CLASSES_ROOT\*\shell\DiffC\command}
    set keye {HKEY_CLASSES_ROOT\*\shell\Emacs\command}

    # Are we in a starkit?
    if {[info exists ::starkit::topdir]} {
        # In a starpack ?
        set exe [file normalize $exe]
        if {[string equal [file normalize $::starkit::topdir] $exe]} {
            set myexe [list $exe]
        } else {
            set myexe [list $exe $::starkit::topdir]
        }
    } else {
        if {[regexp {wish\d+\.exe} $exe]} {
            set exe [file join [file dirname $exe] wish.exe]
            if {[file exists $exe]} {
                set myexe [list $exe]
            }
        }
        set myexe [list $exe $thisScript]
    }

    set valbase {}
    foreach item $myexe {
        lappend valbase \"[file nativename $item]\"
    }
    set valbase [join $valbase]

    set new "$valbase -browse \"%1\""
    MakeRegistryFrame $top.d "Diff" $keyd $new

    set new "$valbase -o \"%1\" -conflict \"%1\""
    MakeRegistryFrame $top.c "Diff Conflict" $keyc $new

    set new "$valbase \"%1\""
    MakeRegistryFrame $top.dd "Directory Diff" $keydd $new
    pack $top.d $top.c $top.dd -side "top" -fill x -padx 4 -pady 4

    locateEditor ::util(editor)
    if {[string match "*runemacs.exe" $::util(editor)]} {
        # Set up emacs
        set newkey "\"[file nativename $::util(editor)]\" \"%1\""
        MakeRegistryFrame $top.e "Emacs" $keye $newkey
        pack $top.e -side "top" -fill x -padx 4 -pady 4
    }

    button $top.close -text "Close" -width 10 -command [list destroy $top] \
            -default active
    pack $top.close -side bottom -pady 4
    bind $top <Key-Return> [list destroy $top]
    bind $top <Key-Escape> [list destroy $top]
}
