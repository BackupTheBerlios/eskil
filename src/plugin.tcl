#----------------------------------------------------------------------
#  Eskil, Plugin handling
#
#  Copyright (c) 2008, Peter Spjuth  (peter.spjuth@gmail.com)
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

proc PluginSearchPath {} {
    set dirs [list . ./plugins]
    lappend dirs [file join $::thisDir .. ..]
    lappend dirs [file join $::thisDir .. .. plugins]
    lappend dirs [file join $::thisDir .. plugins]
    return $dirs
}

# Locate plugin source
proc LocatePlugin {plugin} {
    set src ""
    set dirs [PluginSearchPath]

    foreach dir $dirs {
        set files {}
        lappend files [file join $dir $plugin]
        lappend files [file join $dir $plugin.tcl]
        foreach file $files {
            if {![file exists   $file]} continue
            if {![file isfile   $file]} continue
            if {![file readable $file]} continue
            set ch [open $file r]
            set data [read $ch 20]
            close $ch
            if {[string match "##Eskil Plugin*" $data]} {
                set src $file
                break
            }
        }
        if {$src ne ""} break
    }
    return $src
}

proc createPluginInterp {plugin info} {
    set src [LocatePlugin $plugin]

    if {$src eq ""} {
        return ""
    }

    # Create interpreter
    set pi [interp create -safe]

    # Load source
    $pi invokehidden -global source $src
    $pi eval [list set ::WhoAmI [file rootname [file tail $src]]]
    $pi eval [list set ::Info $info]
    interp share {} stdout $pi

    # Expose needed commands
    interp expose $pi fconfigure ;# ??
    interp hide $pi close

    return $pi
}

proc printPlugin {plugin} {
    set src [LocatePlugin $plugin]
    if {$src eq ""} {
        printPlugins
        return
    }
    set ch [open $src]
    puts -nonewline [read $ch]
    close $ch
}

proc printPlugins {} {
    set dirs [PluginSearchPath]

    foreach dir $dirs {
        set files [glob -nocomplain [file join $dir *.tcl]]
        foreach file $files {
            if {![file exists $file]} continue
            if {![file isfile $file]} continue
            if {![file readable $file]} continue
            set ch [open $file r]
            set data [read $ch 100]
            close $ch
            if {[regexp {^\#\#Eskil Plugin :(.*?)(\n|$)} $data -> descr]} {
                puts "Plugin \"[file rootname [file tail $file]]\" : $descr"
            }
        }
    }
}

proc preparePlugin {top} {
    #FIXA: plugin miffo
    disallowEdit $top
    $::diff($top,plugin) eval [list array set ::Pref [array get ::Pref]]
    set out1 [tmpFile]
    set out2 [tmpFile]

    set chi [open $::diff($top,leftFile) r]
    set cho [open $out1 w]
    interp share {} $chi $::diff($top,plugin)
    interp share {} $cho $::diff($top,plugin)
    set usenew1 [$::diff($top,plugin) eval [list PreProcess left $chi $cho]]
    $::diff($top,plugin) invokehidden close $chi
    $::diff($top,plugin) invokehidden close $cho
    close $chi
    close $cho

    set chi [open $::diff($top,rightFile) r]
    set cho [open $out2 w]
    interp share {} $chi $::diff($top,plugin)
    interp share {} $cho $::diff($top,plugin)
    set usenew2 [$::diff($top,plugin) eval [list PreProcess right $chi $cho]]
    $::diff($top,plugin) invokehidden close $chi
    $::diff($top,plugin) invokehidden close $cho
    close $chi
    close $cho

    if {$usenew1} {
        # The file after processing should be used both
        # for comparison and for displaying.
        set ::diff($top,leftFileBak) $::diff($top,leftFile)
        set ::diff($top,leftFile) $out1
    } else {
        set ::diff($top,leftFileDiff) $out1
        #set ::diff($top,leftLabel) "$::diff($top,RevFile) $tag"
    }
    if {$usenew2} {
        set ::diff($top,rightFileBak) $::diff($top,rightFile)
        set ::diff($top,rightFile) $out2
    } else {
        set ::diff($top,rightFileDiff) $out2
        #set ::diff($top,rightLabel) $::diff($top,RevFile)
    }
}

proc cleanupPlugin {top} {
    if {[info exists ::diff($top,leftFileBak)]} {
        set ::diff($top,leftFile) $::diff($top,leftFileBak)
    }
    if {[info exists ::diff($top,rightFileBak)]} {
        set ::diff($top,rightFile) $::diff($top,rightFileBak)
    }
    unset -nocomplain \
            ::diff($top,leftFileBak) ::diff($top,rightFileBak) \
            ::diff($top,leftFileDiff) ::diff($top,rightFileDiff)
}
