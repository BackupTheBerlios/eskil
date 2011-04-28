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
    lappend dirs [file join $::eskil(thisDir) .. ..]
    lappend dirs [file join $::eskil(thisDir) .. .. plugins]
    lappend dirs [file join $::eskil(thisDir) .. plugins]
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

proc listPlugins {} {
    set dirs [PluginSearchPath]

    foreach dir $dirs {
        set files [glob -nocomplain [file join $dir *.tcl]]
        foreach file $files {
            set file [file normalize $file]
            if {[info exists done($file)]} continue
            if {![file exists $file]} continue
            if {![file isfile $file]} continue
            if {![file readable $file]} continue

            set done($file) 1
            set ch [open $file r]
            set data [read $ch 200]
            close $ch
            if {[regexp {^\#\#Eskil Plugin :(.*?)(\n|$)} $data -> descr]} {
                set result([file rootname [file tail $file]]) $descr
            }
        }
    }
    return [array get result]
}

proc printPlugins {} {
    set plugins [listPlugins]
    if {[llength $plugins] == 0} {
        puts "No plugins found."
        return
    }
    puts "Available plugins:"
    foreach {plugin descr} $plugins {
        puts "Plugin \"$plugin\" : $descr"
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

# GUI for plugin selection
proc EditPrefPlugins {top} {
    set w $top.prefplugin

    # Create window
    destroy $w
    toplevel $w -padx 3 -pady 3
    ttk::frame $w._bg
    place $w._bg -x 0 -y 0 -relwidth 1.0 -relheight 1.0 -border outside
    wm title $w "Preferences: Plugins"

    set plugins [listPlugins]
    if {[llength $plugins] == 0} {
        grid [ttk::label $w.l -text "No plugins found."] - -padx 3 -pady 3
    }
    if {![info exists ::diff($top,pluginname)]} {
        set ::diff($top,pluginname) ""
    }
    if {![info exists ::diff($top,plugininfo)]} {
        set ::diff($top,plugininfo) ""
    }
    set ::diff($top,edit,pluginname) $::diff($top,pluginname) 
    set ::diff($top,edit,plugininfo) $::diff($top,plugininfo)
    set t 0
    foreach {plugin descr} $plugins {
        ttk::radiobutton $w.rb$t -variable ::diff($top,edit,pluginname) -value $plugin -text $plugin
        ttk::label $w.l$t -text $descr -anchor "w"
        grid $w.rb$t $w.l$t -sticky we -padx 3 -pady 3
        incr t
    }
    ttk::radiobutton $w.rb$t -variable ::diff($top,edit,pluginname) -value "" -text "No Plugin"
    grid $w.rb$t -sticky we -padx 3 -pady 3

    ttk::label $w.li -text "Info" -anchor "w"
    ttk::entry $w.ei -textvariable ::diff($top,edit,plugininfo)
    grid $w.li $w.ei -sticky we -padx 3 -pady 3

    ttk::frame $w.fb -padding 3
    ttk::button $w.fb.b1 -text "Ok"     -command [list EditPrefPluginsOk $top $w]
    ttk::button $w.fb.b2 -text "Cancel" -command [list destroy $w]
    set ::widgets($top,prefPluginsOk) $w.fb.b1

    grid $w.fb.b1 x $w.fb.b2 -sticky we
    grid columnconfigure $w.fb {0 2} -uniform a
    grid columnconfigure $w.fb 1 -weight 1

    grid $w.fb - -sticky we
    grid columnconfigure $w 1 -weight 1
}

proc EditPrefPluginsOk {top w} {
    destroy $w
    set ::diff($top,pluginname) $::diff($top,edit,pluginname) 
    set ::diff($top,plugininfo) $::diff($top,edit,plugininfo)
    if {$::diff($top,pluginname) ne ""} {
        set pinterp [createPluginInterp $::diff($top,pluginname) $::diff($top,plugininfo)]
    } else {
        set pinterp ""
    }
    set ::diff($top,plugin) $pinterp
}
