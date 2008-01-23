#----------------------------------------------------------------------
#  Revision control systems support for Eskil.
#
#  Copyright (c) 1998-2007, Peter Spjuth
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

##############################################################################
# Revision Control System specific procedures
##############################################################################

# eskil::rev::XXX::detect {file}
#
# Detect if a file is revision controlled under this system.
#
# Returns true if controlled or false if not.

# eskil::rev::XXX::ParseRevs {filename revs}
#
# Figure out revision from a list given by user
# 
# Returns a list of revisions to display.

# eskil::rev::XXX::get {filename outfile rev}
#
# Get a revision of a file and place it in outfile.
# rev is in any format understood by this system, and
# should be retrieved from ParseRevs


namespace eval eskil::rev::CVS {}
namespace eval eskil::rev::RCS {}
namespace eval eskil::rev::CT {}
namespace eval eskil::rev::GIT {}
namespace eval eskil::rev::SVN {}

proc eskil::rev::CVS::detect {file} {
    set dir [file dirname $file]
    if {[file isdirectory [file join $dir CVS]]} {
        if {[auto_execok cvs] ne ""} {
            return 1
        }
    }
    return 0
}

proc eskil::rev::SVN::detect {file} {
    set dir [file dirname $file]
    if {[file isdirectory [file join $dir .svn]]} {
        if {[auto_execok svn] ne ""} {
            return 1
        }
    }
    return 0
}

proc eskil::rev::RCS::detect {file} {
    set dir [file dirname $file]
    if {[file isdirectory [file join $dir RCS]] || [file exists $file,v]} {
        if {[auto_execok rcs] ne ""} {
            return 1
        }
    }
    return 0
}

proc eskil::rev::CT::detect {file} {
    set dir [file dirname $file]
    if {[auto_execok cleartool] != ""} {
        set old [pwd]
        cd $dir
        if {![catch {exec cleartool pwv -s} view] && $view != "** NONE **"} {
            cd $old
            return 1
        }
        cd $old
    }
    return 0
}

proc eskil::rev::GIT::detect {file} {
    set dir [file dirname $file]
    # Git, detect two steps down. Could be improved. FIXA
    if {[file isdirectory [file join $dir .git]] ||
        [file isdirectory [file join $dir .. .git]] ||
        [file isdirectory [file join $dir .. .. .git]]} {
        if {[auto_execok git] ne ""} {
            return 1
        }
    }
    return 0
}

# Get a CVS revision
proc eskil::rev::CVS::get {filename outfile rev} {
    set old ""
    set dir [file dirname $filename]
    if {$dir != "."} {
        set old [pwd]
        set outfile [file join [pwd] $outfile]
        cd $dir
        set filename [file tail $filename]
    }

    set cmd [list exec cvs -z3 update -p]
    if {$rev != ""} {
        lappend cmd -r $rev
    }
    lappend cmd [file nativename $filename] > $outfile
    if {[catch {eval $cmd} res]} {
        if {![string match "*Checking out*" $res]} {
            tk_messageBox -icon error -title "CVS error" -message $res
        }
    }

    if {$old != ""} {
        cd $old
    }
}

# Get a SVN revision
proc eskil::rev::SVN::get {filename outfile rev} {
    set old ""
    set dir [file dirname $filename]
    if {$dir != "."} {
        set old [pwd]
        set outfile [file join [pwd] $outfile]
        cd $dir
        set filename [file tail $filename]
    }

    set cmd [list exec svn cat]
    if {$rev != ""} {
        lappend cmd -r $rev
    }
    lappend cmd [file nativename $filename] > $outfile
    if {[catch {eval $cmd} res]} {
        if {![string match "*Checking out*" $res]} {
            tk_messageBox -icon error -title "SVN error" -message $res
        }
    }

    if {$old != ""} {
        cd $old
    }
}

# Get an RCS revision
proc eskil::rev::RCS::get {filename outfile {rev {}}} {
    catch {exec co -p$rev [file nativename $filename] \
            > $outfile}
}

# Get a GIT revision
# No support for revisions yet
proc eskil::rev::GIT::get {filename outfile rev} {
    set old [pwd]
    set dir [file dirname $filename]
    set tail [file tail $filename]
    # Locate the top directory
    while {![file isdirectory $dir/.git]} {
        set thisdir [file tail $dir]
        set dir [file dirname $dir]
        set tail [file join $thisdir $tail]
    }
    cd $dir
    catch {exec git show $rev:$tail > $outfile}
    cd $old
    # example: git show HEAD^^^:apa
}

# Get a ClearCase revision
proc eskil::rev::CT::get {filename outfile rev} {
    set filerev [file nativename $filename@@$rev]
    if {[catch {exec cleartool get -to $outfile $filerev} msg]} {
        tk_messageBox -icon error -title "Cleartool error" -message $msg
        return
    }
}

# Return current revision of a CVS file
proc eskil::rev::CVS::GetCurrent {filename} {
    set old ""
    set dir [file dirname $filename]
    if {$dir != "."} {
        set old [pwd]
        cd $dir
        set filename [file tail $filename]
    }

    set cmd [list exec cvs -n status [file nativename $filename]]
    if {[catch {eval $cmd} res]} {
        # What to do here?
        set rev "1.1"
    } else {
        if {![regexp {Working revision:\s+(\d\S*)} $res -> rev]} {
            set rev "1.1"
        }
    }

    if {$old != ""} {
        cd $old
    }
    return $rev
}

# Return current revision of a SVN file
proc eskil::rev::SVN::GetCurrent {filename} {
    set old ""
    set dir [file dirname $filename]
    if {$dir != "."} {
        set old [pwd]
        cd $dir
        set filename [file tail $filename]
    }

    set cmd [list exec svn info [file nativename $filename]]
    if {[catch {eval $cmd} res]} {
        # What to do here?
        set rev "1"
    } else {
        if {![regexp {Last Changed Rev:\s+(\d+)} $res -> rev]} {
            set rev "1"
        }
    }

    if {$old != ""} {
        cd $old
    }
    return $rev
}

# Figure out RCS revision from arguments
proc eskil::rev::RCS::ParseRevs {filename revs} {
    return $revs
}

# Figure out GIT revision from arguments
proc eskil::rev::GIT::ParseRevs {filename revs} {
    set result ""
    foreach rev $revs {
        switch -glob -- $rev {
            HEAD - master {
                lappend result $rev
            }
        }
    }
    if {[llength $result] == 0} {
        set result [list HEAD]
    }
    return $result
}

# Figure out CVS revision from arguments
proc eskil::rev::CVS::ParseRevs {filename revs} {
    set result {}
    foreach rev $revs {
        # An integer rev is a relative rev
        if {[string is integer -strict $rev]} {
            set curr [eskil::rev::CVS::GetCurrent $filename]
            regexp {^(.*\.)(\d+)$} $curr -> head tail
            set tail [expr {$tail + $rev}]
            if {$tail < 1} {set tail 1}
            set rev $head$tail
        }
        lappend result $rev
    }
    return $result
}

# Figure out SVN revision from arguments
proc eskil::rev::SVN::ParseRevs {filename revs} {
    set result {}
    foreach rev $revs {
        # A negative integer rev is a relative rev
        if {[string is integer -strict $rev] && $rev < 0} {
            set curr [eskil::rev::SVN::GetCurrent $filename]
            set rev [expr {$curr + $rev}]
        }
        lappend result $rev
    }
    return $result
}

# Figure out ClearCase revision from arguments
proc eskil::rev::CT::ParseRevs {filename revs} {
    set tmp [eskil::rev::CT::current $filename]
    foreach {stream latest} $tmp break
    if {[llength $revs] == 0} {
        return [list [file join $stream $latest]]
    }

    set result {}
    foreach rev $revs {
        # A negative version number is offset from latest.
        set offset 0
        set tail [file tail $rev]
        if {[string is integer -strict $tail] && $tail < 0} {
            set offset $tail
            set rev [file dirname $rev]
        }
        # If the argument is of the form "name/rev", look for a fitting one
        if {![string is integer $rev] && [regexp {^[^/.]+(/\d+)?$} $rev]} {
            if {[catch {exec cleartool lshistory -short $filename} allrevs]} {#
                tk_messageBox -icon error -title "Cleartool error" \
                        -message $allrevs
                return
            }
            set allrevs [split $allrevs \n]

            set i [lsearch -glob $allrevs "*$rev" ]
            if {$i >= 0} {
                set rev [lindex [split [lindex $allrevs $i] "@"] end]
            }
        }
        set rev [file normalize [file join $stream $rev]]
        # If we don't have a version number, try to find the latest
        if {![string is integer [file tail $rev]]} {
            if {![info exists allrevs]} {
                if {[catch {exec cleartool lshistory -short $filename} allrevs]} {#
                    tk_messageBox -icon error -title "Cleartool error" \
                            -message $allrevs
                    return
                }
                set allrevs [split $allrevs \n]
            }
            set apa [lsearch -regexp -all -inline $allrevs "$rev/\\d+\$"]
            set apa [lindex [lsort -dictionary $apa] end]
            if {$apa ne ""} {
                set rev [lindex [split $apa "@"] end]
            }
        }
        set tail [file tail $rev]
        if {[string is integer -strict $tail] && $offset < 0} {
            set path [file dirname $rev]
            set tail [expr {$tail + $offset}]
            if {$tail < 0} {set tail 0}
            set rev [file join $path $tail]
        }
        lappend result $rev
    }
    return $result
}

# Check in CVS controlled file
proc eskil::rev::CVS::commitFile {top filename} {
    set logmsg [LogDialog $top $filename]
    if {$logmsg ne ""} {
        catch {exec cvs -q commit -m $logmsg $filename}
    }
}

proc eskil::rev::CT::current {filename} {
    # Figure out stream and current version
    if {[catch {exec cleartool ls $filename} info]} {
        tk_messageBox -icon error -title "Cleartool error" -message $info
        return
    }
    set currV {}
    if {![regexp {@@(\S+)\s+from (\S+)\s+Rule} $info -> dummy currV]} {
        regexp {@@(\S+)} $info -> currV
    }
    set stream [file dirname $currV]
    set latest [file tail $currV]
    return [list $stream $latest]
}

##############################################################################
# Exported procedures
##############################################################################

# Figure out what revision control system a file is under
# Returns name of rev system if detected, or "" if none.
proc detectRevSystem {file {preference GIT}} {
    variable eskil::rev::cache

    if {![file exists $file]} { return "" }

    if {[info exists cache($file)]} {
        return $cache($file)
    }
    
    set searchlist [list $preference]
    foreach ns [namespace children eskil::rev] {
        lappend searchlist [namespace tail $ns]
    }
    foreach rev $searchlist {
        set result [eskil::rev::${rev}::detect $file]
        if {$result} {
            set cache($file) $rev
            return $rev
        }
    }
    return
}

# Initialise revision control mode
# The file name should be an absolute normalized path.
proc startRevMode {top rev file} {
    set ::diff($top,mode) "rev"
    set ::diff($top,modetype) $rev
    set ::diff($top,rightDir) [file dirname $file]
    set ::diff($top,RevFile) $file
    set ::diff($top,rightLabel) $file
    set ::diff($top,rightFile) $file
    set ::diff($top,rightOK) 1
    set ::diff($top,leftLabel) $rev
    set ::diff($top,leftOK) 0
    set ::Pref(toolbar) 1
}

# Prepare for RCS/CVS/CT diff. Checkout copies of the versions needed.
proc prepareRev {top} {
    global Pref

    $::widgets($top,commit) configure -state disabled

    set type $::diff($top,modetype)

    set revs {}

    # Search for revision options
    if {$::diff($top,doptrev1) != ""} {
        lappend revs $::diff($top,doptrev1)
    }
    if {$::diff($top,doptrev2) != ""} {
        lappend revs $::diff($top,doptrev2)
    }

    set revs [eskil::rev::${type}::ParseRevs $::diff($top,RevFile) $revs]
    set revlabels {}
    foreach rev $revs {
        lappend revlabels [GetLastTwoPath $rev]
    }

    if {[llength $revs] < 2} {
        # Compare local file with specified version.
        disallowEdit $top 1
        if {[llength $revs] == 0} {
            set r ""
            set tag "($type)"
        } else {
            set r [lindex $revs 0]
            set tag "($type [lindex $revlabels 0])"
        }
        set ::diff($top,leftFile) [tmpFile]
        set ::diff($top,leftLabel) "$::diff($top,RevFile) $tag"
        set ::diff($top,rightLabel) $::diff($top,RevFile)
        set ::diff($top,rightFile) $::diff($top,RevFile)

        eskil::rev::${type}::get $::diff($top,RevFile) $::diff($top,leftFile) $r
        if {$type eq "CVS" && [llength $revs] == 0} {
            $::widgets($top,commit) configure -state normal
        }
    } else {
        # Compare the two specified versions.
        disallowEdit $top
        set r1 [lindex $revs 0]
        set r2 [lindex $revs 1]
        set ::diff($top,leftFile)  [tmpFile]
        set ::diff($top,rightFile) [tmpFile]

        set ::diff($top,leftLabel) \
                "$::diff($top,RevFile) ($type [lindex $revlabels 0])"
        set ::diff($top,rightLabel) \
                "$::diff($top,RevFile) ($type [lindex $revlabels 1])"
        eskil::rev::${type}::get $::diff($top,RevFile) $::diff($top,leftFile) $r1
        eskil::rev::${type}::get $::diff($top,RevFile) $::diff($top,rightFile) $r2
    }
    # Make sure labels are updated before processing starts
    update idletasks
}

# Clean up after a RCS/CVS/CT diff.
proc cleanupRev {top} {
    global Pref

    clearTmp $::diff($top,rightFile) $::diff($top,leftFile)
    set ::diff($top,rightFile) $::diff($top,RevFile)
    set ::diff($top,leftFile) $::diff($top,RevFile)
}

proc revCommit {top} {
    if {[$::widgets($top,commit) cget -state] eq "disabled"} return
    eskil::rev::CVS::commitFile $top $::diff($top,RevFile)
}

##############################################################################
# Utilities
##############################################################################

# Get the last two elements in a file path
proc GetLastTwoPath {path} {
    set last [file tail $path]
    set penultimate [file tail [file dirname $path]]
    if {$penultimate eq "."} {
        return $last
    } else {
        return [file join $penultimate $last]
    }
}

# Dialog for log message
proc LogDialog {top filename {clean 0}} {
    set w $top.logmsg
    destroy  $w
    toplevel $w -padx 3 -pady 3
    wm title $w "Commit log message for [file tail $filename]"

    set ::diff($top,logdialogok) 0

    text $w.t -width 70 -height 10
    if {!$clean && [info exists ::diff(logdialog)]} {
        $w.t insert end $::diff(logdialog)
        $w.t tag add sel 1.0 end-1c
        $w.t mark set insert 1.0
    }

    button $w.ok -width 10 -text "Commit" -underline 1 \
            -command "set ::diff($top,logdialogok) 1 ; \
                      set ::diff(logdialog) \[$w.t get 1.0 end\] ; \
                      destroy $w"
    button $w.ca -width 10 -text "Cancel" -command "destroy $w" -underline 0
    bind $w <Alt-o> [list $w.ok invoke]\;break
    bind $w <Alt-c> [list destroy $w]\;break
    bind $w <Key-Escape> [list destroy $w]\;break

    grid $w.t  - -sticky news -padx 3 -pady 3
    grid $w.ok $w.ca -padx 3 -pady 3
    tkwait visibility $w
    focus -force $w.t
    tkwait window $w

    if {$::diff($top,logdialogok)} {
        set res [string trim $::diff(logdialog)]
        set ::diff(logdialog) $res
        if {$res eq ""} {
            set res "No Log"
        }
    } else {
        set res ""
    }
    return $res
}
