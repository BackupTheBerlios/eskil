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

namespace eval eskil::rev::CVS {}
namespace eval eskil::rev::RCS {}
namespace eval eskil::rev::CT {}
namespace eval eskil::rev::GIT {}

proc eskil::rev::CVS::detect {file} {
    set dir [file dirname $file]
    if {[file isdirectory [file join $dir CVS]]} {
        if {[auto_execok cvs] ne ""} {
            return "CVS"
        }
    }
    return
}

proc eskil::rev::RCS::detect {file} {
    set dir [file dirname $file]
    if {[file isdirectory [file join $dir RCS]] || [file exists $file,v]} {
        if {[auto_execok rcs] ne ""} {
            return "RCS"
        }
    }
    return
}

proc eskil::rev::CT::detect {file} {
    set dir [file dirname $file]
    if {[auto_execok cleartool] != ""} {
        set old [pwd]
        cd $dir
        if {![catch {exec cleartool pwv -s} view] && $view != "** NONE **"} {
            cd $old
            return "CT"
        }
        cd $old
    }
    return
}

proc eskil::rev::GIT::detect {file} {
    set dir [file dirname $file]
    # Git, detect two steps down. Could be improved. FIXA
    if {[file isdirectory [file join $dir .git]] ||
        [file isdirectory [file join $dir .. .git]] ||
        [file isdirectory [file join $dir .. .. .git]]} {
        if {[auto_execok git] ne ""} {
            return "GIT"
        }
    }
    return
}

# Figure out what revision control system a file is under
# Returns "CVS", "RCS", "CT", "GIT" if detected, or "" if none.
proc detectRevSystem {file} {
    # The search order is manually set to ensure GIT priority over CVS.
    foreach rev {GIT CVS RCS CT} {
        set result [eskil::rev::${rev}::detect $file]
        if {$result ne ""} {return $result}
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

# Get a CVS revision
proc GetCvsRev {filename outfile {rev {}}} {
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

# Get an RCS revision
proc GetRcsRev {filename outfile {rev {}}} {
    catch {exec co -p$rev [file nativename $filename] \
            > $outfile}
}

# Get a GIT revision
# No support for revisions yet
proc GetGitRev {filename outfile {rev {}}} {
    # Dummy copy for now FIXA
    file copy $filename $outfile
}

# Return current revision of a CVS file
proc GetCurrentRev {filename} {
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

# Figure out CVS revision from arguments
proc ParseCvsRevs {filename rev} {
    # An integer rev is a relative rev
    if {[string is integer -strict $rev]} {
        set curr [GetCurrentRev $filename]
        regexp {^(.*\.)(\d+)$} $curr -> head tail
        set tail [expr {$tail + $rev}]
        if {$tail < 1} {set tail 1}
        set rev $head$tail
    }
    
    return $rev
}

# Get a ClearCase revision
proc GetCtRev {filename outfile rev} {
    set filerev [file nativename $filename@@$rev]
    if {[catch {exec cleartool get -to $outfile $filerev} msg]} {
        tk_messageBox -icon error -title "Cleartool error" -message $msg
        return
    }
}

# Figure out ClearCase revision from arguments
proc ParseCtRevs {filename stream rev} {
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
        
    return $rev
}

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

# Prepare for RCS/CVS/CT diff. Checkout copies of the versions needed.
proc prepareRev {top} {
    global Pref

    $::widgets($top,commit) configure -state disabled

    set type $::diff($top,modetype)

    if {$type eq "CT"} {
        # Figure out stream and current version
        if {[catch {exec cleartool ls $::diff($top,RevFile)} info]} {
            tk_messageBox -icon error -title "Cleartool error" -message $info
            return
        }
        set currV {}
        if {![regexp {@@(\S+)\s+from (\S+)\s+Rule} $info -> dummy currV]} {
            regexp {@@(\S+)} $info -> currV
        }
        set stream [file dirname $currV]
        set latest [file tail $currV]
    }

    set revs {}

    # Search for revision options
    if {$::diff($top,doptrev1) != ""} {
        lappend revs $::diff($top,doptrev1)
    }
    if {$::diff($top,doptrev2) != ""} {
        lappend revs $::diff($top,doptrev2)
    }

    if {$type eq "CT"} {
        set revs2 {}
        set revlabels {}
        foreach rev $revs {
            set rev [ParseCtRevs $::diff($top,RevFile) $stream $rev]
            lappend revs2 $rev
            lappend revlabels [GetLastTwoPath $rev]
        }
        set revs $revs2
    } elseif {$type eq "CVS"} {
        set revs2 {}
        set revlabels {}
        foreach rev $revs {
            set rev [ParseCvsRevs $::diff($top,RevFile) $rev]
            lappend revs2 $rev
        }
        set revs $revs2
        set revlabels $revs
    } else {
        set revlabels $revs
    }

    if {[llength $revs] < 2} {
        # Compare local file with specified version.
        disallowEdit $top 1
        if {[llength $revs] == 0} {
            set r ""
            if {$type eq "CT"} {
                set r [file join $stream $latest]
            }
            set tag "($type)"
        } else {
            set r [lindex $revs 0]
            set tag "($type [lindex $revlabels 0])"
        }
        set ::diff($top,leftFile) [tmpFile]
        set ::diff($top,leftLabel) "$::diff($top,RevFile) $tag"
        set ::diff($top,rightLabel) $::diff($top,RevFile)
        set ::diff($top,rightFile) $::diff($top,RevFile)

        if {$type eq "CVS"} {
            GetCvsRev $::diff($top,RevFile) $::diff($top,leftFile) $r
        } elseif {$type eq "RCS"} {
            GetRcsRev $::diff($top,RevFile) $::diff($top,leftFile) $r
        } else {
            GetCtRev $::diff($top,RevFile) $::diff($top,leftFile) $r
        }
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
        if {$type eq "CVS"} {
            GetCvsRev $::diff($top,RevFile) $::diff($top,leftFile) $r1
            GetCvsRev $::diff($top,RevFile) $::diff($top,rightFile) $r2
        } elseif {$type eq "RCS"} {
            GetRcsRev $::diff($top,RevFile) $::diff($top,leftFile) $r1
            GetRcsRev $::diff($top,RevFile) $::diff($top,rightFile) $r2
        } else {
            GetCtRev $::diff($top,RevFile) $::diff($top,leftFile) $r1
            GetCtRev $::diff($top,RevFile) $::diff($top,rightFile) $r2
        }
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
    CvsCommitFile $top $::diff($top,RevFile)
}

# Check in CVS controlled file
proc CvsCommitFile {top filename} {
    set logmsg [LogDialog $top $filename]
    if {$logmsg ne ""} {
        catch {exec cvs -q commit -m $logmsg $filename}
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
