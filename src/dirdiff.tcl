#----------------------------------------------------------------------
#  Eskil, Directory diff section
#
#  Copyright (c) 1998-2007, Peter Spjuth  (peter.spjuth@space.se)
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

# Compare file names
proc FStrCmp {s1 s2} {
    # Equality is based on platform's standard
    # Order is dictionary order

    # Exact equal is equal regardless of platform.
    if {$s1 eq $s2} {
        return 0
    }
    # Accept case insensitive equality on windows
    if {$::tcl_platform(platform) eq "windows"} {
        if {[string equal -nocase $s1 $s2]} {
            return 0
        }
    }
    # FIXA: What's the case on Mac?
    
    if {[lindex [lsort -dictionary [list $s1 $s2]] 0] eq $s1} {
        return -1
    }
    return 1
}

# Sort file names
proc Fsort {l} {
    lsort -dictionary $l
}

# Compare two files or dirs
# Return true if equal
proc CompareFiles {file1 file2} {
    global Pref
    if {[catch {file stat $file1 stat1}]} {
        return 0
    }
    if {[catch {file stat $file2 stat2}]} {
        return 0
    }

    # Same type?
    set isdir1 [FileIsDirectory $file1]
    set isdir2 [FileIsDirectory $file2]
    if {$isdir1 != $isdir2} {
	return 0
    }
    # If contents is not checked, same size is enough to be equal
    if {$stat1(size) == $stat2(size) && $Pref(dir,comparelevel) == 0} {
        return 1
    }
    set ignorekey $Pref(dir,ignorekey)
    # Different size is enough when doing binary compare
    if {$stat1(size) != $stat2(size) && $Pref(dir,comparelevel) == 2 \
        && !$ignorekey} {
        return 0
    }
    # Same size and time is always considered equal
    if {$stat1(size) == $stat2(size) && $stat1(mtime) == $stat2(mtime)} {
	return 1
    }
    # Don't check further if contents should not be checked
    if {$Pref(dir,comparelevel) == 0} {
        return 0
    }
    # Don't check further if any is a directory
    if {$isdir1 || $isdir2} {
        # Consider dirs equal until we implement something recursive
	return 1
    }

    switch $Pref(dir,comparelevel) {
        2 -
        1 { # Check contents internally
            set bufsz 65536
            set eq 1
            set ch1 [open $file1 r]
            set ch2 [open $file2 r]
            if {$Pref(dir,comparelevel) == 2} {
                fconfigure $ch1 -translation binary
                fconfigure $ch2 -translation binary
            }
            if {$ignorekey} {
                # Assume that all keywords are in the first block
                set f1 [read $ch1 $bufsz]
                set f2 [read $ch2 $bufsz]
                regsub -all {\$\w+:[^\$]*\$} $f1 {} f1
                regsub -all {\$\w+:[^\$]*\$} $f2 {} f2
                # Compensate for any change in length
                if {[string length $f1] < [string length $f2]} {
                    append f1 [read $ch1 [expr {[string length $f2] - [string length $f1]}]]
                }
                if {[string length $f2] < [string length $f1]} {
                    append f2 [read $ch2 [expr {[string length $f1] - [string length $f2]}]]
                }
                if {![string equal $f1 $f2]} {
                    set eq 0
                }
            }
            while {$eq && ![eof $ch1] && ![eof $ch2]} {
                set f1 [read $ch1 $bufsz]
                set f2 [read $ch2 $bufsz]
                if {![string equal $f1 $f2]} {
                    set eq 0
                }
            }
            if {![eof $ch1] || ![eof $ch2]} {
                set eq 0
            }
            close $ch1
            close $ch2
        }
    }
    return $eq
}

# Returns the contents of a directory as a sorted list of file tails.
proc DirContents {dir} {
    global Pref
    set files [glob -tails -directory $dir -nocomplain * {.[a-zA-Z]*}]

    if {$Pref(dir,onlyrev)} {
        # FIXA: move to rev and make general for other systems
        set entries [file join $dir CVS Entries]
        if {[file exists $entries]} {
            set ch [open $entries r]
            set data [read $ch]
            close $ch
            foreach line [split $data \n] {
                set name [lindex [split $line /] 1]
                set controlled($name) 1
            }
            set files2 {}
            foreach file $files {
                if {[info exists controlled($file)]} {
                    lappend files2 $file
                }
            }
            set files $files2
        }
    }

    set files2 {}
    foreach file $files {
        set full [file join $dir $file]
        # Apply filters
        if {[FileIsDirectory $full]} {
            if {[llength $Pref(dir,incdirs)] == 0} {
                set allowed 1
            } else {
                set allowed 0
                foreach pat $Pref(dir,incdirs) {
                    if {[string match $pat $file]} {
                        set allowed 1
                        break
                    }
                }
            }
            if {$allowed} {
                foreach pat $Pref(dir,exdirs) {
                    if {[string match $pat $file]} {
                        set allowed 0
                        break
                    }
                }
            }
            if {!$allowed} continue
        } else {
            if {[llength $Pref(dir,incfiles)] == 0} {
                set allowed 1
            } else {
                set allowed 0
                foreach pat $Pref(dir,incfiles) {
                    if {[string match $pat $file]} {
                        set allowed 1
                        break
                    }
                }
            }
            if {$allowed} {
                foreach pat $Pref(dir,exfiles) {
                    if {[string match $pat $file]} {
                        set allowed 0
                        break
                    }
                }
            }
            if {!$allowed} continue
        }
        lappend files2 $file
    }

    return [Fsort $files2]
}

# Bring up an editor to display a file.
proc EditFile {file} {
    locateEditor ::util(editor)
    exec $::util(editor) $file &
}

# Pick a directory for compare
proc BrowseDir {dirVar entryW} {
    global Pref
    upvar "#0" $dirVar dir

    set newdir $dir
    while {$newdir != "." && ![FileIsDirectory $newdir]} {
        set newdir [file dirname $newdir]
    }
    set newdir [tk_chooseDirectory -initialdir $newdir -title "Select Directory"]
    if {$newdir != ""} {
        set dir $newdir
        $entryW xview end
    }
}

snit::widget DirCompare {
    component tree
    component hsb
    component vsb

    option -leftdir  -default "" -configuremethod SetDirOption
    option -rightdir -default "" -configuremethod SetDirOption

    variable AfterId
    variable IdleQueue
    variable IdleQueueArr
    variable leftMark ""
    variable rightMark ""

    constructor {args} {
        install tree using ttk::treeview $win.tree -height 20 \
                -columns {type status leftfull leftname leftsize leftdate rightfull rightname rightsize rightdate} \
                -displaycolumns {leftname leftsize leftdate rightname rightsize rightdate}
        if {[tk windowingsystem] ne "aqua"} {
            install vsb using ttk::scrollbar $win.vsb -orient vertical \
                    -command "$tree yview"
            install hsb using ttk::scrollbar $win.hsb -orient horizontal \
                    -command "$tree xview"
        } else {
            install vsb using scrollbar $win.vsb -orient vertical \
                    -command "$tree yview"
            install hsb using scrollbar $win.hsb -orient horizontal \
                    -command "$tree xview"
        }
        $tree configure -yscroll "$vsb set" -xscroll "$hsb set"

        set AfterId ""
        set IdleQueue {}

        $tree heading \#0 -text "Structure"
        $tree heading leftname -text "Name"
        $tree heading leftsize -text "Size"
        $tree heading leftdate -text "Date"
        $tree heading rightname -text "Name"
        $tree heading rightsize -text "Size"
        $tree heading rightdate -text "Date"

        $tree column leftsize  -stretch 0 -width 70 -anchor e
        $tree column rightsize -stretch 0 -width 70 -anchor e
        $tree column leftdate  -stretch 0 -width 120
        $tree column rightdate -stretch 0 -width 120

        $tree tag configure unknown -foreground grey
        $tree tag configure empty   -foreground grey
        $tree tag configure equal   -foreground {}
        $tree tag configure new     -foreground green
        $tree tag configure old     -foreground blue
        $tree tag configure change  -foreground red

        bind $tree <<TreeviewOpen>> "[mymethod UpdateDirNode] \[%W focus\]"
        bind $tree <Button-3> "[mymethod ContextMenu] %x %y %X %Y"
        bind $tree <Double-ButtonPress-1> "[mymethod DoubleClick] %x %y"

        grid $tree $vsb -sticky nsew
        grid $hsb         -sticky nsew
        grid columnconfigure $win 0 -weight 1
        grid rowconfigure    $win 0 -weight 1

        $self configurelist $args
        #$self ReStart
    }
    destructor {
        if {$AfterId ne ""} {
            after cancel $AfterId
        }
        set AfterId ""
    }

    method SetDirOption {option value} {
        set options($option) $value
        if {$options(-leftdir) ne "" && \
                [file isdirectory $options(-leftdir)] && \
                $options(-rightdir) ne "" && \
                [file isdirectory $options(-rightdir)]} {
            after idle [mymethod ReStart]
        }
    }

    method ReStart {} {
        # Delete all idle processing
        if {$AfterId ne ""} {
            after cancel $AfterId
        }
        set AfterId ""
        set IdleQueue {}
        array unset IdleQueueArr
        
        # Fill in clean root data
        $tree delete [$tree children {}]
        $tree set {} type directory
        $self SetNodeStatus {} empty
        $tree set {} leftfull   $options(-leftdir)
        $tree set {} leftname   [file tail $options(-leftdir)]
        $tree set {} rightfull  $options(-rightdir)
        $tree set {} rightname  [file tail $options(-rightdir)]

        $self UpdateDirNode {}
    }

    # Format a time stamp for display
    proc FormatDate {date} {
        clock format $date -format "%Y-%m-%d %H:%M:%S"
    }

    # Remove all equal nodes from tree
    method PruneEqual {} {
        set todo [$tree children {}]
        while {[llength $todo] > 0} {
            set todoNow $todo
            set todo {}
            foreach node $todoNow {
                set status [$tree set $node status]
                if {$status eq "equal"} {
                    $tree delete $node
                } else {
                    eval lappend todo [$tree children $node]
                }
            }
        }
    }

    # Open or close all directories in the tree view
    method OpenAll {{state 1}} {
        set todo [$tree children {}]
        while {[llength $todo] > 0} {
            set todoNow $todo
            set todo {}
            foreach node $todoNow {
                set children [$tree children $node]
                if {[llength $children] > 0} {
                    $tree item $node -open $state
                    eval lappend todo $children
                }
            }
        }
    }

    # Copy a file from one directory to the other
    method CopyFile {node from} {
        global dirdiff Pref

        set lf [$tree set $node leftfull]
        set rf [$tree set $node rightfull]
        set parent [$tree parent $node]
        set lp [$tree set $parent leftfull]
        set rp [$tree set $parent rightfull]

        if {$from eq "left"} {
            set src $lf
            if {$rf ne ""} {
                set dst $rf
            } elseif {$rp ne ""} {
                set dst [file join $rp [file tail $src]]
            } else {
                return
            }
        } elseif {$from eq "right"} {
            set src $rf
            if {$lf ne ""} {
                set dst $lf
            } elseif {$lp ne ""} {
                set dst [file join $lp [file tail $src]]
            } else {
                return
            }
        } else {
            error "Bad to argument to CopyFile: $to"
        }

        if {[file exists $dst]} {
            if {[tk_messageBox -icon question -title "Overwrite file?" -message \
                    "Copy\n$src\noverwriting\n$dst ?" -type yesno] eq "yes"} {
                file copy -force $src $dst
                # FIXA: update file info in tree too
                $self SetNodeStatus $node equal
            }
        } else {
            if {[tk_messageBox -icon question -title "Copy file?" -message \
                    "Copy\n$src\nto\n$dst ?" -type yesno] eq "yes"} {
                file copy $src $dst
                # FIXA: update file info in tree too
                $self SetNodeStatus $node equal
            }
        }
    }

    # React on double-click
    method DoubleClick {x y} {
        set node [$tree identify row $x $y]

        set lf [$tree set $node leftfull]
        set rf [$tree set $node rightfull]
        set type [$tree set $node type]

        # On a file that exists on both sides, start a file diff
        if {$type eq "file" && $lf ne "" && $rf ne ""} {
            newDiff $lf $rf
            # Stop the default bindings from running
            break
        }
    }

    # Bring up a context menu on a file.
    method ContextMenu {x y X Y} {
        #global dirdiff Pref

        set node [$tree identify row $x $y]
        set col [$tree identify column $x $y]
        set colname [$tree column $col -id]

        set lf [$tree set $node leftfull]
        set rf [$tree set $node rightfull]
        set type [$tree set $node type]

        set m $win.popup
        destroy $m
        menu $m
        
        if {$col eq "#0"} {
            $m add command -label "Prune equal" -command [mymethod PruneEqual]
            $m add command -label "Expand all" -command [mymethod OpenAll]
            $m add command -label "Collaps all" -command [mymethod OpenAll 0]
        }

        if {$type eq "file" && $lf ne "" && $rf ne ""} {
            # Files, both exist
            $m add command -label "Compare Files" -command [list \
                    newDiff $lf $rf]
        }
        if {[string match left* $colname] && $lf ne ""} {
            $m add command -label "Copy File" \
                    -command [mymethod CopyFile $node left]
            $m add command -label "Edit File" \
                    -command [list EditFile $lf]
            $m add command -label "Mark File" \
                    -command [list set [myvar leftMark] $lf]
            if {$rightMark != ""} {
                $m add command -label "Compare with $rightMark" \
                        -command [list newDiff $lf $rightMark]
            }
        } elseif {[string match right* $colname] && $rf ne ""} {
            $m add command -label "Copy File" \
                    -command [mymethod CopyFile $node right]
            $m add command -label "Edit File" \
                    -command [list EditFile $rf]
            $m add command -label "Mark File" \
                    -command [list set [myvar rightMark] $rf]
            if {$leftMark != ""} {
                $m add command -label "Compare with $leftMark" \
                        -command [list newDiff $leftMark $rf]
            }
        }

        tk_popup $m $X $Y
    }

    method AddNodeToIdle {node} {
        if {[info exists IdleQueueArr($node)]} { return }
        lappend IdleQueue $node
        set IdleQueueArr($node) 1

        if {$AfterId eq ""} {
            set AfterId [after 1 [mymethod UpdateIdle]]
        }
    }
    method UpdateIdle {} {
        set AfterId "X"

        set pre [clock clicks -milliseconds]
        while {[llength $IdleQueue] > 0} {
            set node [lindex $IdleQueue 0]
            set IdleQueue [lrange $IdleQueue 1 end]
            unset IdleQueueArr($node)

            if {[$tree set $node type] ne "directory"} {
                $self UpdateFileNode $node
            } else {
                $self UpdateDirNode $node
            }
            # Work for at least 20 ms to keep things efficient
            set post [clock clicks -milliseconds]
            if {($post - $pre) > 20} break
        }

        if {[llength $IdleQueue] > 0} {
            set AfterId [after 1 [mymethod UpdateIdle]]
        } else {
            set AfterId ""
        }
    }

    method SetNodeStatus {node status} {
        $tree set $node status $status
        $tree item $node -tags $status
        #puts "Set [$tree item $node -text] to $status"

        # Loop through children to update parent
        set parent [$tree parent $node]
        if {$parent eq ""} { return }

        # If this is only present on one side, there is no need to update
        set lf [$tree set $parent leftfull]
        set rf [$tree set $parent rightfull]
        if {$lf eq "" || $rf eq ""} { return }

        set pstatus equal
        foreach child [$tree children $parent] {
            set status [$tree set $child status]
            switch $status {
                unknown {
                    set pstatus unknown
                    break
                }
                new - old - change {
                    set pstatus change
                }
            }
        }
        #puts "Setting parent [$tree set $parent leftname] to $pstatus"
        $self SetNodeStatus $parent $pstatus
    }

    method UpdateDirNode {node} {
        if {[$tree set $node type] ne "directory"} {
            return
        }
        if {[$tree set $node status] ne "empty"} {
            #puts "Dir [$tree set $node leftfull] already done"
            return
        }
        $tree delete [$tree children $node]

        set leftfull [$tree set $node leftfull]
        set rightfull [$tree set $node rightfull]
        $self CompareDirs $leftfull $rightfull $node
    }

    method UpdateFileNode {node} {
        set leftfull [$tree set $node leftfull]
        set rightfull [$tree set $node rightfull]
        set equal [CompareFiles $leftfull $rightfull]
        if {$equal} {
            $self SetNodeStatus $node equal
        } else {
            $self SetNodeStatus $node change
        }
            
        #$self CompareDirs $leftfull $rightfull $node

        #$self SetNodeStatus $node unknown
        #$tree set $node leftfull
        #$tree set $node leftname
        #$tree set $node rightfull
        #$tree set $node rightname
    }

    # List files under a directory node
    # Returns status for the new node
    method ListFiles {df1 df2 node} {
        if {$df1 ne ""} {
            set type [file type $df1]
            set name [file tail $df1]
        } else {
            set type [file type $df2]
            set name [file tail $df2]
        }
        if {[catch {file stat $df1 stat1}]} {
            set size1 ""
            set time1 ""
        } else {
            set size1 $stat1(size)
            set time1 [FormatDate $stat1(mtime)]
        }
        if {[catch {file stat $df2 stat2}]} {
            set size2 ""
            set time2 ""
        } else {
            set size2 $stat2(size)
            set time2 [FormatDate $stat2(mtime)]
        }
        if {$type eq "directory"} {
            set values [list $type unknown \
                    $df1 "" "" "" \
                    $df2 "" "" ""]
        } else {
            set values [list $type unknown \
                    $df1 [file tail $df1] $size1 $time1 \
                    $df2 [file tail $df2] $size2 $time2]
        }
        set id [$tree insert $node end -text $name \
                -values $values]
        if {$type eq "directory"} {
            ## Make it so that this node is openable
            $tree insert $id 0 -text dummy ;# a dummy
            $tree item $id -text $name/
            $self SetNodeStatus $id empty
            $self AddNodeToIdle $id
        } elseif {$size1 == $size2 && \
                $time1 == $time2} {
            $self SetNodeStatus $id equal
        } elseif {$size1 == ""} {
            $self SetNodeStatus $id new
        } elseif {$size2 == ""} {
            $self SetNodeStatus $id old
        } else {
            $self SetNodeStatus $id unknown
            $self AddNodeToIdle $id
        }
        return [$tree set $id status]
    }

    # Compare two directories.
    method CompareDirs {dir1 dir2 node} {
        global Pref
        if {$dir1 eq ""} {
            set files1 {}
        } else {
            set files1 [DirContents $dir1]
        }
        if {$dir2 eq ""} {
            set files2 {}
        } else {
            set files2 [DirContents $dir2]
        }

        set len1 [llength $files1]
        set len2 [llength $files2]

        set p1 0
        set p2 0
        set status_change 0
        set status_unknown 0
        while 1 {
            if {$p1 < $len1 && $p2 < $len2} {
                set f1 [lindex $files1 $p1]
                set df1 [file join $dir1 $f1]
                set f2 [lindex $files2 $p2]
                set df2 [file join $dir2 $f2]
                set apa [FStrCmp $f1 $f2]
                if {$apa == 0} {
                    # Equal names, separate them if not the same type
                    set apa [expr {- [FileIsDirectory $df1] \
                                   + [FileIsDirectory $df2]}]
                }

                switch -- $apa {
                    0 {
                        set sts [$self ListFiles $df1 $df2 $node]
                        incr p1
                        incr p2
                        if {$sts eq "unknown"} {
                            set status_unknown 1
                        }
                    }
                    -1 {
                        $self ListFiles $df1 "" $node
                        incr p1
                        set status_change 1
                    }
                    1 {
                        $self ListFiles "" $df2 $node
                        incr p2
                        set status_change 1
                    }
                }
            } elseif {$p1 < $len1 && $p2 >= $len2} {
                set f1 [lindex $files1 $p1]
                $self ListFiles [file join $dir1 $f1] "" $node
                incr p1
                set status_change 1
            } elseif {$p1 >= $len1 && $p2 < $len2} {
                set f2 [lindex $files2 $p2]
                $self ListFiles "" [file join $dir2 $f2] $node
                incr p2
                set status_change 1
            } else {
                break
            }
        }
        if {$dir1 eq ""} {
            set status new
        } elseif {$dir2 eq ""} {
            set status old
        } elseif {$status_change} {
            set status change
        } elseif {$status_unknown} {
            set status unknown
        } else {
            set status equal
        }
        $self SetNodeStatus $node $status
    }
}

snit::widget DirDiff {
    hulltype toplevel
    component tree

    constructor {args} {
        lappend ::diff(diffWindows) $win
        wm title $win "Eskil Dir"
        wm protocol $win WM_DELETE_WINDOW [list cleanupAndExit $win]

        install tree using DirCompare $win.dc -leftdir $::dirdiff(leftDir) \
                -rightdir $::dirdiff(rightDir)

        frame $win.fe1
        frame $win.fe2

        menu $win.m
        $hull configure -menu $win.m

        $win.m add cascade -menu $win.m.mf -label "File" -underline 0
        menu $win.m.mf
        $win.m.mf add command -label "Compare" -underline 1 \
                -command [mymethod DoDirCompare] -accelerator "Alt-c"
        bind $win <Alt-c> [mymethod DoDirCompare]
        $win.m.mf add separator
        $win.m.mf add command -label "Close" -underline 0 \
                -command [list cleanupAndExit $win]
        $win.m.mf add separator
        $win.m.mf add command -label "Quit" -underline 0 \
                -command [list cleanupAndExit all]

        $win.m add cascade -menu $win.m.mo -label "Preferences" -underline 0
        menu $win.m.mo
        $win.m.mo add command -label "Prefs..." -command makeDirDiffPrefWin
        $win.m.mo add cascade -label "Check" -menu $win.m.mo.mc

        menu $win.m.mo.mc
        $win.m.mo.mc add radiobutton -variable Pref(dir,comparelevel) -value 0 \
                -label "Do not check contents"
        $win.m.mo.mc add radiobutton -variable Pref(dir,comparelevel) -value 1 \
                -label "Normal compare"
        $win.m.mo.mc add radiobutton -variable Pref(dir,comparelevel) -value 2 \
                -label "Binary compare"
        $win.m.mo.mc add checkbutton -variable Pref(dir,ignorekey) \
                -label "Ignore \$Keyword:\$"
        
        $win.m add cascade -label "Tools" -underline 0 -menu $win.m.mt
        menu $win.m.mt
        $win.m.mt add command -label "New Diff Window" -underline 0 \
                -command makeDiffWin
        $win.m.mt add command -label "Clip Diff" -underline 0 \
                -command makeClipDiffWin
        if {$::tcl_platform(platform) eq "windows"} {
            if {![catch {package require registry}]} {
                $win.m.mt add separator
                $win.m.mt add command -label "Setup Registry" -underline 6 \
                        -command makeRegistryWin
            }
        }
        
        $win.m add cascade -label "Help" -underline 0 -menu $win.m.help
        menu $win.m.help
        $win.m.help add command -label "Tutorial" -command makeTutorialWin \
                -underline 0
        $win.m.help add command -label "About" -command makeAboutWin -underline 0
        
        if {$::debug} {
            $win.m add cascade -label "Debug" -menu $win.m.md -underline 0
            menu $win.m.md
            if {$::tcl_platform(platform) eq "windows"} {
                $win.m.md add checkbutton -label "Console" -variable consolestate \
                        -onvalue show -offvalue hide -command {console $consolestate}
                $win.m.md add separator
            }
            $win.m.md add command -label "Reread Source" -underline 0 \
                    -command {EskilRereadSource}
            $win.m.md add separator
            $win.m.md add command -label "Redraw Window" -command {makeDirDiffWin 1}
        }
        
        button $win.bu -text "Up Both" -command [mymethod UpDir] \
                -underline 0 -padx 15
        bind $win <Alt-u> "$win.bu invoke"
        
        #catch {font delete myfont}
        #font create myfont -family $Pref(fontfamily) -size $Pref(fontsize)

        entry $win.e1 -textvariable dirdiff(leftDir)
        button $win.bu1 -text "Up" -padx 10 -command [mymethod UpDir 1]
        button $win.bb1 -text "Browse"  -padx 10 \
                -command "[list BrowseDir dirdiff(leftDir) $win.e1]
                          [mymethod DoDirCompare]"
        $win.e1 xview end
        entry $win.e2 -textvariable dirdiff(rightDir)
        button $win.bu2 -text "Up" -padx 10 -command [mymethod UpDir 2]
        button $win.bb2 -text "Browse" -padx 10 \
                -command "[list BrowseDir dirdiff(rightDir) $win.e2]
                          [mymethod DoDirCompare]"
        $win.e2 xview end
        bind $win.e1 <Return> [mymethod DoDirCompare]
        bind $win.e2 <Return> [mymethod DoDirCompare]
        
        pack $win.bb1 $win.bu1 -in $win.fe1 -side right -pady 1
        pack $win.bu1 -padx 6
        pack $win.e1 -in $win.fe1 -side left -fill x -expand 1
        pack $win.bb2 $win.bu2 -in $win.fe2 -side right -pady 1
        pack $win.bu2 -padx 6
        pack $win.e2 -in $win.fe2 -side left -fill x -expand 1
        
        grid $win.fe1  $win.bu $win.fe2  -sticky we
        grid $tree     -       -         -sticky news
        grid $win.bu -padx 6

        grid rowconfigure    $win  2    -weight 1
        grid columnconfigure $win {0 2} -weight 1
    }

    method DoDirCompare {} {
        $tree configure -leftdir $::dirdiff(leftDir) \
                -rightdir $::dirdiff(rightDir)
    }

    # Go up one level in directory hierarchy.
    # 0 = both
    method UpDir {{n 0}} {
        global dirdiff Pref
        switch $n {
            0 {
                set dirdiff(leftDir) [file dirname $dirdiff(leftDir)]
                set dirdiff(rightDir) [file dirname $dirdiff(rightDir)]
                .dirdiff.e1 xview end
                .dirdiff.e2 xview end
            }
            1 {
                set dirdiff(leftDir) [file dirname $dirdiff(leftDir)]
                .dirdiff.e1 xview end
            }
            2 {
                set dirdiff(rightDir) [file dirname $dirdiff(rightDir)]
                .dirdiff.e2 xview end
            }
        }
        $self DoDirCompare
    }
}

# Transfer preferences from dialog to real settings
proc ApplyDirDiffPref {} {
    foreach item {
        dir,comparelevel
        dir,ignorekey
        dir,onlyrev
    } {
        set ::Pref($item) $::TmpPref($item)
    }
    # Handle preferences that must be a list
    foreach item {
        dir,incfiles
        dir,exfiles
        dir,incdirs
        dir,exdirs
    } {
        # Force a split to make sure the list is valid
        if {[catch {llength $::TmpPref($item)}]} {
            set ::TmpPref($item) [regexp -all -inline {\S+} $::TmpPref($item)]
        }
        set ::Pref($item) $::TmpPref($item)
    }
}

# Create directory diff preferences window.
proc makeDirDiffPrefWin {} {
    set top .dirdiffprefs
    if {[winfo exists $top] && [winfo toplevel $top] eq $top} {
        raise $top
        focus -force $top
        return
    } else {
        destroy $top
        toplevel $top -padx 3 -pady 3
        foreach item {
            dir,comparelevel
            dir,ignorekey
            dir,incfiles
            dir,exfiles
            dir,incdirs
            dir,exdirs
            dir,onlyrev
        } {
            set ::TmpPref($item) $::Pref($item)
        }
    }

    wm title $top "Eskil Directory Preferences"

    set check [labelframe $top.check -text "Check" -padx 3 -pady 3]
    radiobutton $check.rb1 -variable TmpPref(dir,comparelevel) -value 0 \
            -text "Do not check contents"
    radiobutton $check.rb2 -variable TmpPref(dir,comparelevel) -value 1 \
            -text "Normal compare"
    radiobutton $check.rb3 -variable TmpPref(dir,comparelevel) -value 2 \
            -text "Binary compare"
    grid $check.rb1 -sticky w -padx 3 -pady 3
    grid $check.rb2 -sticky w -padx 3 -pady 3
    grid $check.rb3 -sticky w -padx 3 -pady 3
    grid columnconfigure $check {0 1 2} -uniform a -weight 1

    set opts [labelframe $top.opts -text "Options" -padx 3 -pady 3]
    checkbutton $opts.cb1 -variable TmpPref(dir,ignorekey) \
            -text "Ignore \$Keyword:\$"
    eval pack [winfo children $opts] -side top -anchor w 

    set filter [labelframe $top.filter -text "Filter" -padx 3 -pady 3]
    label $filter.l1 -text "Include Files" -anchor w
    entry $filter.e1 -width 20 -textvariable TmpPref(dir,incfiles)
    label $filter.l2 -text "Exclude Files" -anchor w
    entry $filter.e2 -width 20 -textvariable TmpPref(dir,exfiles)
    label $filter.l3 -text "Include Dirs" -anchor w
    entry $filter.e3 -width 20 -textvariable TmpPref(dir,incdirs)
    label $filter.l4 -text "Exclude Dirs" -anchor w
    entry $filter.e4 -width 20 -textvariable TmpPref(dir,exdirs)
    checkbutton $filter.cb1 -text "Only revision controlled" \
            -variable TmpPref(dir,onlyrev)
    grid $filter.l1 $filter.e1 -sticky we -padx 3 -pady 3
    grid $filter.l2 $filter.e2 -sticky we -padx 3 -pady 3
    grid $filter.l3 $filter.e3 -sticky we -padx 3 -pady 3
    grid $filter.l4 $filter.e4 -sticky we -padx 3 -pady 3
    grid $filter.cb1 - -sticky w -padx 3 -pady 3
    grid columnconfigure $filter 1 -weight 1

    set fb [frame $top.fb -padx 3 -pady 3]
    button $fb.ok -width 10 -text "Ok" \
            -command "ApplyDirDiffPref ; destroy $top"
    button $fb.ap -width 10 -text "Apply" -command ApplyDirDiffPref
    button $fb.ca -width 10 -text "Cancel" -command "destroy $top"
    grid $fb.ok $fb.ap $fb.ca -padx 3 -pady 3
    grid columnconfigure $fb {0 1 2} -uniform a -weight 1

    pack $fb -side bottom -fill x
    pack $check $opts $filter -side top -fill x
}

# Experimental...
#preprocess filter på namnen så man kan jämföra bibliotek
#med ändrade namn.
proc makeRegSubWin {} {
    set top .ddregsub
    if {[winfo exists $top] && [winfo toplevel $top] eq $top} {
        raise $top
        focus -force $top
        return
    } else {
        destroy $top
        toplevel $top
    }

    wm title $top "Eskil Dir Preprocess"

    entry $top.e1 -textvariable ::dirdiff(pattern) -width 15
    entry $top.e2 -textvariable ::dirdiff(replace) -width 15

    label $top.l1 -text "Pattern" -anchor w
    label $top.l2 -text "Subst"   -anchor w
    
    grid $top.l1 $top.e1 -sticky we
    grid $top.l2 $top.e2 -sticky we
    grid columnconfigure $top 1 -weight 1
    grid rowconfigure    $top 2 -weight 1
    
}

proc makeDirDiffWin {{redraw 0}} {
    DirDiff .dirdiff
}

