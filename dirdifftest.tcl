#!/home/peter/tcl/install/bin/tclsh8.5
lappend auto_path [pwd]/eskil.vfs/lib/

package require Tk
package require Ttk
package require tile
package require snit

set ::eskil_testsuite 1
source src/eskil.tcl

set dirdiff(leftDir) /home/peter/tcl/tcl
set dirdiff(rightDir) /home/peter/tcl/tclmaster

#set dirdiff(leftDir) /home/peter/src/eskil/gugg1
#set dirdiff(rightDir) /home/peter/src/eskil/gugg2

# Compare file names
proc FStrCmp {s1 s2} {
    # On Unix filenames are case sensitive
    if {$::tcl_platform(platform) eq "unix"} {
	return [string compare $s1 $s2]
    }
    string compare -nocase $s1 $s2
}

# Sort file names
proc Fsort {l} {
    if {$::tcl_platform(platform) eq "unix"} {
	return [lsort $l]
    }
    # Case insensitive on windows
    lsort -dictionary $l
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
            if {$Pref(nodir)} continue
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
    if {$stat1(size) == $stat2(size) && $Pref(comparelevel) == 0} {
        return 1
    }
    set ignorekey $Pref(dir,ignorekey)
    # Different size is enough when doing binary compare
    if {$stat1(size) != $stat2(size) && $Pref(comparelevel) eq "1b" \
        && !$ignorekey} {
        return 0
    }
    # Same size and time is always considered equal
    if {$stat1(size) == $stat2(size) && $stat1(mtime) == $stat2(mtime)} {
	return 1
    }
    # Don't check further if contents should not be checked
    if {$Pref(comparelevel) == 0} {
        return 0
    }
    # Don't check further if any is a directory
    if {$isdir1 || $isdir2} {
        # Consider dirs equal until we implement something recursive
	return 1
    }

    switch $Pref(comparelevel) {
        1b -
        1 { # Check contents internally
            set bufsz 65536
            set eq 1
            set ch1 [open $file1 r]
            set ch2 [open $file2 r]
            if {$Pref(comparelevel) eq "1b"} {
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

snit::widget DirCompare {
    component tree
    component hsb
    component vsb

    option -leftdir
    option -rightdir

    variable AfterId
    variable IdleQueue
    variable IdleQueueArr

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

        $self configurelist $args
        set AfterId ""
        set IdleQueue {}

        $tree heading \#0 -text "Directory"
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

        # Fill in root data
        $tree set {} type       directory
        $self SetNodeStatus {} empty
        $tree set {} leftfull   $options(-leftdir)
        $tree set {} leftname   [file tail $options(-leftdir)]
        $tree set {} rightfull  $options(-rightdir)
        $tree set {} rightname  [file tail $options(-rightdir)]

        $self UpdateDirNode {}
        bind $tree <<TreeviewOpen>> "[mymethod UpdateDirNode] \[%W focus\]"
        bind $tree <Button-3> "[mymethod ContextMenu] %x %y %X %Y"

        grid $tree $vsb -sticky nsew
        grid $hsb         -sticky nsew
        grid columnconfigure $win 0 -weight 1
        grid rowconfigure    $win 0 -weight 1
    }
    destructor {

    }

    proc FormatDate {date} {
        clock format $date -format "%Y-%m-%d %H:%M:%S"
    }

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

    # Bring up a context menu on a file.
    method ContextMenu {x y X Y} {
        #global dirdiff Pref

        set node [$tree identify row $x $y]
        set col [$tree identify column $x $y]


        set lf [$tree set $node leftfull]
        #$tree set $node leftname
        set rf [$tree set $node rightfull]
        #$tree set $node rightname
        #set i [lindex $dirdiff(infoFiles) $row]
        set i 0

        set m $win.popup
        destroy $m
        menu $m
        
        if {$col eq "#0"} {
            $m add command -label "Prune equal" -command [mymethod PruneEqual]
            $m add command -label "Expand all" -command [mymethod OpenAll]
            $m add command -label "Collaps all" -command [mymethod OpenAll 0]
        }

        if {($i & 12) == 12} { # Both are dirs
            $m add command -label "Compare Directories" -command "
            [list set dirdiff(leftDir) $lf]
            [list set dirdiff(rightDir) $rf]
            [list if \$Pref(autocompare) "after idle doDirCompare"]
        "
        }
        if {$i & 4 && $w eq $dirdiff(wLeft)} { # Left is dir
        $m add command -label "Step down left directory" -command "
            [list set dirdiff(leftDir) $lf]
            [list if \$Pref(autocompare) "after idle doDirCompare"]
        "
        }
        if {$i & 8 && $w eq $dirdiff(wRight)} { # Right is dir
            $m add command -label "Step down right directory" -command "
            [list set dirdiff(rightDir) $rf]
            [list if \$Pref(autocompare) "after idle doDirCompare"]
        "
        }
        if {($i & 12) == 0 && ($i & 3) == 0} { # Neither is dir, Both exists
            $m add command -label "Compare Files" -command [list \
                    newDiff $lf $rf]
        }
        if {0 && $w eq $dirdiff(wLeft) && ($i & 13) == 0} {
            $m add command -label "Copy File" \
                    -command [list CopyFile $row right]
            $m add command -label "Edit File" \
                    -command [list EditFile $row left]
            $m add command -label "Mark File" \
                    -command [list set ::dirdiff(leftMark) $lf]
            if {$::dirdiff(rightMark) != ""} {
                $m add command -label "Compare with $::dirdiff(rightMark)" \
                        -command [list newDiff $lf $::dirdiff(rightMark)]
            }
        }
        if {0 && $w eq $dirdiff(wRight) && ($i & 14) == 0} {
            $m add command -label "Copy File" \
                    -command [list CopyFile $row left]
            $m add command -label "Edit File" \
                    -command [list EditFile $row right]
            $m add command -label "Mark File" \
                    -command [list set ::dirdiff(rightMark) $rf]
            if {$::dirdiff(leftMark) != ""} {
                $m add command -label "Compare with $::dirdiff(leftMark)" \
                        -command [list newDiff $::dirdiff(leftMark) $rf]
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
                        $self ListFiles $df1 $df2 $node
                        incr p1
                        incr p2
                        set status_unknown 1
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

proc makeWin {} {

    set w [DirCompare .f -leftdir $::dirdiff(leftDir) -rightdir $::dirdiff(rightDir)]
    pack $w -fill both -expand 1
}

makeWin


