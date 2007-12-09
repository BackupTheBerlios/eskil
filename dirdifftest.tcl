#!/home/peter/tcl/install/bin/tclsh8.5
lappend auto_path [pwd]/eskil.vfs/lib/

package require Tk
package require Ttk
package require tile
package require snit

set dirdiff(leftDir) /home/peter/tcl/tcl
set dirdiff(rightDir) /home/peter/tcl/tclmaster

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
        install tree using ttk::treeview %AUTO%
        $self configurelist $args
        set AfterId ""
        set IdleQueue {}
    }
    destructor {

    }

    method AddNodeToIdle {node} {
        if {[info exists IdleQueueArr($node)]} { return }
        lappend IdleQueue $node
        set IdleQueueArr($node) 1

        if {$AfterId eq ""} {
            set AfterId [after idle [list UpdateIdle $tree]]
        }
    }
}

proc InitIdleHandler {tree} {
    if {![info exists ::IdleHandler($tree,afterid)]} {
        set ::IdleHandler($tree,afterid) ""
    }
    if {$::IdleHandler($tree,afterid) ne ""} {
        after cancel $::IdleHandler($tree,afterid)
        set ::IdleHandler($tree,afterid) ""
    }
    #array unset ::IdleHandler $tree,*
    set ::IdleHandler($tree) {}
}

proc AddNodeToIdle {tree node} {
    if {[info exists ::IdleHandler($tree,$node)]} { return }
    lappend ::IdleHandler($tree) $node
    set ::IdleHandler($tree,$node) 1

    if {$::IdleHandler($tree,afterid) eq ""} {
        set ::IdleHandler($tree,afterid) [after idle [list UpdateIdle $tree]]
    }
}

proc UpdateIdle {tree} {
    set ::IdleHandler($tree,afterid) {}

    if {[llength $::IdleHandler($tree)] > 0} {
        set node [lindex $::IdleHandler($tree) 0]
        set ::IdleHandler($tree) [lrange $::IdleHandler($tree) 1 end]
        unset ::IdleHandler($tree,$node)

        UpdateDirNode $tree $node
    }

    if {[llength $::IdleHandler($tree)] > 0} {
        set ::IdleHandler($tree,afterid) [after idle [list UpdateIdle $tree]]
    }
}

proc UpdateDirNode {tree node} {
    if {[$tree set $node type] ne "directory"} {
	return
    }
    if {[$tree set $node status] ne "empty"} {
        puts "Dir [$tree set $node leftfull] already done"
	return
    }
    $tree delete [$tree children $node]

    set leftfull [$tree set $node leftfull]
    set leftall [lsort -dictionary [glob -nocomplain -dir $leftfull *]]
    foreach f $leftall {
	set type [file type $f]
	set id [$tree insert $node end -text [file tail $f] \
		-values [list $type unknown $f [file tail $f] 0 0 $f [file tail $f] 0 0]]
	if {$type eq "directory"} {
	    ## Make it so that this node is openable
            $tree set $id status empty
	    $tree insert $id 0 -text dummy ;# a dummy
	    $tree item $id -text [file tail $f]/
            AddNodeToIdle $tree $id
        }
    }

    $tree set $node status unknown
    $tree set $node leftfull
    $tree set $node leftname
    $tree set $node rightfull
    $tree set $node rightname
}

proc makeWin {} {

    set top .dirdiff
    toplevel $top

    set w [frame $top.f]
    ## Create the tree and set it up
    ttk::treeview $w.tree \
            -columns {type status leftfull leftname leftsize leftdate rightfull rightname rightsize rightdate} \
            -displaycolumns {leftname leftsize leftdate rightname rightsize rightdate} \
            -yscroll "$w.vsb set" -xscroll "$w.hsb set"
    if {[tk windowingsystem] ne "aqua"} {
        ttk::scrollbar $w.vsb -orient vertical -command "$w.tree yview"
        ttk::scrollbar $w.hsb -orient horizontal -command "$w.tree xview"
    } else {
        scrollbar $w.vsb -orient vertical -command "$w.tree yview"
        scrollbar $w.hsb -orient horizontal -command "$w.tree xview"
    }
    $w.tree heading \#0 -text "Directory"
    $w.tree heading leftname -text "Name"
    $w.tree heading leftsize -text "Size"
    $w.tree heading leftdate -text "Date"
    $w.tree heading rightname -text "Name"
    $w.tree heading rightsize -text "Size"
    $w.tree heading rightdate -text "Date"

    $w.tree column leftsize  -stretch 0 -width 70
    $w.tree column rightsize -stretch 0 -width 70
    $w.tree column leftdate  -stretch 0 -width 70
    $w.tree column rightdate -stretch 0 -width 70

    # Fill in root data
    $w.tree set {} type       directory
    $w.tree set {} status     empty
    $w.tree set {} leftfull   $::dirdiff(leftDir)
    $w.tree set {} leftname   [file tail $::dirdiff(leftDir)]
    $w.tree set {} rightfull  $::dirdiff(leftDir)
    $w.tree set {} rightname  [file tail $::dirdiff(leftDir)]

    InitIdleHandler $w.tree
    UpdateDirNode $w.tree {}
    bind $w.tree <<TreeviewOpen>> {UpdateDirNode %W [%W focus]}

    pack $w -fill both -expand 1
    grid $w.tree $w.vsb -sticky nsew
    grid $w.hsb         -sticky nsew
    grid columnconfigure $w 0 -weight 1
    grid rowconfigure    $w 0 -weight 1
}

makeWin


