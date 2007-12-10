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
        install tree using ttk::treeview $win.tree \
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

        $tree column leftsize  -stretch 0 -width 70
        $tree column rightsize -stretch 0 -width 70
        $tree column leftdate  -stretch 0 -width 70
        $tree column rightdate -stretch 0 -width 70

        # Fill in root data
        $tree set {} type       directory
        $tree set {} status     empty
        $tree set {} leftfull   $options(-leftdir)
        $tree set {} leftname   [file tail $options(-leftdir)]
        $tree set {} rightfull  $options(-rightdir)
        $tree set {} rightname  [file tail $options(-rightdir)]

        $self UpdateDirNode {}
        bind $tree <<TreeviewOpen>> "[mymethod UpdateDirNode] \[%W focus\]"

        grid $tree $vsb -sticky nsew
        grid $hsb         -sticky nsew
        grid columnconfigure $win 0 -weight 1
        grid rowconfigure    $win 0 -weight 1
    }
    destructor {

    }

    method AddNodeToIdle {node} {
        if {[info exists IdleQueueArr($node)]} { return }
        lappend IdleQueue $node
        set IdleQueueArr($node) 1

        if {$AfterId eq ""} {
            set AfterId [after idle [mymethod UpdateIdle]]
        }
    }
    method UpdateIdle {} {
        set AfterId ""

        if {[llength $IdleQueue] > 0} {
            set node [lindex $IdleQueue 0]
            set IdleQueue [lrange $IdleQueue 1 end]
            unset IdleQueueArr($node)

            $self UpdateDirNode $node
        }

        if {[llength $IdleQueue] > 0} {
            set AfterId [after idle [mymethod UpdateIdle]]
        }
    }

    method UpdateDirNode {node} {
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
                $self AddNodeToIdle $id
            }
        }

        $tree set $node status unknown
        $tree set $node leftfull
        $tree set $node leftname
        $tree set $node rightfull
        $tree set $node rightname
    }
}

proc makeWin {} {

    set w [DirCompare .f -leftdir $::dirdiff(leftDir) -rightdir $::dirdiff(rightDir)]
    pack $w -fill both -expand 1
}

makeWin


