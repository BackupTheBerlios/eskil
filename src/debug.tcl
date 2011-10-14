# debug.tcl
#
#    Helpers for debugging.
#
#
proc testme {x y z} {
    puts "In Testme $x $y $z"
    list [lindex $x 0] [lindex $y 0]
}

proc debugTRenter {cmd op} {
    set fr [info frame -2]
    puts "Line [dict get $fr line] Enter: '$cmd'"
}
proc debugTRenterstep {cmd op} {
    set fr [info frame -2]
    #puts "$fr"
    puts "Line [dict get $fr line] Enterstep: '$cmd'"
}
proc debugTRleave {cmd core res op} {
    puts "Leave: '$res'"
}
proc debugTRleavestep {cmd code res op} {
    puts "Leavestep: '$res'"
}

proc debugTR {cmd} {
    trace add execution $cmd enter debugTRenter
    trace add execution $cmd leave debugTRleave
    trace add execution $cmd enterstep debugTRenterstep
    trace add execution $cmd leavestep debugTRleavestep
}

proc DebugMenu {m} {
    $m add cascade -label "Debug" -menu $m.debug -underline 0
    menu $m.debug

    if {$::tcl_platform(platform) eq "windows"} {
        $m.debug add checkbutton -label "Console" -variable consolestate \
                -onvalue show -offvalue hide -command {console $consolestate} \
                -underline 0
        $m.debug add separator
    }

    $m.debug add command -label "Edit" -command ProcEditor -underline 0
    return $m.debug
}

proc ProcEditorUpdate {a k} {
    # Only update on keys generating characters
    if {$a eq ""} return
    #puts "Key '$a' '$k'"
    set p [info procs $::ProcEditor(proc)]
    if {$p eq "" && $::ProcEditor(proc) ne ""} {
        # Try prefix matching
        set p [info procs $::ProcEditor(proc)*]
    }
    set p [lsort -dictionary $p]
    $::ProcEditor(procW) configure -values $p
    # Keep the first
    set p [lindex $p 0]

    if {$p eq ""} {
        set ::ProcEditor(args) ""
        $::ProcEditor(bodyW) delete 1.0 end
        return
    }
    if {$p ne $::ProcEditor(proc)} {
        after idle [list set ::ProcEditor(proc) $p]
        after idle [list $::ProcEditor(procW) selection range insert end]
    }

    after idle ProcEditorSelected
}

proc ProcEditorSelected {} {
    set p [info procs $::ProcEditor(proc)]
    if {$p eq ""} {
        set ::ProcEditor(args) ""
        $::ProcEditor(bodyW) delete 1.0 end
        return
    }
    set arglist {}
    foreach i [info args $p] {
        if {[info default $p $i value]} {
            lappend arglist [list $i $value]
        } else {
            lappend arglist [list $i]
        }
    }
    set body [info body $p]

    set ::ProcEditor(args) $arglist
    $::ProcEditor(bodyW) delete 1.0 end
    $::ProcEditor(bodyW) insert end $body
}

proc ProcEditorRedefine {} {
    set body [$::ProcEditor(bodyW) get 1.0 end]
    set body [string trimright $body]

    ##nagelfar ignore Non constant argument to proc
    proc $::ProcEditor(proc) $::ProcEditor(args) $body
}

proc ProcEditor {} {
    set top .proceditor
    destroy $top
    toplevel $top -padx 3 -pady 3
    ttk::frame $top.bgf
    place $top.bgf -x 0 -y 0 -relwidth 1.0 -relheight 1.0 -anchor nw
    wm title $top "Proc Editor"

    ttk::label $top.l1 -text "Proc" -anchor w
    ttk::combobox $top.e1 -textvariable ::ProcEditor(proc) -values ""
    set ::ProcEditor(procW) $top.e1
    bind $top.e1 <KeyRelease> {ProcEditorUpdate %A %K}
    bind $top.e1 <<ComboboxSelected>> ProcEditorSelected
    #trace add variable ::ProcEditor(proc) write "ProcEditorUpdate"
    ttk::label $top.l2 -text "Args" -anchor w
    ttk::entry $top.e2 -textvariable ::ProcEditor(args)
    set ::ProcEditor(bodyW) [text $top.t -yscrollcommand "$top.sby set"]
    ttk::scrollbar $top.sby -orient vertical -command "$top.t yview"

    ttk::button $top.b -text "Redefine" -command ProcEditorRedefine

    grid $top.l1 $top.e1 -        -padx 3 -pady 3 -sticky we
    grid $top.l2 $top.e2 -        -padx 3 -pady 3 -sticky we
    grid $top.t  -       $top.sby -padx 3 -pady 3 -sticky news
    grid $top.b  -       -        -padx 3 -pady 3

    grid columnconfigure $top 1 -weight 1
    grid rowconfigure    $top 2 -weight 1
}
