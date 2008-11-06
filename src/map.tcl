#----------------------------------------------------------------------
#  Eskil, Map
#
#  Copyright (c) 1998-2005, Peter Spjuth  (peter.spjuth@gmail.com)
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

proc createMap {top} {
    global Pref

    set w $top.c_map
    if {$Pref(wideMap)} {
        set width 20
    } else {
        set width 6
    }
    canvas $w -width $width -borderwidth 0 -selectborderwidth 0 \
            -highlightthickness 0 -height 10
    set map [image create photo map$top]

    $w create image 0 0 -anchor nw -image $map
    bind $w <Destroy>   [list image delete $map]
    bind $w <Configure> [list drawMap $top %h]
    bind $w <Button-2>  [list ThumbMap $top %y]

    return $w
}

proc clearMap {top} {
    set ::diff($top,changes) {}
    set ::diff($top,mapMax) 0
    drawMap $top -1
}

proc addChange {top n tag line1 n1 line2 n2} {
    if {$tag ne ""} {
        lappend ::diff($top,changes) [list $::diff($top,mapMax) $n \
                $tag $line1 $n1 $line2 $n2]
    }
    incr ::diff($top,mapMax) $n
}

proc addMapLines {top n} {
    incr ::diff($top,mapMax) $n
}

proc drawMap {top newh} {
    global Pref

    set oldh [map$top cget -height]
    if {$oldh == $newh} return

    map$top blank
    if {![info exists ::diff($top,changes)] || \
	    [llength $::diff($top,changes)] == 0} return

    set w [winfo width $top.c_map]
    set h [winfo height $top.c_map]
    set x2 [expr {$w - ($Pref(wideMap) ? 5 : 1)}]
    if {$x2 < 0} { set x2 0 }
    map$top configure -width $w -height $h
    incr h -1
    set y0 0
    foreach change $::diff($top,changes) {
	foreach {start length type dum1 dum2 dum3 dum4} $change break
	set y1 [expr {$start * $h / $::diff($top,mapMax) + 1}]
	if {!$y0} { set y0 $y1 } ;# Record first occurance
	if {$y1 < 1} {set y1 1}
	if {$y1 > $h} {set y1 $h}
	set y2 [expr {($start + $length) * $h / $::diff($top,mapMax) + 1}]
	if {$y2 < 1} {set y2 1}
	if {$y2 <= $y1} {set y2 [expr {$y1 + 1}]}
	if {$y2 > $h} {set y2 $h}
	incr y2
	map$top put $Pref(color$type) -to 1 $y1 $x2 $y2
    }
    if {$Pref(wideMap)} {
        map$top put black -to $x2 $y0 $w $y2
    }
}

# Allow button 2 on map to jump to a position
proc ThumbMap  {top y} {
    incr y 15
    ::tk::ScrollButton2Down $top.sby 0 $y
}
