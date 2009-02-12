#----------------------------------------------------------------------
#  Eskil, Printing object
#
#  Copyright (c) 2006, Peter Spjuth  (peter.spjuth@gmail.com)
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

package require pdf4tcl
package require snit

snit::type eskilprint {
    component pdf
    delegate method * to pdf

    delegate option -margin to pdf
    delegate option -paper  to pdf

    option -cpl        -default 80
    option -cpln       -default 5
    option -headsize   -default 8
    option -headleft   -default "Header Text Left"
    option -headright  -default "Header Text Right"
    option -headnpages -default 10
    option -file       -default exp.pdf

    variable width
    variable height
    variable hoy
    variable fontsize
    variable linesize
    variable nlines
    variable ox1
    variable ox2
    variable oy
    variable page

    constructor {args} {
        set tmp(-file) $options(-file)
        catch {array set tmp $args}
        install pdf using pdf4tcl::pdf4tcl %AUTO% \
                -landscape 1 -paper a4 -margin 15mm -file $tmp(-file)
        $self configurelist $args
        $self StartPrint
    }
    destructor {
        catch {$pdf destroy}
    }

    method StartPrint {} {
        # Page size
        lassign [$pdf getDrawableArea] width height

        # Header metrics
        $pdf setFont $options(-headsize) Courier
        set headoffset [expr {$options(-headsize) + [$pdf getFontMetric bboxy]}]
        set hoy $headoffset

        # Figure out font size from number of chars per line
        set charwidthHead [$pdf getCharWidth "0"]
        set charwidth [expr {$width / 2.0 / ($options(-cpl) + $options(-cpln) + 1)}]
        set fontsize [expr {$options(-headsize) * $charwidth / $charwidthHead}]
        $pdf setFont $fontsize

        # Text metrics
        set linesize  $fontsize
        set offset    [expr {$fontsize + [$pdf getFontMetric bboxy]}]
        set charwidth [$pdf getCharWidth "0"]
        set nlinesf [expr {($height - $options(-headsize)) / $linesize}]
        # Number of lines per page
        set nlines  [expr {int($nlinesf - 1.0)}]
        #set nlines 66
        # Offsets to starting points in both subpages.
        set ox1 $charwidth
        set ox2 [expr {$width / 2.0 + $charwidth}]
        set oy  [expr {($nlinesf - $nlines) / 2.0 * $linesize + \
                                     $offset + $options(-headsize)}]

        # Reset current page
        set page 0
    }
    method getNLines {} {
        return $nlines
    }

    # Start a new page
    method newPage {} {
        $pdf startPage
        incr page

        # Draw borders
        $pdf setStrokeColor 0 0 0
        $pdf setFillColor 0.0 0.0 0.0
        $pdf setLineStyle 0.5
        # Outer border
        $pdf rectangle 0 $options(-headsize) \
                $width [- $height $options(-headsize)]
        # Center line
        $pdf line [/ $width 2.0] $options(-headsize) \
                [/ $width 2.0] $height

        # Header
        $pdf setFont $options(-headsize) Courier
        $pdf text $options(-headleft) -x 0 -y $hoy 
        $pdf text "Page $page of $options(-headnpages)" \
                -x [expr {$width / 2.0}] -y $hoy -align center
        $pdf text $options(-headright) -x $width -y $hoy -align right

        # Normal font
        $pdf setFont $fontsize Courier
    }

    method setHalf {half} {
        if {$half eq "left"} {
            $pdf setTextPosition $ox1 $oy
        } else {
            $pdf setTextPosition $ox2 $oy
        }
    }

    method setTag {tag fill} {
        variable tags
        set tags($tag) $fill
    }

    # Format a line of text/tag pairs
    method drawTextLine {line} {
        variable tags
        foreach {text tag} $line {
            if {$tag eq ""} {
                $self text $text
            } else {
                set fill $tags($tag)
                $self text $text -fill $fill
            }
        }
    }

    # Produce one page
    method onePage {} {

        $self newPage

        # Dummy strings
        set longstr  [string repeat "MifjgqIo" 100]
        set strcpl   [string range $longstr 0 [expr {$options(-cpl) - 1}]]
        set strcpl10 [string range $longstr 0 [expr {$options(-cpl) + 9}]]

        # Text
        for {set line 0} {$line < $nlines} {incr line} {
            if {$line % 2 == 0} {
                $pdf setBgColor 1.0 0.5 0.5
            } else {
                $pdf setBgColor 0.5 1.0 0.5
            }
            $pdf text $line -x $ox1 -y [expr {$oy + $line * $linesize}]
            $pdf text "hejsan" -fill 1
            $pdf text "hoppsan" -fill "0.5 0.5 1.0"
            $pdf text "Miffo"
            #$pdf drawTextAt $ox2 [expr {$oy + $line * $linesize}] "Hejsan" -fill 1
            #$pdf drawTextAt $ox2 [expr {$oy + $line * $linesize}] $strcpl -fill 1
            $pdf text $strcpl -x $ox2 -y [expr {$oy + $line * $linesize}] -fill 1
        }
    }

    # Finish a print job
    method endPrint {} {
        $pdf finish
        $pdf destroy
        $self destroy
    }
}
