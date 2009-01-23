##Eskil Plugin : Compare with backslash-newline removed

# Example file for a plugin.
# A plugin must start exactly like this one.
# The text after : is the summary you can get at the command line

# This plugin replaces any backslash-newline with space, thus
# ignoring restructured lines.

# A plugin must define this procedure to do the job.
# side: left or right
# chi:  An input channel for reading the original file.
# cho:  An output channel for writing the processed file.
proc PreProcess {side chi cho} {
    set trim 0
    while {[gets $chi line] >= 0} {
        if {$trim} {
            set line [string trimleft $line]
            set trim 0
        }
        if {[string index $line end] eq "\\"} {
            puts -nonewline $cho [string range $line 0 end-1]
            puts -nonewline $cho " "
            set trim 1
        } else {
            puts $cho $line
        }
    }
    # Signal that the file after processing should be used both
    # for comparison and for displaying.
    return 1
}
