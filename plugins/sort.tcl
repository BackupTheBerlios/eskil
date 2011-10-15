##Eskil Plugin : Compare files after sorting lines

# Example file for a plugin.
# A plugin must start exactly like this one.
# The text after : is the summary you can get at the command line

# This plugin compares the set of words in files.

# A plugin must define this procedure to do the job.
# side: left or right
# chi:  An input channel for reading the original file.
# cho:  An output channel for writing the processed file.
proc PreProcess {side chi cho} {
    set data [read $chi]
    set endingNewLine 0
    if {[string index $data end] eq "\n"} {
        set data [string range $data 0 end-1]
        set endingNewLine 1
    }
    set lines [split $data \n]
    # Allow sort parameters in info
    set lines [lsort -dictionary {*}$::Info $lines]
    puts -nonewline $cho [join $lines \n]
    if {$endingNewLine} {
        puts $cho ""
    }
    # Signal that the file after processing should be used both
    # for comparison and for displaying.
    return 1
}
