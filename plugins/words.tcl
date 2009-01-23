##Eskil Plugin : Compare set of words

# Example file for a plugin.
# A plugin must start exactly like this one.
# The text after : is the summary you can get at the command line

# This plugin compares the set of words in files.

# A plugin must define this procedure to do the job.
# side: left or right
# chi:  An input channel for reading the original file.
# cho:  An output channel for writing the processed file.
proc PreProcess {side chi cho} {
    while {[gets $chi line] >= 0} {
        foreach word [regexp -all -inline {\w+} $line] {
            # Plugins have access to preferences
            if {[info exists ::Pref(nocase)] && $::Pref(nocase)} {
                set word [string tolower $word]
            }
            set words($word) 1
        }
    }
    puts $cho [join [lsort -dictionary [array names words]] \n]
    # Signal that the file after processing should be used both
    # for comparison and for displaying.
    return 1
}
