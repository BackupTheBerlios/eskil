##Eskil Plugin

# Example file for a plugin.
# A plugin must start with the exact first line as this one.

# This plugin implements case insensitive matching, corresponding to the
# -nocase flag.

# A plugin must define this procedure to do the job.
# side: left or right
# chi:  An input channel for reading the original file.
# cho:  An output channel for writing the processed file.
proc PreProcess {side chi cho} {
    while {1} {
        # Read data in large chunks for speed
        set data [read $chi 100000]
        if {$data eq ""} break
        # Use lower case for comparison, thus getting case insensitive
        puts -nonewline $cho [string tolower $data]
    }
    # Signal that the file after processing should be used only for
    # comparison, not for displaying.
    # The processed file must match the original line-wise.
    return 0
}
