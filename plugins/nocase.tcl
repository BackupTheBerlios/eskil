##Eskil Plugin

# Example file for a plugin.
# Correspondning to -nocase flag.

proc PreProcess {side chi cho} {
    while {1} {
        set data [read $chi 100000]
        if {$data eq ""} break
        puts -nonewline $cho [string tolower $data]
    }
    return 0
}
