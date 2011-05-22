# I hope uname and sed are in the path, ...
function shorthost() {
    uname -n | sed 's/\([^\.]*\.[^\.]*\)\..*/\1/'
}

