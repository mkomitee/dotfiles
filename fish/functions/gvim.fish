function gvim
    set -l server GVIM
    if test (count $argv) -gt 0
        command gvim --servername $server --remote-silent $argv; or command gvim $argv
    else
        command gvim --servername $server; or gvim $argv
    end
end
