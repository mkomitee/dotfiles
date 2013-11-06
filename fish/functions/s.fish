function s
    if test (count $argv) -gt 0
        sudo $argv
    else
        sudo -s
    end
end


