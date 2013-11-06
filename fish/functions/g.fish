function g
    if test (count $argv) -gt 0
        git $argv
    else
        git status --short --branch
    end
end
