function psgrep
    ps auxww | grep --color=always $argv | grep -v grep
end
