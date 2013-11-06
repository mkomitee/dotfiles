function resume
    if test -z $TMUX
        if test (count $argv) -eq 0
            set session main
        else
            set session $argv[1]
        end
        tmux -u2 attach-session -t $session
        if test $status -ne 0
            tmux -u2 new-session -s $session
        end
    else
        echo "Don't nest TMUX sessions"
    end
end
