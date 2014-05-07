function fish_prompt --description 'Write out the prompt'

    # Cache this for later
    set last_status $status

    z --add "$PWD"

    # Yep, this shouldn't be necessary, but I'm at a loss.
    if not set -q installed_user_key_bindings
        fish_user_key_bindings
    end

    # Cache these so we don't need to execute set_color over and over
    if not set -q __fish_color_normal
        set -g __fish_color_normal (set_color normal)
    end
    if not set -q __fish_color_red
        set -g __fish_color_red (set_color red)
    end
    if not set -q __fish_color_blue
        set -g __fish_color_blue (set_color blue)
    end
    if not set -q __fish_color_cyan
        set -g __fish_color_cyan (set_color cyan)
    end
    if not set -q __fish_color_yellow
        set -g __fish_color_yellow (set_color yellow)
    end

    # Give an indication if the previous command exited abnormally
    if test $last_status -ne 0
        set __fish_status [$last_status]
        set __fish_prompt_status "$__fish_color_red$__fish_status$__fish_color_normal "
    else
        set __fish_prompt_status ""
    end

    # Cache this so we don't need to recheck the current hostname whenever we
    # want to display a prompt.
    if not set -q __fish_hostname
        set -g __fish_hostname (hostname | cut -d . -f 1,2)
    end
    if test $USER = root
        set __fish_color_host $__fish_color_red
    else if test $fish_bind_mode = default
        set __fish_color_host $__fish_color_blue
    else if test $fish_bind_mode = visual
        set __fish_color_host $__fish_color_cyan
    else
        set __fish_color_host $__fish_color_yellow
    end
    set __fish_prompt_host "$__fish_color_host$__fish_hostname$__fish_color_normal "

    # Include the cwd
    set __fish_color_cwd $__fish_color_yellow
    set __fish_cwd (prompt_pwd)
    set __fish_prompt_cwd "$__fish_color_cwd$__fish_cwd$__fish_color_normal "

    # Give an indication if we have jobs running in the background or suspended
    set __fish_jobs_count (jobs | wc -l)
    if test $__fish_jobs_count -ne 0
        set __fish_color_jobs $__fish_color_red
        set __fish_jobs "($__fish_jobs_count)"
        set __fish_prompt_jobs "$__fish_color_jobs$__fish_jobs$__fish_color_normal "
    else
        set __fish_prompt_jobs ""
    end

    # Another indication of whether I'm root or not
    if test $USER = root
        set __fish_delimiter '#'
        set __fish_color_delimiter $__fish_color_red
    else
        set __fish_delimiter '%'
        set __fish_color_delimiter $__fish_color_yellow
    end
    set __fish_prompt_delimiter "$__fish_color_delimiter$__fish_delimiter$__fish_color_normal "

    # Put it all together
    echo -n -s $__fish_prompt_status \
               $__fish_prompt_host \
               $__fish_prompt_cwd \
               $__fish_prompt_jobs \
               $__fish_prompt_delimiter
end
