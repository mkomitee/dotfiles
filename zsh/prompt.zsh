# #!/usr/bin/env zsh

MKPROMPT_CUTOFF=3
MKPROMPT_KEYMAP=

autoload -Uz promptinit
promptinit
autoload colors;
colors;
autoload -Uz add-zsh-hook

setopt prompt_subst
setopt transient_rprompt

# %! %h          - Current history event number
# %#             - # if superuser, else %
# %%             - a literal %
# %)             - use with %X(.tstr.fstr)
# %*             - Time in 24-hour format with seconds
# %/ %d          - $PWD
# %c %. %C       - Deprecated alternatives, differ by default n
# %?             - Return status of last command
# %@ %t          - Time of day in am/pm format
# %B (%b)        - Start (stop) bold face mode
# %D %D{str}     - Date as YY-MM-DD, optional strftime spec
# %E             - Clear to end of line
# %i             - Script/function line number ($LINENO)
# %j             - Number of jobs listed by jobs
# %L             - Shell depth ($SHLVL)
# %l             - Login terminal without /dev or /dev/tty
# %M             - Full host name
# %m             - Host name to first dot or n dots
# %N             - Name of script, function, sourced file
# %n             - Name of user ($USERNAME)
# %S %s          - Start (stop) standout mode
# %T             - Time of dsay, 24-hour format
# %U %u          - Start (stop) underline mode
# %v             - nth component of $psvar array
# %W             - Date as middle-endian MM/DD/YY
# %w             - Date as DAY DD
# %y             - Login terminal without /dev
# %_             - Parser state (continuation lines, debug)
# %~             - Like %/, %d but with tilde substitution
# %{esc%}        - Escape sequence (esc doesn't move cursor)
# %X(.tstr.fstr) - tstr if test X gives n, else fstr
# %<str<         - Truncate to n on left, str on left if so
# %>str>         - Truncate to n on right, str on right if so

function mkprompt_history() {
    echo "%(!,%{$fg[red]%},%{$fg[yellow]%})=%!= %{$reset_color%}"
}

function mkprompt_host() {
    if [ "$MKPROMPT_KEYMAP" = 'vicmd' ]; then
        echo "%{$fg[blue]%}%m%{$reset_color%}"
    else
        echo "%(!.%{$fg[red]%}.%{$fg[yellow]%})%m%{$reset_color%}"
    fi
}

function mkprompt_prompt() {
    echo "%(!,%{$fg[red]%},%{$fg[yellow]%})%#%{$reset_color%}"
}

function mkprompt_jobs() {
    echo "%(1j,%{$fg[yellow]%}(%j%)%{$reset_color%} ,)"
}

function mkprompt_exit() {
    echo "%(?,,%{$fg[red]%}[%?]%{$reset_color%} )"
}

function mkprompt_cwd() {
    echo "%(!,%{$fg[red]%},%{$fg[yellow]%})%3~%{$reset_color%}"
}

function mkprompt_timer() {
    MKPROMPT_TIMER=${SECONDS}
}

function mkprompt_elapsed() {
    if [[ ${MKPROMPT_TIMER} -ne 0 ]]; then
        local elapsed=$(($SECONDS - $MKPROMPT_TIMER))
        if [[ $elapsed -ge  ${MKPROMPT_CUTOFF} ]]; then
            local hours=$((${elapsed}/3600))
            local remainder=$((${elapsed}%3600))
            local minutes=$((${remainder}/60))
            local seconds=$((${remainder}%60))
            local clock=$(printf '%d:%02d:%02d' $hours $minutes $seconds)
            echo "%{$fg[red]%}[${clock}]%{$reset_color%}"
        fi
    fi
    MKPROMPT_TIMER=0
}

function mkprompt_setup() {
    PROMPT=""
    PROMPT="${PROMPT}$(mkprompt_exit)"
    PROMPT="${PROMPT}$(mkprompt_history)"
    PROMPT="${PROMPT}$(mkprompt_host) "
    PROMPT="${PROMPT}$(mkprompt_cwd) "
    PROMPT="${PROMPT}$(mkprompt_jobs)"
    PROMPT="${PROMPT}$(mkprompt_prompt) "
    RPROMPT=""
    RPROMPT="${RPROMPT}$(mkprompt_elapsed)"
}

function zle-line-init zle-keymap-select {
    MKPROMPT_KEYMAP=${KEYMAP}
    mkprompt_setup
    zle reset-prompt
}

add-zsh-hook precmd mkprompt_setup
add-zsh-hook preexec mkprompt_timer
