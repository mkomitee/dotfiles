# #!/usr/bin/env zsh

if [ "$TERM" = "dumb" ]; then
    return
fi

MKPROMPT_CUTOFF=3
MKPROMPT_KEYMAP=
MKPROMPT_DN=`uname -n | cut -d. -f2`
MKPROMPT_LDNS=()

autoload -Uz promptinit
promptinit
autoload colors;
colors;
autoload -Uz add-zsh-hook

setopt prompt_subst
setopt transient_rprompt

typeset -AHg FX FG BG

FX=(
    reset     "%{[00m%}"
    bold      "%{[01m%}" no-bold      "%{[22m%}"
    italic    "%{[03m%}" no-italic    "%{[23m%}"
    underline "%{[04m%}" no-underline "%{[24m%}"
    blink     "%{[05m%}" no-blink     "%{[25m%}"
    reverse   "%{[07m%}" no-reverse   "%{[27m%}"
)

for color in {000..255}; do
    FG[$color]="%{[38;5;${color}m%}"
    BG[$color]="%{[48;5;${color}m%}"
done

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
    echo "%(!,%{$FG[124]%},%{$FG[027]%})=%!= %{$reset_color%}"
}

function mkprompt_host() {
    if [[ ! -z "$PRIMARY_HOST" && ! -z "$HOSTNAME" && $PRIMARY_HOST = $HOSTNAME ]]; then
    elif [[ ${MKPROMPT_LDNS[(i)$MKPROMPT_DN]} -le ${#MKPROMPT_LDNS} ]]; then
        echo "%(!.%{$FG[124]%}.%{$FG[027]%})%m%{$reset_color%} "
    else
        echo "%(!.%{$FG[124]%}.%{$FG[027]%})%2m%{$reset_color%} "
    fi
}

function mkprompt_prompt() {
    if [ "$MKPROMPT_KEYMAP" = 'vicmd' ]; then
        echo "%(!,%{$BG[124]%},%{$BG[027]%})%%%{$reset_color%}"
    else
        echo "%(!,%{$FG[124]%},%{$FG[027]%})%%%{$reset_color%}"
    fi
}

function mkprompt_jobs() {
    echo "%(1j,%{$FG[027]%}(%{$FG[008]%}%j%{$FG[027]%}%)%{$reset_color%} ,)"
}

function mkprompt_exit() {
    echo "%(?,,%{$FG[124]%}[%{$FG[008]%}%?%{$FG[124]%}]%{$reset_color%} )"
}

function mkprompt_cwd() {
    echo "%(!,%{$FG[124]%},%{$FG[027]%})%25<..<%~%{$reset_color%}%<< "
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
            echo "%{$FG[124]%}[${clock}]%{$reset_color%} "
        fi
    fi
    MKPROMPT_TIMER=0
}

function mkprompt_branch() {
    # Defines path as current directory
    local current_dir=$PWD
    # While current path is not root path
    while [[ $current_dir != '/' ]]; do
        # Git repository
        if [[ -d "${current_dir}/.git" ]]; then
            echo "%{$FG[027]%}±" $(git_remote)@${"$(<"$current_dir/.git/HEAD")"##*/}%{$reset_color%}
            return;
        fi
        # Mercurial repository
        if [[ -d "${current_dir}/.hg" ]]; then
            echo "%{$FG[027]%}☿" $(<"$current_dir/.hg/branch")%{$reset_color%}
            return;
        fi
        # Defines path as parent directory and keeps looking for :)
        current_dir="${current_dir:h}"
    done
}

function mkprompt_extras() {
    if [[ ! -z "$MKPROMPT_EXTRAS" ]]; then
        echo "%{$FG[027]%}(%{$FG[008]%}$MKPROMPT_EXTRAS%{$FG[027]%})%{$reset_color%} "
    fi
}
#008
function mkprompt_setup() {
    PROMPT=""
    PROMPT="${PROMPT}$(mkprompt_exit)"
    PROMPT="${PROMPT}$(mkprompt_jobs)"
    # PROMPT="${PROMPT}$(mkprompt_history)"
    PROMPT="${PROMPT}$(mkprompt_extras)"
    PROMPT="${PROMPT}$(mkprompt_host)"
    PROMPT="${PROMPT}$(mkprompt_cwd)"
    PROMPT="${PROMPT}$(mkprompt_prompt) "
    # RPROMPT=""
    # RPROMPT="${RPROMPT}$(mkprompt_elapsed)"
    # RPROMPT="${RPROMPT}$(mkprompt_branch)"
}

function zle-line-init zle-keymap-select {
    MKPROMPT_KEYMAP=${KEYMAP}
    mkprompt_setup
    zle reset-prompt
}

zle -N zle-line-init
zle -N zle-keymap-select

add-zsh-hook precmd mkprompt_setup
add-zsh-hook preexec mkprompt_timer

TRAPWINCH() {
    zle && { zle reset-prompt; zle -R }
}
