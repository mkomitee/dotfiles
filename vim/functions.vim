function! WhatGroup()
    if !exists("*synstack")
        return
    endif
    echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc
command! WhatGroup :call WhatGroup()


function! RemoveTrailingWhiteSpace()
    silent! normal! mw
    silent! %:s/\s\+$//
    silent! normal! `w
    silent! delm w
endfun
command! RemoveTrailingWhiteSpace :call RemoveTrailingWhiteSpace()

if has('python')
    let s:uname = system("echo -n \"$(uname)\"")
    if s:uname ==# 'SunOS'
        function! Pomodoro()
            return ''
        endfunction
    else
        py << eopython
try:
    import os.path
    import sys
    import vim
    sys.path.insert(0, os.path.join(os.environ['HOME'], 'scripts'))
    import pomodoro
except Exception:
    pass
eopython

        function! Pomodoro()
        py << eopython
try:
    history = pomodoro.History.deserialize()
    last = history.last
    if last:
        vim.command("return '%s'" % last)
    else:
        vim.command("return '--:--'")
except Exception:
    vim.command("return '??:??'")
eopython
        endfunction

        function! PomodoroStart()
        py << eopython
try:
    pomodoro.start()
except Exception:
    pass
eopython
        endfunction
        command! PomodoroStart :call PomodoroStart()

        function! PomodoroCancel()
        py << eopython
try:
    pomodoro.cancel()
except Exception:
    pass
eopython
        endfunction
        command! PomodoroCancel :call PomodoroCancel()
    endif
endif
