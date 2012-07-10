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

function! Pomodoro()
py << eopython
import os.path
import sys
import vim
sys.path.insert(0, os.path.join(os.environ['HOME'], 'scripts'))
from pomodoro import *
history = History.deserialize()
last = history.last
if last:
    vim.command("return '%s'" % last)
else:
    vim.command("return '--:--'")
eopython
endfunction
