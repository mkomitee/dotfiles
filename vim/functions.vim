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
