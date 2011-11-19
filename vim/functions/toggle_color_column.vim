
function! ToggleColorColumn(toggle)
    if exists('+colorcolumn')
        if a:toggle == 'on'
            setlocal colorcolumn=+1,+2,+3,+4,+5,+6,+7,+8,+9,+10
        else
            setlocal colorcolumn=
        endif
    else
    endif
endfunction
