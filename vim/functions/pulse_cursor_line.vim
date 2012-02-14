function! PulseCursorLine()
    let current_window = winnr()

    windo set nocursorline
    execute current_window . 'wincmd w'

    setlocal cursorline

    redir => old_hi
        silent execute 'hi CursorLine'
    redir END
    let old_hi = split(old_hi, '\n')[0]
    let old_hi = substitute(old_hi, 'xxx', '', '')

    hi CursorLine guibg=#2a2a2a ctermbg=233
    redraw
    sleep 10m

    hi CursorLine guibg=#333333 ctermbg=235
    redraw
    sleep 10m

    hi CursorLine guibg=#3a3a3a ctermbg=237
    redraw
    sleep 10m

    hi CursorLine guibg=#444444 ctermbg=239
    redraw
    sleep 10m

    hi CursorLine guibg=#3a3a3a ctermbg=237
    redraw
    sleep 10m

    hi CursorLine guibg=#333333 ctermbg=235
    redraw
    sleep 10m

    hi CursorLine guibg=#2a2a2a ctermbg=233
    redraw
    sleep 10m

    execute 'hi ' . old_hi

    windo set cursorline
    execute current_window . 'wincmd w'
endfunction
