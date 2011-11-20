
function! ToggleMyScratch()
    let file_name     = '~/.scratch'
    let buffer_number = bufnr(file_name)
    let window_number = bufwinnr(buffer_number)
    if buffer_number == -1
        exe 'split' . file_name
    else
        if window_number != -1
            if winnr() != window_number
                exe window_number . "wincmd w"
            else
                write
                quit
            endif
        else
            exe 'split' . file_name
        endif
    endif
endfunction
