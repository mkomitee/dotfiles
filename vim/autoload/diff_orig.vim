
function! DiffOrig()
    if !exists("b:diff_active") && &buftype == "nofile"
        echoerr "E: Cannot diff a scratch buffer"
        return -1
    elseif expand("%") == ""
        echoerr "E: Buffer doesn't exist on disk"
        return -1
    endif

    if !exists("b:diff_active") || b:diff_active == 0
        let b:diff_active = 1
        let l:orig_filetype = &l:filetype

        leftabove vnew
        set buftype=nofile
        read #
        0delete_
        let &l:filetype = l:orig_filetype
        diffthis
        wincmd p
        diffthis
    else
        diffoff
        wincmd p
        bdelete
        let b:diff_active = 0
    endif
endfunction

