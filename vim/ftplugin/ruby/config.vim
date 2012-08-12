setlocal omnifunc=rubycomplete#Complete
setlocal keywordprg=ri
setlocal tabstop=2
setlocal shiftwidth=2
setlocal softtabstop=2

let g:rubycomplete_buffer_loading=1
let g:rubycomplete_classes_in_global=1

au BufWritePre,FileWritePre <buffer> call RemoveTrailingWhiteSpace()

if exists('+colorcolumn')
    set colorcolumn=+1
endif
