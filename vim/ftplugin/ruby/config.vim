setlocal omnifunc=rubycomplete#Complete
setlocal keywordprg=ri
setlocal tabstop=4
setlocal shiftwidth=4
setlocal softtabstop=4

let g:rubycomplete_buffer_loading=1
let g:rubycomplete_classes_in_global=1

au BufWritePre,FileWritePre <buffer> call RemoveTrailingWhiteSpace()
