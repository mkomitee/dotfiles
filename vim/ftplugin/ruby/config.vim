call ToggleColorColumn('on')

setlocal textwidth=100
setlocal omnifunc=rubycomplete#Complete
setlocal list
setlocal keywordprg=ri
setlocal tabstop=2
setlocal shiftwidth=2
setlocal softtabstop=2

let g:rubycomplete_buffer_loading=1
let g:rubycomplete_classes_in_global=1
