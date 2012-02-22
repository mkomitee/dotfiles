setlocal textwidth=80
nnoremap <localleader>l :PyLint<cr>
let w:overlength=matchadd('ErrorMsg', '\%>80v.\+', -1)
nnoremap <buffer> <Leader>p :call Pyflakes()<CR>
au BufWritePre,FileWritePre <buffer> silent! %:s/\s\+$//
