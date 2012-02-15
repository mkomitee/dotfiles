setlocal textwidth=80
nnoremap <localleader>l :PyLint<cr>
let w:overlength=matchadd('ErrorMsg', '\%>80v.\+', -1)
