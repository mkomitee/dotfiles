setlocal equalprg=perltidy
setlocal formatprg=perltidy
setlocal omnifunc=PerlComplete
setlocal keywordprg="perldoc -f"
set iskeyword+=$
set iskeyword+=%
set iskeyword+=@
au BufWritePre,FileWritePre <buffer> silent! %:s/\s\+$//

if exists('+colorcolumn')
    set colorcolumn=80
else
   au BufWinEnter <buffer> let w:overlength=matchadd('ErrorMsg', '\%>80v.\+', -1)
endif
