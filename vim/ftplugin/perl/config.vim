setlocal equalprg=perltidy
setlocal formatprg=perltidy
setlocal omnifunc=PerlComplete
setlocal keywordprg="perldoc -f"
set iskeyword+=$
set iskeyword+=%
set iskeyword+=@
setlocal tabstop=4
setlocal shiftwidth=4
setlocal softtabstop=4
au BufWritePre,FileWritePre <buffer> call RemoveTrailingWhiteSpace()

if exists('+colorcolumn')
    set colorcolumn=+1
endif
