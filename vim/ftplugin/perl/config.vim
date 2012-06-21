setlocal equalprg=perltidy
setlocal formatprg=perltidy
setlocal omnifunc=PerlComplete
setlocal keywordprg="perldoc -f"
set iskeyword+=$
set iskeyword+=%
set iskeyword+=@
au BufWritePre,FileWritePre <buffer> call RemoveTrailingWhiteSpace()
