setlocal tabstop=4
setlocal shiftwidth=4
setlocal softtabstop=4
setlocal keywordprg=pydoc
au BufWritePre,FileWritePre <buffer> call RemoveTrailingWhiteSpace()
