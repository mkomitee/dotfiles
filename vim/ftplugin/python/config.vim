call ToggleColorColumn('on')

setlocal textwidth=100
setlocal list
setlocal omnifunc=pythoncomplete#Complete
setlocal equalprg=pytidy
setlocal formatprg=pytidy
setlocal tabstop=4
setlocal softtabstop=4
setlocal shiftwidth=4
setlocal smarttab
setlocal expandtab
setlocal smartindent
setlocal keywordprg=pydoc
SyntasticEnable python
let makeprg = 'pyflakes %'
