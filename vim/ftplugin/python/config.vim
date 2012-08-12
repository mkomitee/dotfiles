setlocal tabstop=4
setlocal shiftwidth=4
setlocal softtabstop=4
setlocal keywordprg=pydoc
au BufWritePre,FileWritePre <buffer> call RemoveTrailingWhiteSpace()
set omnifunc=pythoncomplete#Complete

" Add the virtualenv's site-packages to vim path
py << EOF
import os
import sys
import vim
if os.environ.get('VIRTUAL_ENV', None):
    project_base_dir = os.environ['VIRTUAL_ENV']
    sys.path.insert(0, project_base_dir)
    activate_this = os.path.join(project_base_dir, 'bin/activate_this.py')
    execfile(activate_this, dict(__file__=activate_this))
EOF

if exists('+colorcolumn')
    set colorcolumn=+1
endif
