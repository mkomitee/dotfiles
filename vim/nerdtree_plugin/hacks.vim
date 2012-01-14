let g:NERDTreeCustomReuseWindows = '1'

call NERDTreeAddKeyMap({
       \ 'key': 'w',
       \ 'scope': 'all',
       \ 'callback': 'NERDTreeCustomToggleReuse',
       \ 'quickhelpText': 'Toggle use existing windows' })

function! NERDTreeCustomToggleReuse()
    let g:NERDTreeCustomReuseWindows = g:NERDTreeCustomReuseWindows ? 0 : 1
    echomsg (g:NERDTreeCustomReuseWindows ? 'Reusing' : 'Not reusing') . ' existing windows'
endfunction

call NERDTreeAddKeyMap({
       \ 'key': 'i',
       \ 'scope': 'FileNode',
       \ 'callback': 'NERDTreeCustomOpenSplit',
       \ 'quickhelpText': 'open split reusing if able' })

function! NERDTreeCustomOpenSplit(node)
    call a:node.open({'where': 'h', 'reuse': g:NERDTreeCustomReuseWindows})
endfunction

call NERDTreeAddKeyMap({
       \ 'key': 's',
       \ 'scope': 'FileNode',
       \ 'callback': 'NERDTreeCustomOpenVSplit',
       \ 'quickhelpText': 'open vsplit reusing if able' })

function! NERDTreeCustomOpenVSplit(node)
    call a:node.open({'where': 'v', 'reuse': g:NERDTreeCustomReuseWindows})
endfunction

call NERDTreeAddKeyMap({
       \ 'key': 't',
       \ 'scope': 'FileNode',
       \ 'callback': 'NERDTreeCustomOpenInTab',
       \ 'quickhelpText': 'open in new tab reusing if able' })

function! NERDTreeCustomOpenInTab(node)
    call a:node.open({'where': 't', 'reuse': g:NERDTreeCustomReuseWindows})
endfunction

call NERDTreeAddKeyMap({
       \ 'key': 'T',
       \ 'scope': 'FileNode',
       \ 'callback': 'NERDTreeCustomOpenInTabSilent',
       \ 'quickhelpText': 'open in new background tab reusing if able' })

function! NERDTreeCustomOpenInTabSilent(node)
    call a:node.open({'where': 't', 'stay': 1, 'reuse': g:NERDTreeCustomReuseWindows})
endfunction

