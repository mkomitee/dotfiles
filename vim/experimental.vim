" upper/lower word
nmap <leader>u mQviwU`Q
nmap <leader>l mQviwu`Q

" upper/lower first char of word
nmap <leader>U mQgewvU`Q
nmap <leader>L mQgewvu`Q

" Remove F1 Help
inoremap <F1> <nop>
nnoremap <F1> <nop>
vnoremap <F1> <nop>

" Better command-line editing
cnoremap <C-j> <t_kd>
cnoremap <C-k> <t_ku>
cnoremap <C-a> <Home>
cnoremap <C-e> <End>

" Show git diff when commiting
autocmd FileType gitcommit DiffGitCached | wincmd p

" Easy window resizing
noremap <up> <C-W>+
noremap <down> <C-W>-
nnoremap <left> <C-W><
nnoremap <right> <C-W>>

" . returns to starting place
nnoremap . .`[

" Simple way to find out where the hell my cursor is
function WhereAmI()
    set invcursorline
    set invcursorcolumn
endfunction

nnoremap <leader>? :call WhereAmI()<cr>
