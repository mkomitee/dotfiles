" $HOME/.config/oni/config.js
" $HOME/.config/oni/config.tsx
" $HOME/.config/oni/tsconfig.json

set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath
source ~/.vim/vimrc

" Recommended from the oni minimal config
if exists("g:gui_oni")
  set mouse=a
  set noshowcmd
  set noshowmode
  nnoremap <leader>ff :call OniCommand("quickOpen.show")<CR>
  nnoremap <leader>pf :call OniCommand("quickOpen.show")<CR>
  nnoremap <leader>pg :call OniCommand("quickOpen.show")<CR>
  nnoremap <leader>bb :call OniCommand("buffer.open")<CR>
endif


" Allow C-w to function in neovim terminals as it does in vim.
tnoremap <C-w> <C-\><C-n><C-w>
tnoremap <C-w>n <C-\><C-n>

autocmd TermOpen *
      \ if &buftype ==# 'terminal' |
      \ startinsert |
      \ endif

source ~/.config/nvim/nvim.local
