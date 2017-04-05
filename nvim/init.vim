"dein Scripts-----------------------------
if &compatible
    set nocompatible               " Be iMproved
endif

" Required:
set runtimepath+=$HOME/.config/nvim/dein/repos/github.com/Shougo/dein.vim

" Required:
if dein#load_state('$HOME/.config/nvim/dein')
    call dein#begin('$HOME/.config/nvim/dein')

    " Let dein manage dein
    " Required:
    call dein#add('$HOME/.config/nvim/dein/repos/github.com/Shougo/dein.vim')

    " Issues installing
    " call dein#add('majutsushi/tagbar')

    " Some combination of these causes segfaults, unsure why.
    " call dein#add('Shougo/neosnippet-snippets')
    " call dein#add('Shougo/neosnippet.vim')
    " call dein#add('airblade/vim-gitgutter')
    " call dein#add('vim-airline/vim-airline')

    call dein#add('Shougo/denite.nvim')
    call dein#add('Shougo/deoplete.nvim')
    call dein#add('Shougo/neomru.vim')
    call dein#add('Shougo/neoyank.vim')
    call dein#add('chemzqm/unite-location')
    call dein#add('easymotion/vim-easymotion')
    call dein#add('gregsexton/gitv')
    call dein#add('int3/vim-extradite')
    call dein#add('jceb/vim-orgmode')
    call dein#add('justinmk/vim-sneak')
    call dein#add('kshenoy/vim-signature')
    call dein#add('morhetz/gruvbox')
    call dein#add('nathanaelkane/vim-indent-guides')
    call dein#add('neomake/neomake')
    call dein#add('roxma/vim-tmux-clipboard')
    call dein#add('tpope/vim-commentary')
    call dein#add('tpope/vim-eunuch')
    call dein#add('tpope/vim-fugitive')
    call dein#add('tpope/vim-obsession')
    call dein#add('tpope/vim-repeat')
    call dein#add('tpope/vim-rsi')
    call dein#add('tpope/vim-sensible')
    call dein#add('tpope/vim-sleuth')
    call dein#add('tpope/vim-speeddating')
    call dein#add('tpope/vim-surround')
    call dein#add('tpope/vim-unimpaired')

    " Required:
    call dein#end()
    call dein#save_state()
endif

" Required:
filetype plugin indent on
syntax enable

" If you want to install not installed plugins on startup.
if dein#check_install()
    call dein#install()
endif

"End dein Scripts

let mapleader="\<space>"
let localleader = '\\'

nnoremap <silent> <leader>qq :qall<CR>

" Swap j, k, $ with gj, gk, g$. Go one line, even with wrapping.
nnoremap <silent> j gj
nnoremap <silent> k gk
nnoremap <silent> $ g$

nnoremap <silent> gj j
nnoremap <silent> gk k
nnoremap <silent> g$ $

onoremap <silent> j gj
onoremap <silent> k gk
onoremap <silent> $ g$

onoremap <silent> gj j
onoremap <silent> gk k
onoremap <silent> g$ $

vnoremap <silent> j gj
vnoremap <silent> k gk
vnoremap <silent> $ g$

vnoremap <silent> gj gj
vnoremap <silent> gk gk
vnoremap <silent> g$ g$

" smart-home: ^ and 0 will toggle between one another
nnoremap <expr> <silent> ^ col('.') == match(getline('.'),'\S')+1 ? 'g0' : 'g^'
vnoremap <expr> <silent> ^ col('.') == match(getline('.'),'\S')+1 ? 'g0' : 'g^'
onoremap <expr> <silent> ^ col('.') == match(getline('.'),'\S')+1 ? 'g0' : 'g^'
nnoremap <expr> <silent> 0 col('.') == 1 ? 'g^' : 'g0'
vnoremap <expr> <silent> 0 col('.') == 1 ? 'g^' : 'g0'
onoremap <expr> <silent> 0 col('.') == 1 ? 'g^' : 'g0'

" Reselect visually selected text after indenting
vnoremap <silent> > >gv
vnoremap <silent> < <gv<Paste>

" Use column marks by default
nnoremap <silent> ' `
nnoremap <silent> ` '

cmap <silent> w!! :SudoWrite<CR>

" Implement spacemacs <leader>w window map
nnoremap <silent> <leader>wh <C-W>h
nnoremap <silent> <leader>wj <C-W>j
nnoremap <silent> <leader>wk <C-W>k
nnoremap <silent> <leader>wl <C-W>l
nnoremap <silent> <leader>w- <C-W>s
nnoremap <silent> <leader>w/ <C-W>v
nnoremap <silent> <leader>wd <C-W>q
nnoremap <silent> <leader>wH <C-W>H
nnoremap <silent> <leader>wJ <C-W>J
nnoremap <silent> <leader>wK <C-W>K
nnoremap <silent> <leader>wL <C-W>L

" Implement spacemacs <leader>b buffer map
nnoremap <silent> <leader>bd :bdelete<CR>
nnoremap <silent> <leader>bl :Denite -auto-resize -winminheight=5 buffer<CR>
nnoremap <silent> <leader>bb :Denite -auto-resize -winminheight=5 buffer<CR>

" Implement spacemacs <leader>t toggle map
nnoremap <silent> <leader>ti :IndentGuidesToggle<CR>
nnoremap <silent> <leader>tm :SignatureToggle<CR>
nnoremap <silent> <leader>tt :TagbarToggle<CR>
nnoremap <silent> <leader>tc :Denite -auto-resize -winminheight=5 colorscheme<CR>

" Implement spacemacs <leader>f file map
nnoremap <silent> <leader>fed :e $HOME/.config/nvim/init.vim<CR>
nnoremap <silent> <leader>ff :DeniteBufferDir -auto-resize -winminheight=5 file_rec<CR>
nnoremap <silent> <leader>fr :Denite file_mru -auto-resize -winminheight=5<CR>

" Implement spacemacs <leader>p project map
call denite#custom#alias('source', 'file_rec/git', 'file_rec')
call denite#custom#var('file_rec/git', 'command',
            \ ['git', 'ls-files', '-co', '--exclude-standard'])
nnoremap <silent> <leader>pf :DeniteBufferDir -auto-resize -winminheight=5 file_rec/git<CR>


" Implement spacemacs <leader>g git map
nnoremap <silent> <leader>gs :Gstatus<CR>
nnoremap <silent> <leader>gl :Extradite<CR>

map <silent> <Leader><Leader><Leader> <Plug>(easymotion-s2)

" . returns to starting place after repeat
nnoremap <silent> . .`[

highlight ExtraWhiteSpace ctermbg=darkgreen guibg=darkgreen
match ExtraWhitespace /\s\+$\|\t/

" Move all automatically generated files
silent execute '!mkdir -p $HOME/.config/nvim/.cache/backup/'
silent execute '!mkdir -p $HOME/.config/nvim/.cache/swap/'
silent execute '!mkdir -p $HOME/.config/nvim/.cache/undo/'
silent execute '!mkdir -p $HOME/.config/nvim/.cache/views/'
set backupdir=$HOME/.config/nvim/.cache/backup/
set directory=$HOME/.config/nvim/.cache/swap/
set undodir=$HOME/.config/nvim/.cache/undo/
set viewdir=$HOME/.config/nvim/.cache/views/
set viminfo+=n~/.config/nvim/.cache/viminfo

" 4 Spaces!
set expandtab
set shiftwidth=4
set softtabstop=4
set tabstop=4

set breakindent           " preserve horizontal indentation when breaking
set colorcolumn=+1        " show @80 cols
set foldlevelstart=99     " open all folds on open
set hidden                " Allow for hidden buffers
set ignorecase            " Ignore case in searches (modified by smartcase)
set smartcase             " when caps are included in searches, make it case
                          "  sensitive
set list                  " show invisibles
set magic                 " Improve searching
set nojoinspaces          " 1 space when joining w/ a period, not 2
set nostartofline         " don't move the cursor when moving vertically
set notimeout             " wait for keystrokes ...
set number                " line numbers
set pumheight=10          " 10 entries in completion menus
set secure                " limit commands in local .nvimrc files
set showbreak=↪           " special character showing breaks
set showcmd               " show commands as they're entered
set showmatch             " show matching parens on insertion
set spellsuggest=best,10  " suggest spelling corrections, at most 10
set splitbelow            " split below, not above
set splitright            " split to right, not left
set undofile              " Keep track between invocation

" Spelling ...
set spellfile=$HOME/.dotfiles/vim/en.utf-8.add
set spelllang=en_US

set fillchars=          " reset fillchars
set fillchars+=fold:-   " fill foldtext with dashes
set fillchars+=diff:⣿   " indicate deleted lines in diffs with ⣿
set fillchars+=vert:│   " indicate vertical splits with │
set fillchars+=stlnc:━  " indicate horizontal splits with -

set listchars+=tab:▸▸      " display tabs
set listchars+=extends:❯   " hint that theres more to the right, ...
set listchars+=precedes:❮  " hint that theres more to the left, ...

set shortmess=    " reset shortmess
set shortmess+=f  " '(3 of 5)' instead of '(file 3 of 5)'
set shortmess+=i  " '[noeol] instead of '[Incompelte last line]'
set shortmess+=l  " '999L, 888C' instead of '999 lines, 888 characters'
set shortmess+=m  " '[+]' instead of '[Modified]'
set shortmess+=n  " '[New]' instead of '[New File]'
set shortmess+=r  " '[RO]' instead of '[readonly]'
set shortmess+=w  " '[w]' instead of 'written' '[a]' instead of 'appended'
set shortmess+=x  " '[dos]' instead of '[dos format]', '[unix]' instead of
                  "  '[unix format]', '[mac]' instead of '[mac format]'
set shortmess+=o  " overwrite message for writing a file with subsequent
                  "  message or reading a file (useful for ":wn" or when
                  "  'autowrite' on)
set shortmess+=O  " message for reading a file overwrites any previous
                  "  message. Also for quickfix message (e.g., ":cn").
set shortmess+=t  " truncate file message at the start if it is too long to
                  "  fit on the command-line, "<" will appear in the left most
                  "  column. Ignored in Ex mode.
set shortmess+=T  " truncate other messages in the middle if they are too long
                  "  to fit on the command line.  "..." will appear  in the
                  "  middle. Ignored in Ex mode.
set shortmess+=I  " don't give the intro message when starting Vim |:intro|.

set virtualedit=         " reset virtualedit
set virtualedit+=block   " can move past the end of the line in visual block
                         "  mode
set virtualedit+=insert  " can move past the end of the line in insert mode

set formatoptions=    " reset formatoptions
set formatoptions+=c  " autowrap comments using textwidth
set formatoptions+=q  " format comments with gq
set formatoptions+=n  " recognize numbered lists
set formatoptions+=j  " joining comments deletes comment leader

set wildmode=list:longest

set wildignore+=*/.git/*,*/.hg/*,*/.svn/*,*/CVS/*    " ignore vcs directories
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg       " ignore images
set wildignore+=*.o,*.so,*.out                       " ignore compiled objects
set wildignore+=*.sw?                                " ignore swap files
set wildignore+=*.DS_Store                           " ignore mac crap
set wildignore+=*.pyc,*.pyo                          " ignore compiled python
set wildignore+=*.log,*.LOG                          " ignore log files
set wildignore+=*.[12345678]                         " ignore rotated logs
set wildignore+=*.gz,*.tar,*.tgz,*.bz2,*.cpio,*.rpm  " ignore archives
set wildignore+=*.ignore                             " ignore extras

set completeopt=          " reset completeopt
set completeopt+=longest  " insert the longest match
set completeopt+=menuone  " show the menu when there are matches

set termguicolors    " Be pretty
set background=dark  " And dark!
let g:gruvbox_contrast_dark="hard"
colorscheme gruvbox

function! Preserve(command)
  " Preparation: save last search, and cursor position.
  let _s=@/
  let l = line(".")
  let c = col(".")
  " Do the business:
  execute a:command
  " Clean up: restore previous search history, and cursor position
  let @/=_s
  call cursor(l, c)
endfunction

function! RemoveTrailingWhiteSpace()
    " Allow a buffer local variable trailing_witespace_ok  to permit trailing
    " whitspace -- sometimes I need to do this if im editing someone elses code
    " and don't want to own the entire file in vcs. Enable it with let
    " trailing_witespace_ok=1, disable it with unlet trailing_witespace_ok
    if !exists("b:trailing_whitespace_ok")
        call Preserve("%s/\\s\\+$//e")
    endif
endfun
command! RemoveTrailingWhiteSpace :call RemoveTrailingWhiteSpace()

function! RestoreCursorPosition()
    " Restore cursor position to the last time you were in the file, this uses
    " marks so it's dependent on viminfo
    for ft in ['gitcommit', 'hgcommit', 'cvs', 'svn']
        if &filetype==ft
            return
        endif
    endfor
    normal! g`"
endfunction
"
" Highlight VCS conflict markers
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

" Restore cursor position
augroup restoreCursorPosition
  autocmd!
  autocmd BufReadPost * call RestoreCursorPosition()
augroup END

au FileType make au BufWritePre <buffer> RemoveTrailingWhiteSpace
au FileType make setl noexpandtab
au FileType make setl nosmarttab
au FileType make setl softtabstop=0
au FileType make setl tabstop=4

au FileType perl setl iskeyword+=$,%,@
au FileType perl au BufWritePre <buffer> RemoveTrailingWhiteSpace

au FileType puppet au BufWritePre <buffer> RemoveTrailingWhiteSpace

au FileType python au BufWritePre <buffer> RemoveTrailingWhiteSpace
au FileType python setl textwidth=79

au FileType sh,zsh au BufWritePre <buffer> RemoveTrailingWhiteSpace

au FileType vim au BufWritePre <buffer> RemoveTrailingWhiteSpace

au FileType help setl nolist
au FileType help setl nonumber
au FileType help setl nomodified
au FileType help setl nomodifiable
au FileType help nnoremap <buffer> q :close<cr>

au FileType org nnoremap <buffer> <leader>meeh :OrgExportToHTML<cr>

au FileType qf nnoremap <buffer> q :close<cr>

let g:deoplete#enable_at_startup=1
let g:deoplete#max_list=10

nnoremap <leader>wqf :Denite -auto-resize -winminheight=5 quickfix<cr>
nnoremap <leader>wll :Denite -auto-resize -winminheight=5 location_list<cr>

" <C-G> can act like emacs <C-G>
if empty(mapcheck('<C-G>', 'i'))
  inoremap <C-G> <C-C>
endif
if empty(mapcheck('<C-G>', 'n'))
  nnoremap <C-G> <C-C>
endif
if empty(mapcheck('<C-G>', 'c'))
  cnoremap <C-G> <C-C>
endif

nnoremap <silent> <M-x> :Denite -auto-resize -winminheight=5 command<CR>
call denite#custom#map('insert', '<Up>', '<denite:move_to_previous_line>', 'noremap')
call denite#custom#map('insert', '<Down>', '<denite:move_to_next_line>', 'noremap')

" From my spacemacs <leader>a map ...
call denite#custom#alias('source', 'file_rec/notes', 'file_rec')
call  denite#custom#var('file_rec/notes', 'command', ['bash', '-c', "ls -1 $HOME/notes/*.org"])
nnoremap <silent> <leader>an :Denite -auto-resize -winminheight=5 file_rec/notes<CR>

set clipboard+=unnamed

" Implement spacemacs <leader>r git map
nnoremap <silent> <leader>ry :Denite neoyank -auto-resize -winminheight=5<cr>
nnoremap <silent> <leader>rj :Denite jump -auto-resize -winminheight=5<cr>
nnoremap <silent> <leader>rr :Denite register -auto-resize -winminheight=5<cr>
nnoremap <silent> <leader>rc :Denite change -auto-resize -winminheight=5<cr>
nnoremap <silent> <leader>rl :Denite -resume -auto-resize -winminheight=5<cr>
nnoremap <silent> <leader>rm :Denite mark -auto-resize -winminheight=5<cr>

nnoremap <silent> <leader>h :Denite help -auto-resize -winminheight=5<cr>

autocmd! BufWritePost * Neomake
autocmd! BufReadPost * Neomake

let g:neomake_error_sign = { 'text': '>>' } ", 'texthl': 'NeomakeErrorSign' }
let g:neomake_warning_sign = { 'text': '==' } ", 'texthl': 'NeomakeWarningSign' }
let g:neomake_info_sign = { 'text': '--'} " , 'texthl': 'NeomakeInfoSign' }
let g:neomake_message_sign = { 'text': '~~' } ", 'texthl': 'NeomakeMessageSign' }
hi NeomakeError cterm=NONE ctermfg=167 gui=NONE guisp=#fb4934
hi NeomakeWarning cterm=NONE ctermfg=223 gui=NONE guisp=#ebdbb2
hi NeomakeInfo cterm=NONE ctermfg=208 gui=NONE guisp=#fe8019
hi NeomakeMessage cterm=NONE ctermfg=214 gui=NONE guisp=#fabd2f
