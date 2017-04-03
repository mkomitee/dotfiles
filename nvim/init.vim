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

    " call dein#add('tpope/vim-vinegar')
    call dein#add('Shougo/denite.nvim')
    call dein#add('Shougo/deol.nvim')
    call dein#add('Shougo/deoplete.nvim')
    call dein#add('Shougo/neosnippet-snippets')
    call dein#add('Shougo/neosnippet.vim')
    call dein#add('Shougo/neoyank.vim')
    call dein#add('airblade/vim-gitgutter')
    call dein#add('easymotion/vim-easymotion')
    call dein#add('gregsexton/gitv')
    call dein#add('int3/vim-extradite')
    call dein#add('justinmk/vim-sneak')
    call dein#add('kshenoy/vim-signature')
    call dein#add('majutsushi/tagbar')
    call dein#add('morhetz/gruvbox')
    call dein#add('nathanaelkane/vim-indent-guides')
    call dein#add('tpope/vim-commentary')
    call dein#add('tpope/vim-rsi')
    call dein#add('tpope/vim-obsession')
    call dein#add('tpope/vim-eunuch')
    call dein#add('tpope/vim-fugitive')
    call dein#add('tpope/vim-repeat')
    call dein#add('tpope/vim-sensible')
    call dein#add('tpope/vim-sleuth')
    call dein#add('tpope/vim-surround')
    call dein#add('tpope/vim-unimpaired')
    call dein#add('vim-airline/vim-airline')

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

"End dein Scripts-------------------------
"
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

" smart-home: ^ and 0 will toggle between one another {{{
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
nnoremap <silent> <leader>bl :Denite buffer<CR>
nnoremap <silent> <leader>bb :Denite buffer<CR>

" Implement spacemacs <leader>t toggle map
nnoremap <silent> <leader>ti :IndentGuidesToggle<CR>
nnoremap <silent> <leader>tm :SignatureToggle<CR>
nnoremap <silent> <leader>tt :TagbarToggle<CR>
"
" Implement spacemacs <leader>f file map
nnoremap <silent> <leader>fed :e $HOME/.config/nvim/init.vim<CR>
nnoremap <silent> <leader>ff :DeniteBufferDir file_rec<CR>

" Implement spacemacs <leader>p project map
call denite#custom#alias('source', 'file_rec/git', 'file_rec')
call denite#custom#var('file_rec/git', 'command',
            \ ['git', 'ls-files', '-co', '--exclude-standard'])
nnoremap <silent> <leader>pf :DeniteBufferDir file_rec/git<CR>
"
" Implement spacemacs <leader>g git map
nnoremap <silent> <leader>gs :Gstatus<CR>
nnoremap <silent> <leader>gl :Extradite<CR>

map <silent> <Leader><Leader><Leader> <Plug>(easymotion-s2)

" . returns to starting place after repeat 
nnoremap <silent> . .`[

highlight ExtraWhiteSpace ctermbg=darkgreen guibg=darkgreen
match ExtraWhitespace /\s\+$\|\t/

set background=dark
set backupdir=$HOME/.config/nvim/.cache/backup/
set breakindent
set colorcolumn=+1
set directory=$HOME/.config/nvim/.cache/swap/
set expandtab
set foldlevelstart=99
set hidden
set ignorecase
set list
set magic
set nojoinspaces
set nostartofline
set notimeout
set number
set pumheight=10
set secure
set shiftwidth=4
set showbreak=↪
set showcmd
set showmatch
set smartcase
set softtabstop=4
set spellfile=$HOME/.dotfiles/vim/en.utf-8.add
set spelllang=en_US
set spellsuggest=best,10
set splitbelow
set splitright
set tabstop=4
set termguicolors
set undodir=$HOME/.config/nvim/.cache/undo/
set undofile
set viewdir=$HOME/.config/nvim/.cache/views/
set viminfo+=n~/.config/nvim/.cache/viminfo

set fillchars=         " reset fillchars
set fillchars+=fold:-  " fill foldtext with dashes
set fillchars+=diff:⣿  " indicate deleted lines in diffs with ⣿
set fillchars+=vert:│  " indicate vertical splits with │
set fillchars+=stlnc:━ " indicate horizontal splits with -

set listchars+=tab:▸▸     " display tabs
set listchars+=extends:❯  " hint that theres more to the right, ...
set listchars+=precedes:❮ " hint that theres more to the left, ...

set shortmess=   " reset shortmess
set shortmess+=f " '(3 of 5)' instead of '(file 3 of 5)'
set shortmess+=i " '[noeol] instead of '[Incompelte last line]'
set shortmess+=l " '999L, 888C' instead of '999 lines, 888 characters'
set shortmess+=m " '[+]' instead of '[Modified]'
set shortmess+=n " '[New]' instead of '[New File]'
set shortmess+=r " '[RO]' instead of '[readonly]'
set shortmess+=w " '[w]' instead of 'written' '[a]' instead of 'appended'
set shortmess+=x " '[dos]' instead of '[dos format]', '[unix]' instead of
                 "  '[unix format]', '[mac]' instead of '[mac format]'
set shortmess+=o " overwrite message for writing a file with subsequent message
                 "  for reading a file (useful for ":wn" or when 'autowrite' on)
set shortmess+=O " message for reading a file overwrites any previous message.
                 "  Also for quickfix message (e.g., ":cn").
set shortmess+=t " truncate file message at the start if it is too long to fit
                 "  on the command-line, "<" will appear in the left most column.
                 "  Ignored in Ex mode.
set shortmess+=T " truncate other messages in the middle if they are too long to
                 "  fit on the command line.  "..." will appear  in the middle.
                 "  Ignored in Ex mode.
set shortmess+=I " don't give the intro message when starting Vim |:intro|.

set virtualedit=        " reset virtualedit
set virtualedit+=block  " can move past the end of the line in visual block mode
set virtualedit+=insert " can move past the end of the line in insert mode

set formatoptions=   " reset formatoptions
set formatoptions+=c " autowrap comments using textwidth
set formatoptions+=q " format comments with gq
set formatoptions+=n " recognize numbered lists
set formatoptions+=j " joining comments deletes comment leader

set wildmode=list:longest

set wildignore+=*/.git/*,*/.hg/*,*/.svn/*,*/CVS/*   " ignore vcs directories
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg      " ignore images
set wildignore+=*.o,*.so,*.out                      " ignore compiled objects
set wildignore+=*.sw?                               " ignore swap files
set wildignore+=*.DS_Store                          " ignore mac crap
set wildignore+=*.pyc,*.pyo                         " ignore compiled python
set wildignore+=*.log,*.LOG                         " ignore log files
set wildignore+=*.[12345678]                        " ignore rotated logs
set wildignore+=*.gz,*.tar,*.tgz,*.bz2,*.cpio,*.rpm " ignore archives
set wildignore+=*.ignore                            " ignore extras

let g:gruvbox_contrast_dark="hard"
colorscheme gruvbox
