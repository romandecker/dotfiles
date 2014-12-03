set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'

Plugin 'vim-scripts/genutils'

Plugin 'kien/ctrlp.vim'
Plugin 'camelcasemotion'
Plugin 'mattn/emmet-vim'
Plugin 'lervag/vim-latex'

Plugin 'christoomey/vim-tmux-navigator'
Plugin 'airblade/vim-gitgutter'
Plugin 'bling/vim-airline'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'scrooloose/syntastic'
Plugin 'xolox/vim-misc'
Plugin 'xolox/vim-notes'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

colorscheme solarized

set number
set sw=4
set ts=4
set tw=80
set cc=80

set autoindent
set showmatch
set hlsearch
set incsearch
set ignorecase

set expandtab
set smarttab

set sc

nmap <C-l> gt
nmap <C-h> gT
nmap <C-t> :tabnew<CR>

if has("gui_running")
   " Remove Toolbar
   set guioptions-=T
endif

" Tab related stuff
nnoremap th  :tabfirst<CR>
nnoremap tk  :tabnext<CR>
nnoremap tj  :tabprev<CR>
nnoremap tl  :tablast<CR>
nnoremap tt  :tabedit<Space>
nnoremap tn  :tabnext<Space>
nnoremap tm  :tabm<Space>
nnoremap td  :tabclose<CR>


" Search mappings: These will make it so that going to the next one in a
" search will center on the line it's found in.
map N Nzz
map n nzz

"keep backup files in central directory (dirs have to exist)
set backup
set backupdir=~/.vim/backup
set directory=~/.vim/tmp

"enable persistent undo
set undofile
set undodir=~/.vim/undo
set undolevels=1000
set undoreload=10000

" No error bells
set noeb vb t_vb=

"Movement in wrapped lines
nmap j gj
nmap k gk
nmap 0 g0
nmap $ g$

nmap <S-j> 4j
nmap <S-k> 4k

xmap <S-j> 4j
xmap <S-k> 4k

nmap gq :bw<CR>
nmap <C-s> :w<CR>

nmap w ,w
nmap b ,b
nmap e ,e

let mapleader = ";"
let maplocalleader = "\\"
nmap <leader><CR> :nohlsearch<CR>
nmap <leader>s :w<CR>

let g:ctrlp_custom_ignore = '\v[\/](.git|.hg|.svn|node_modules|bower_components)$'

"press gp to reselect pasted text
nnoremap <expr> gp '`[' . strpart(getregtype(), 0, 1) . '`]'

"Always display status bar (vim-airline)
set laststatus=2


let g:syntastic_javascript_chjeckers = ['jshint']
