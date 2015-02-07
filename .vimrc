set nocompatible              " be iMproved, required
filetype off                  " required

call plug#begin('~/.vim/plugged')

Plug 'vim-scripts/genutils'

Plug 'kien/ctrlp.vim'
Plug 'camelcasemotion'
Plug 'digitaltoad/vim-jade'
Plug 'groenewege/vim-less'
Plug 'godlygeek/tabular'
Plug 'plasticboy/vim-markdown'
Plug 'elzr/vim-json'
Plug 'mattn/emmet-vim'
Plug 'lervag/vim-latex'
Plug 'christoomey/vim-tmux-navigator'
Plug 'bling/vim-airline'
Plug 'terryma/vim-multiple-cursors'
Plug 'scrooloose/syntastic'
Plug 'xolox/vim-misc'
Plug 'altercation/vim-colors-solarized'
Plug 'editorconfig/editorconfig-vim'
Plug 'marijnh/tern_for_vim'
Plug 'tpope/vim-surround'
Plug 'rhysd/committia.vim'
Plug 'gcmt/wildfire.vim'
Plug 'svermeulen/vim-repeat'
Plug 'svermeulen/vim-easyclip'
Plug 'jonathanfilip/vim-lucius'
Plug 'Lokaltog/vim-easymotion'
Plug 'matchit.zip'
Plug 'Valloric/YouCompleteMe', { 'do': './install.sh' }

call plug#end()

" Colorscheme
if $COLORTERM == 'gnome-terminal'
  set t_Co=256
endif

set background=light
let g:lucius_use_underline = 0
colorscheme lucius
LuciusWhite

syntax on

set number
set shiftwidth=4
set tabstop=4
set colorcolumn=80
set textwidth=0

set autoindent
set showmatch
set hlsearch
set incsearch
set ignorecase
set smartcase

set nowrap
set linebreak
set nolist

set expandtab
set smarttab

"keep backup files in central directory (dirs have to exist)
set backup
set backupdir=~/.vim/backup
set directory=~/.vim/tmp

"enable persistent undo (directory has to exist)
set undofile
set undodir=~/.vim/undo
set undolevels=1000
set undoreload=10000

" No error bells
set noeb vb t_vb=

"better tab-completion
set wildmode=longest,list,full
set wildmenu
set hidden

set scrolloff=5

"Always display status bar (vim-airline)
set laststatus=2

if has("gui_running")
   " Remove Toolbar
   set guioptions-=T
endif

let mapleader = ";"
let maplocalleader = "\\"

set pastetoggle=<F2>

" buffer related stuff
nnoremap tk  :bn<CR>
nnoremap tj  :bp<CR>
nnoremap tq  :bw<CR>

" Search mappings: These will make it so that going to the next one in a
" search will center on the line it's found in.
map N Nzz
map n nzz

"Movement in wrapped lines
nmap j gj
nmap k gk
nmap 0 g0
nmap $ g$

nmap gq :bw<CR>
nmap <C-s> :w<CR>

nmap w ,w
nmap b ,b
nmap e ,e
nmap ss :w<CR>

nmap <leader><CR> :nohlsearch<CR>
nmap <leader>s :w<CR>

nmap <Leader>a= :Tabularize /=<CR>
vmap <Leader>a= :Tabularize /=<CR>
nmap <Leader>a: :Tabularize /:\zs<CR>
vmap <Leader>a: :Tabularize /:\zs<CR>


"Enable emmet only for html-ish files
let g:user_emmet_install_global = 0
autocmd FileType html,css EmmetInstall
au BufNewFile,BufRead *.ejs set filetype=html
au BufNewFile,BufRead *.jade call SetJadeOptions()

function! SetJadeOptions()
    set filetype=jade
    setlocal indentkeys-=*<Return>
endfunction

au BufNewFile,BufRead *.less set filetype=less

let g:ctrlp_custom_ignore = '\v[\/](.git|
                                   \.hg|
                                   \.svn|
                                   \node_modules|
                                   \bower_components|
                                   \doc|
                                   \docs)$'

"press gp to reselect pasted text
nnoremap <expr> gp '`[' . strpart(getregtype(), 0, 1) . '`]'

nmap <S-m> v$hm

"check for correct indentation only
let g:airline#extensions#whitespace#checks = [ 'indent' ]

" allow spaces after tabs
let g:airline#extensions#whitespace#mixed_indent_algo = 1

"display buffers
let g:airline#extensions#tabline#enabled = 1

" html checker doesn't know html5...
let g:syntastic_html_checkers = []

let g:syntastic_javascript_checkers = ['jshint']

"disable folding for vim-markdown (to prevent everything being folded on open)
let g:vim_markdown_folding_disabled=1

"don't conceal quotes in json files
let g:vim_json_syntax_conceal=0

let g:EditorConfig_verbos = 1

" If ~/.vimrc.local exists, source it to support host-local configs
if filereadable( $HOME.'/.vimrc.local' )
    source ~/.vimrc.local
endif

" If .vimrc.local exists in current directory, to support project-local configs
if filereadable( ".vimrc.local" )
    source .vimrc.local
endif
