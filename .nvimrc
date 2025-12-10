" Basic settings
set number
set shiftwidth=2
set tabstop=2
set expandtab
set autoindent
set smartcase
set ignorecase
set incsearch
set hlsearch
set nowrap
set linebreak
set mouse=a
set clipboard=unnamed,unnamedplus
set undofile
set undodir=~/.vim/undo
set noswapfile
set scrolloff=5

" Colors
syntax on
set termguicolors
colorscheme default

" Plugins
call plug#begin('~/.local/share/nvim/plugged')

Plug 'wellle/targets.vim'
Plug 'bkad/CamelCaseMotion'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'editorconfig/editorconfig-vim'
Plug 'christoomey/vim-tmux-navigator'

call plug#end()

" Leader
let mapleader = " "

nnoremap <leader>fs :w<CR>
nnoremap <silent> <esc> :noh<CR><esc>

" Redirect all delete-operations to black-hole
" Use 'm' ("move") for all cut-operations
nnoremap d "_d
nnoremap m d
nnoremap mm dd

vnoremap d "_d
vnoremap m d

nnoremap c "_c

nnoremap x "_x
nnoremap <leader>x x

" buffer and tab navigation
nnoremap <leader>n :enew<CR>
nnoremap <leader>j :bp<CR>
nnoremap <leader>k :bn<CR>
nnoremap <leader>q :bd<CR>

" split navigation
nmap <leader>wq <C-w>q
nmap <leader>w- :split<CR>
nmap <leader>w\| :vsplit<CR>
nmap <leader>wo :only<CR>

" movement
nmap <silent> w <Plug>CamelCaseMotion_w
nmap <silent> b <Plug>CamelCaseMotion_b
nmap <silent> e <Plug>CamelCaseMotion_e

nnoremap j gj
nnoremap k gk

" safe paste in visual mode
xnoremap p "_dP



" Include local, .gitignored files:
for file in [
      \ $HOME . '/.vimrc.abbreviations',
      \ '.vimrc.abbreviations',
      \ $HOME . '/.nvimrc.local',
      \ '.nvimrc.local'
      \ ]
  if filereadable(file)
    execute 'source' file
  endif
endfor
