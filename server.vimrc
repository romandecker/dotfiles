" vim: set foldmethod=marker

set number
" Plugins {{{
" ===========
call plug#begin('~/.nvim/plugged')

" Libraries
" Plug 'vim-scripts/genutils'
Plug 'svermeulen/vim-repeat'
Plug 'wellle/targets.vim'   " Add additional text-objects
Plug 'michaeljsmith/vim-indent-object'   " indent text-object
Plug 'kana/vim-operator-user'

" Motions
Plug 'bkad/CamelCaseMotion'
Plug 'glts/vim-textobj-comment' " A comment text-object
Plug 'kana/vim-textobj-user'    " needed by vim-textobj-xmlattr
Plug 'whatyouhide/vim-textobj-xmlattr' " XML/HTML attribute text objects (ix,ax)
Plug 'tmhedberg/matchit' " More uses for %
Plug 'ironhouzi/vim-stim' " Better *
Plug 'easymotion/vim-easymotion'
Plug 'jeetsukumaran/vim-indentwise'
Plug 'bronson/vim-visual-star-search'

" Language-specific
Plug 'ekalinin/Dockerfile.vim', { 'for': 'Dockerfile' }
Plug 'othree/yajs.vim', { 'for': 'javascript' }
Plug 'elzr/vim-json', { 'for': 'json' }

" File management
Plug 'ctrlpvim/ctrlp.vim'

" editing/formatting
Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'cohama/lexima.vim'
Plug 'mjbrownie/swapit'
Plug 'ntpeters/vim-better-whitespace'

" Visual
Plug 'bling/vim-airline'
Plug 'chreekat/vim-paren-crosshairs'

" Integration
Plug 'tpope/vim-obsession' " required for vim-prosession
Plug 'dhruvasagar/vim-prosession'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-eunuch' " Some unix commands as vim commands
Plug 'kopischke/vim-fetch' " open files with line numbers like file.c:22
Plug 'moll/vim-bbye'    " close buffers without messing up window layout

" Server-specific
Plug 'vim-scripts/nginx.vim'


call plug#end()
" }}}


" Basic settings {{{
" ==================
syntax on

set shiftwidth=2
set tabstop=2
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

set modeline

"keep backup files in central directory (dirs have to exist)
set backupdir=~/.vim/backup
set directory=~/.vim/tmp
" Avoid swapfiles alltogether
set noswapfile

"enable persistent undo (directory has to exist)
set undofile
set undodir=~/.vim/undo
set undolevels=1000
set undoreload=10000

" No error bells
set noeb

"better tab-completion
set wildmode=longest,list,full
set wildmenu

set hidden

set scrolloff=5
set sidescrolloff=10
set sidescroll=1

set mouse=a

"Always display status bar (vim-airline)
set laststatus=2
"
"Always use the system register
set clipboard=unnamed,unnamedplus

" Allow backspace to delete previously entered characters
set backspace=indent,eol,start

" }}}

" Mappings {{{
" ============
let mapleader = " "
let maplocalleader = "\\"

set pastetoggle=<F2>

nmap <leader>vpp :source $MYVIMRC<CR>
nmap <leader>vpi :PlugInstall<CR>
nmap <leader>vpu :PlugUpdate<CR>
nmap <leader>vpc :PlugClean<CR>


" Search mappings: These will make it so that going to the next one in a
" search will center on the line it's found in.
map N Nzz
map n nzz



" buffer related stuff
nnoremap <leader>n  :enew<CR>
nnoremap <leader>j  :bp<CR>
nnoremap <leader>k  :bn<CR>
nnoremap <leader>q  :Bdelete<CR>    " use command from moll/vim-bbye
nnoremap <leader><S-q>  :bufdo bd<CR>

nnoremap tn :tabnew<CR>
nnoremap tj :tabp<CR>
nnoremap tk :tabn<CR>

" Movement in wrapped lines
nnoremap j gj
nnoremap k gk
nnoremap 0 g0
nnoremap $ g$

" save with leader s
nmap <leader>s :w<CR>
" open current directory with leader f d
nmap <leader>fs :w<CR>
nmap <leader>fd :e .<CR>
" save with C-s (when forwarding is enabled in terminal, or in gui)
nmap <C-s> :w<CR>

" enable camelcasemotion
nmap <silent> w <Plug>CamelCaseMotion_w
nmap <silent> b <Plug>CamelCaseMotion_b
nmap <silent> e <Plug>CamelCaseMotion_e

" Redirect all delete-operations to black-hole
" Use 'm' ("move") for all cut-operations
nnoremap d "_d
nnoremap m d
nnoremap mm dd

vnoremap d "_d
vnoremap m d

" remap p in visual mode to first delete to blackhole to prevent tainting yank
" register
xnoremap p "_dP

" Use gm for marks instead of normal m
nnoremap gm m

" Clear highlighting on escape in normal mode
nnoremap <esc> :noh<return><esc>
" needed so that vim still understands escape sequences
nnoremap <esc>^[ <esc>^[

" window-stuff with leader w
nmap <leader>ws :split<CR>
nmap <leader>w<S-s> :vsplit<CR>
nmap <leader>wq <C-w>q

" I keep pressing this accidentally way to often...
nmap q: <Nop>

imap <C-l> <Del>

" Leader-p instead of =p for indented paste
nmap <Leader>p =p

nnoremap <Tab> %

" navigate jumplist
nnoremap [j <C-o>
nnoremap ]j <C-i>

" navigate changelist
nnoremap [c g;
nnoremap ]c g,
" }}}

" Plugin-specific {{{

"check for correct indentation only
let g:airline#extensions#whitespace#checks = [ 'indent' ]

" allow spaces after tabs
let g:airline#extensions#whitespace#mixed_indent_algo = 1

"display buffers
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'

let g:EasyMotion_do_mapping = 0

nmap <leader>fj <Plug>(easymotion-bd-jk)
nmap <leader>fk <Plug>(easymotion-bd-jk)
nmap <leader>fw <Plug>(easymotion-bd-w)
nmap <leader>ff <Plug>(easymotion-bd-f)

" Delete a block
nmap dab $d%dd

" rules for jumping over closing stuff when there's whitespace present
call lexima#add_rule( { 'char': ')', 'at': '\%#\s*)', 'leave': ')' } )
call lexima#add_rule( { 'char': ']', 'at': '\%#\s*]', 'leave': ']' } )
call lexima#add_rule( { 'char': '}', 'at': '\%#\s*}', 'leave': '}' } )

" call lexima#add_rule( { 'char': ';', 'at': '\%#\.*)$', 'input_after': ';', 'leave': ')' } )

" rule for wrapping a line in a block (sadly not dot-repeatable)
call lexima#add_rule( { 'char': '<CR>',
                    \   'at': '{\%#}\S\+',
                    \   'input': '<Esc>ll"td$i<CR><Esc>O<C-r>t' } )

" }}}

" autocmd {{{
autocmd BufNewFile,BufRead *.ejs set filetype=html
autocmd BufNewFile,BufRead *.jade set filetype=jade

autocmd FileType less set filetype=less

" Automatically set nopaste when exiting insert mode
autocmd InsertLeave * set nopaste

" Automatically resize splits when window is resized
autocmd VimResized * exe "normal! \<c-w>="

autocmd FileType javascript autocmd BufWritePre <buffer> StripWhitespace

" Automatically activate nginx.vim for nginx config files
autocmd BufRead,BufNewFile /etc/nginx/*,/usr/local/nginx/conf/* if &ft == '' | setfiletype nginx | endif 


" Set ctrlp method depending on whether ag is available or not
if executable('ag')
    set grepprg=ag\ --nogroup\ --nocolor
    let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
else
    let g:ctrlp_custom_ignore = '\v[\/](.git|
                                       \.hg|
                                       \.svn|
                                       \node_modules|
                                       \bower_components|
                                       \.session.vim)$'
endif

" }}}

" Local Variables:
" mode: vimrc
" End:
