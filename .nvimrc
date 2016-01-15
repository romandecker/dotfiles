" vim: set foldmethod=marker foldlevel=0:

" Plugins {{{
" ===========
call plug#begin('~/.nvim/plugged')

" Libraries
Plug 'vim-scripts/genutils'
Plug 'xolox/vim-misc'
Plug 'svermeulen/vim-repeat'
Plug 'kana/vim-submode'
Plug 'tpope/vim-dispatch'   " needed for omnisharp
Plug 'wellle/targets.vim'   " Add additional text-objects
Plug 'michaeljsmith/vim-indent-object'   " indent text-object

" Motions
Plug 'camelcasemotion'
Plug 'justinmk/vim-sneak'
Plug 'glts/vim-textobj-comment' " A comment text-object
Plug 'kana/vim-textobj-user'    " needed by vim-textobj-xmlattr
Plug 'whatyouhide/vim-textobj-xmlattr' " XML/HTML attribute text objects (ix,ax)
Plug 'matchit.zip' " More uses for %
Plug 'ironhouzi/vim-stim' " Better *

" Language-specific
Plug 'plasticboy/vim-markdown', { 'for': 'mkd' }
Plug 'shime/vim-livedown'
Plug 'lervag/vimtex'
Plug 'ekalinin/Dockerfile.vim'
Plug 'mattn/emmet-vim'
Plug 'digitaltoad/vim-jade', { 'for': 'jade' }
Plug 'heavenshell/vim-jsdoc'
Plug 'pangloss/vim-javascript'
Plug 'elzr/vim-json', { 'for': 'json' }
Plug 'marijnh/tern_for_vim', { 'for': 'javascript' }
Plug 'leafgarland/typescript-vim'
Plug 'kchmck/vim-coffee-script'

" File management
Plug 'ctrlpvim/ctrlp.vim'
Plug 'scrooloose/nerdtree'
Plug 'danro/rename.vim'
Plug 'rking/ag.vim'
Plug 'dyng/ctrlsf.vim'

" editing/formatting
Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'DeX3/vim-argformat'
Plug 'dhruvasagar/vim-table-mode'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'zef/vim-cycle'
Plug 'cohama/lexima.vim'
Plug 'ntpeters/vim-better-whitespace'

" Visual
Plug 'DeX3/vim-smartresize'
Plug 'bling/vim-airline'
Plug 'ntpeters/vim-airline-colornum'
Plug 'chreekat/vim-paren-crosshairs'
Plug 'Valloric/MatchTagAlways'
Plug 'Yggdroot/indentLine'

" Colors
Plug 'flazz/vim-colorschemes'   " a lot of basic colorschemes

" Integration
Plug 'christoomey/vim-tmux-navigator'
Plug 'brauner/vimtux'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-obsession' " required for vim-prosession
Plug 'dhruvasagar/vim-prosession'
Plug 'vim-utils/vim-husk'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-eunuch' " Some unix commands as vim commands

" Misc
Plug 'benekastah/neomake'
Plug 'editorconfig/editorconfig-vim'
Plug 'Valloric/YouCompleteMe', { 'do': './install.py' }
Plug 'sjl/gundo.vim'    " the undo-tree
Plug 'SirVer/ultisnips'
Plug 'moll/vim-bbye'    " close buffers without messing up window layout
Plug 'xolox/vim-notes'

call plug#end()
" }}}

" Colors {{{
" ==========
if $COLORTERM == 'gnome-terminal'
  set t_Co=256
endif

set background=dark
colorscheme apprentice
" }}}

" Basic settings {{{
" ==================

syntax on

set number
set shiftwidth=2
set tabstop=2
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

set modeline

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
set sidescrolloff=10
set sidescroll=1

set mouse=a

"Always display status bar (vim-airline)
set laststatus=2

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

nmap <leader>pp :source $MYVIMRC<CR>
nmap <leader>pi :PlugInstall<CR>
nmap <leader>pu :PlugUpdate<CR>
nmap <leader>pc :PlugClean<CR>


" Search mappings: These will make it so that going to the next one in a
" search will center on the line it's found in.
map N Nzz
map n nzz



" buffer related stuff
nnoremap <leader>n  :tabnew<CR>
nnoremap <leader>j  :tabprevious<CR>
nnoremap <leader>k  :tabnext<CR>
nnoremap <leader>q  :bdelete<CR>    " use command from moll/vim-bbye
nnoremap <leader><S-q>  :bufdo bd<CR>

nnoremap tn :tabnew<CR>
nnoremap tj :tabp<CR>
nnoremap tk :tabn<CR>
nnoremap gn :enew<CR>
nnoremap gj :bprevious<CR>
nnoremap gk :bnext<CR>

" Movement in wrapped lines
nnoremap j gj
nnoremap k gk
nnoremap 0 g0
nnoremap $ g$

" save with leader s
nmap <leader>s :w<CR>
" save with C-s (when forwarding is enabled in terminal, or in gui)
nmap <C-s> :w<CR>

" enable camelcasemotion
nmap w ,w
nmap b ,b
nmap e ,e

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

" Toggle NERDTree with leader w t
nmap <leader>wt :NERDTreeToggle<CR>

nmap g/ <Plug>CtrlSFPrompt

nmap <Leader>fu :call UnWrap()<CR>

"press gp to reselect pasted text
nnoremap <expr> gp '`[' . strpart(getregtype(), 0, 1) . '`]'

nmap <S-u> :GundoToggle<CR>

nmap <Leader><Leader> :call ExecuteKeys( 'Up Enter' )<CR>

" I keep pressing this accidentally way to often...
nmap q: <Nop>

nmap <Leader>fc :ArgFormatConcise<CR>
nmap <Leader>fm :ArgFormatMultiline<CR>
nmap <Leader>fp :ArgFormatOnPar<CR>

" duplicate the above block with <leader>db
nnoremap <Leader>db mp?)\\|]\\|}<CR><S-v>%y`pp:nohl<CR>

imap <C-l> <Del>

" Easier to type umlauts, make sure to set M-u and M-s accordingly in
" .vimrc.local
" e.g. set <M-u>=u
" generate the u by pressing <C-v> and then <M-u>!
imap <M-u> <C-k>:
imap <M-s> <C-k>ss

" Use C-p to duplicate a block of code in visual mode
vmap <Leader>db y`>p

call submode#enter_with('vresize', 'n', '', '<leader>wj', ':SmartResizeJ<CR>')
call submode#enter_with('vresize', 'n', '', '<leader>wk', ':SmartResizeK<CR>')
call submode#leave_with('vresize', 'n', '', '<Esc>')
call submode#map('vresize', 'n', '', 'j', ':SmartResizeJ<CR>' )
call submode#map('vresize', 'n', '', 'k', ':SmartResizeK<CR>')

call submode#enter_with('resize', 'n', '', '<leader>wh', ':SmartResizeH<CR>')
call submode#enter_with('resize', 'n', '', '<leader>wl', ':SmartResizeL<CR>')
call submode#leave_with('resize', 'n', '', '<Esc>')
call submode#map('resize', 'n', '', 'h', ':SmartResizeH<CR>' )
call submode#map('resize', 'n', '', 'l', ':SmartResizeL<CR>')
" }}}

" Plugin-specific {{{

"Enable emmet only for html-ish files
let g:user_emmet_install_global = 0
autocmd FileType html,css,xml EmmetInstall

let g:ycm_autoclose_preview_window_after_completion = 1

" make YCM compatible with UltiSnips
let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']

" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger = "<Tab>"
let g:UltiSnipsJumpForwardTrigger = "<C-f>"
let g:UltiSnipsJumpBackwardTrigger = "<C-b>"

"check for correct indentation only
let g:airline#extensions#whitespace#checks = [ 'indent' ]

" allow spaces after tabs
let g:airline#extensions#whitespace#mixed_indent_algo = 1

"display buffers
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'

"use powerline fonts
let g:airline_powerline_fonts = 1

let g:neomake_javascript_enabled_makers = ['jshint', 'jscs']



"disable folding for vim-markdown (to prevent everything being folded on open)
let g:vim_markdown_folding_disabled=1

"don't conceal quotes in json files
let g:vim_json_syntax_conceal=0

let g:EditorConfig_verbose = 1
let g:EditorConfig_exclude_patterns = ['fugitive://.*']

let g:vimtex_fold_enabled = 0
let g:tex_flavor = 'latex'
let g:vimtex_indent_enabled = 0

let g:ctrlsf_position = 'bottom'
let g:ctrlsf_indent = 2
let g:ctrlsf_default_root = 'cwd'
let g:ctrlsf_mapping = {
    \ "next": "n",
    \ "prev": "N",
    \ }

let g:jsdoc_allow_input_prompt = 1
let g:jsdoc_input_description = 0
let g:jsdoc_input_return_description = 0

let g:argformat_spaces_around_arglist = 1

" rules for jumping over closing stuff when there's whitespace present
call lexima#add_rule( { 'char': ')', 'at': '\%#\s*)', 'leave': ')' } )
call lexima#add_rule( { 'char': ']', 'at': '\%#\s*]', 'leave': ']' } )
call lexima#add_rule( { 'char': '}', 'at': '\%#\s*}', 'leave': '}' } )

" rule for wrapping a line in a block (sadly not dot-repeatable)
call lexima#add_rule( { 'char': '<CR>',
                    \   'at': '{\%#}\S\+',
                    \   'input': '<Esc>ll"td$i<CR><Esc>O<C-r>t' } )

let g:ctrlp_prompt_mappings = {
  \ 'PrtHistory(-1)':       [],
  \ 'PrtHistory(1)':        [],
  \ 'AcceptSelection("e")': ['<C-b>', '<2-LeftMouse>'],
  \ 'AcceptSelection("h")': ['<c-x>', '<c-cr>', '<c-s>'],
  \ 'AcceptSelection("t")': ['<CR>'],
  \ 'AcceptSelection("v")': ['<c-v>', '<RightMouse>'],
  \ }

" }}}

" autocmd {{{
:autocmd BufWrite * :Neomake

autocmd BufNewFile,BufRead *.ejs set filetype=html
autocmd BufNewFile,BufRead *.jade set filetype=jade

autocmd BufNewFile,BufRead *.less set filetype=less

autocmd BufNewFile,BufRead *.tex setlocal spell
autocmd BufNewFile,BufRead *.md setlocal spell

autocmd FileType notes setlocal textwidth=120
autocmd FileType notes setlocal colorcolumn=0

" Automatically set nopaste when exiting insert mode
autocmd InsertLeave * set nopaste

" Automatically resize splits when window is resized
autocmd VimResized * exe "normal! \<c-w>="

autocmd FileType javascript autocmd BufWritePre <buffer> StripWhitespace

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

" misc {{{
" ========

" Unwraps a block of code
function! UnWrap()
    let cursor = getpos( "." )
    :normal $<%
    :normal $%dd
    call setpos( ".", cursor )
    :normal dd
endfunction

command! -nargs=+ Silent execute 'silent <args>' | redraw!
" }}}

" GUI-specific {{{
" ================

if has("gui_running")
   " Remove Toolbar
   set guioptions-=T
endif

" }}}

" tmux-specific {{{
" =================

" set up vimtux' variables so that target always points to first pane of first
" window of open session
if $TMUX != ''
  let g:vimtux = {}
  let session = system( "tmux display-message -p '#S'" )
  let g:vimtux['session'] = strpart( session, 0, len(session) - 1 )
  let g:vimtux['window'] = '0'
  let g:vimtux['pane'] = '0'
endif

" }}}

" source {{{
" Use vim's abbreviations file directly
if filereadable( $HOME.'/.vimrc.abbreviations' )
  source ~/.vimrc.abbreviations
endif

if filereadable( '.vimrc.abbreviations' )
  source .vimrc.abbreviations
endif

" If ~/.nvimrc.local exists, source it to support host-local configs
if filereadable( $HOME.'/.nvimrc.local' )
  source ~/.nvimrc.local
endif

" If .nvimrc.local exists in current directory, to support project-local configs
if filereadable( ".nvimrc.local" )
  source .nvimrc.local
endif
" }}}
