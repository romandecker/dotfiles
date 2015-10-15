
set nocompatible              " be improved, required
filetype off                  " required

call plug#begin('~/.vim/plugged')

Plug 'vim-scripts/genutils'
Plug 'xolox/vim-misc'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'camelcasemotion'
Plug 'digitaltoad/vim-jade', { 'for': 'jade' }
Plug 'plasticboy/vim-markdown', { 'for': 'mkd' }
Plug 'elzr/vim-json', { 'for': 'json' }
Plug 'mattn/emmet-vim'
Plug 'lervag/vimtex'
Plug 'christoomey/vim-tmux-navigator'
Plug 'bling/vim-airline'
Plug 'terryma/vim-multiple-cursors'
Plug 'scrooloose/syntastic'
Plug 'editorconfig/editorconfig-vim'
Plug 'marijnh/tern_for_vim', { 'for': 'javascript' }
Plug 'tpope/vim-surround'
Plug 'svermeulen/vim-repeat'
Plug 'svermeulen/vim-easyclip'
Plug 'jonathanfilip/vim-lucius'
Plug 'justinmk/vim-sneak'
Plug 'matchit.zip'
Plug 'Valloric/YouCompleteMe', { 'do': './install.sh --omnisharp-completer' }
Plug 'kana/vim-submode'
Plug 'sjl/gundo.vim'
Plug 'chreekat/vim-paren-crosshairs'
Plug 'danro/rename.vim'
Plug 'tpope/vim-obsession'
Plug 'dhruvasagar/vim-prosession'
Plug 'tpope/vim-commentary'
Plug 'OmniSharp/omnisharp-vim'
Plug 'tpope/vim-dispatch'   " needed for omnisharp
Plug 'ekalinin/Dockerfile.vim'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'   " some predefined snippets
Plug 'wellle/targets.vim'   " Add additional text-objects
Plug 'glts/vim-textobj-comment' " A comment text-object
Plug 'kana/vim-textobj-user' " needed by vim-textobj-xmlattr
Plug 'whatyouhide/vim-textobj-xmlattr' " XML/HTML attribute text objects (ix, ax)
Plug 'scrooloose/nerdtree'
Plug 'rking/ag.vim'
Plug 'DeX3/vim-smartresize'
Plug 'dyng/ctrlsf.vim'
Plug 'vim-scripts/ScrollColors'
Plug 'flazz/vim-colorschemes'
Plug 'ntpeters/vim-airline-colornum'
Plug 'majutsushi/tagbar'
Plug 'heavenshell/vim-jsdoc'
Plug 'romainl/Apprentice'
Plug 'pangloss/vim-javascript'
Plug 'dhruvasagar/vim-table-mode'
Plug 'moll/vim-bbye'    " close buffers without messing up window layout
Plug 'brauner/vimtux'
Plug 'leafgarland/typescript-vim'
Plug 'tpope/vim-fugitive'
Plug 'cohama/lexima.vim'
Plug 'vim-utils/vim-husk'
Plug 'terryma/vim-smooth-scroll'

call plug#end()

" Colorscheme
if $COLORTERM == 'gnome-terminal'
  set t_Co=256
endif

set background=dark
colorscheme apprentice

syntax on

set encoding=utf8
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

if has("gui_running")
   " Remove Toolbar
   set guioptions-=T
endif

let mapleader = " "
let maplocalleader = "\\"

set pastetoggle=<F2>

" buffer related stuff
nnoremap <leader>n  :enew<CR>
nnoremap <leader>j  :bp<CR>
nnoremap <leader>k  :bn<CR>
nnoremap <leader>q  :Bdelete<CR>    " use command from moll/vim-bbye
nnoremap <leader><S-q>  :bufdo bd<CR>

nnoremap tn :tabnew<CR>
nnoremap tj :tabp<CR>
nnoremap tk :tabn<CR>

" Search mappings: These will make it so that going to the next one in a
" search will center on the line it's found in.
map N Nzz
map n nzz

" Movement in wrapped lines
nmap j gj
nmap k gk
nmap 0 g0
nmap $ g$

nmap <C-s> :w<CR>

" enable camelcasemotion
nmap w ,w
nmap b ,b
nmap e ,e

" m is used by easyclip use gm to create marks instead
nnoremap gm m

" Clear highlighting on escape in normal mode
nnoremap <esc> :noh<return><esc>
" needed so that vim still understands escape sequences
nnoremap <esc>^[ <esc>^[

" use ENTER to enter command mode directly
nnoremap <CR> :

nmap <leader>s :w<CR>

" window-stuff with leader w
nmap <leader>ws :split<CR>
nmap <leader>w<S-s> :vsplit<CR>
nmap <leader>wq <C-w>q

" Toggle NERDTree with leader w t
nmap <leader>wt :NERDTreeToggle<CR>

" vim-sneak's mappings get overwritten by vim-easyclip, so re-instate them
nmap s <Plug>Sneak_s
nmap S <Plug>Sneak_S

nmap f <Plug>Sneak_f
nmap F <Plug>Sneak_F

nmap t <Plug>Sneak_t
nmap T <Plug>Sneak_T

nmap g/ <Plug>CtrlSFPrompt

imap <C-l> <Del>

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

nmap <Leader>tu :call UnWrap()<CR>

" comment/uncomment lines
nmap <Leader>tcc gcc
vmap <Leader>tc gc

" Use C-p to duplicate a block of code in visual mode
vmap <Leader>db y`>p

"press gp to reselect pasted text
nnoremap <expr> gp '`[' . strpart(getregtype(), 0, 1) . '`]'

" map M to work just like D used to work without easyclip
nmap <S-m> v$hm

nmap <S-u> :GundoToggle<CR>

nmap <Leader><Leader> :call ExecuteKeys( 'Up Enter' )<CR>

" duplicate the above block with <leader>db
nnoremap <Leader>db mp?)\\|]\\|}<CR><S-v>%y`pp:nohl<CR>

noremap <silent> <c-u> :call smooth_scroll#up(&scroll, 0, 2)<CR>
noremap <silent> <c-d> :call smooth_scroll#down(&scroll, 0, 2)<CR>

"Enable emmet only for html-ish files
let g:user_emmet_install_global = 0
autocmd FileType html,css,xml EmmetInstall
au BufNewFile,BufRead *.ejs set filetype=html
au BufNewFile,BufRead *.jade set filetype=jade

au BufNewFile,BufRead *.less set filetype=less

" Unwraps a block of code
function! UnWrap()
    let cursor = getpos( "." )
    :normal $<%
    :normal $%dd
    call setpos( ".", cursor )
    :normal dd
endfunction

" Enable relative numbers for currently focused window
set number
if has('autocmd')
augroup vimrc_linenumbering
    autocmd!
    autocmd WinLeave *
                \ if &number |
                \   set norelativenumber |
                \ endif
    autocmd BufWinEnter *
                \ if &number |
                \   set relativenumber |
                \ endif
    autocmd VimEnter *
                \ if &number |
                \   set relativenumber |
                \ endif
augroup END
endif

" Automatically set nopaste when exiting insert mode
autocmd InsertLeave * set nopaste

" Automatically resize splits when window is resized
autocmd VimResized * exe "normal! \<c-w>="

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

let g:ycm_autoclose_preview_window_after_completion = 1

" make YCM compatible with UltiSnips (using supertab)
let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
let g:SuperTabDefaultCompletionType = '<C-n>'

" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"

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

" html checker doesn't know html5...
let g:syntastic_html_checkers = []

let g:syntastic_javascript_checkers = ['jshint', 'jscs']

let g:syntastic_error_symbol = "✗"
let g:syntastic_warning_symbol = "⚠"
let g:syntastic_always_populate_loc_list = 1

"disable folding for vim-markdown (to prevent everything being folded on open)
let g:vim_markdown_folding_disabled=1

"don't conceal quotes in json files
let g:vim_json_syntax_conceal=0

let g:EditorConfig_verbose = 1
let g:EditorConfig_exclude_patterns = ['fugitive://.*']

" share easyclip yanks between sessions
let g:EasyClipShareYanks = 1

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

call lexima#add_rule( { 'char': ')', 'at': '\%#\s*)', 'leave': ')' } )
call lexima#add_rule( { 'char': ']', 'at': '\%#\s*]', 'leave': ']' } )
call lexima#add_rule( { 'char': '}', 'at': '\%#\s*}', 'leave': '}' } )

command! -nargs=+ Silent execute 'silent <args>' | redraw!

" set up vimtux' variables so that target always points to first pane of first
" window of open session
if $TMUX != ''
  let g:vimtux = {}
  let session = system( "tmux display-message -p '#S'" )
  let g:vimtux['session'] = strpart( session, 0, len(session) - 1 )
  let g:vimtux['window'] = '0'
  let g:vimtux['pane'] = '0'
endif

" If ~/.vimrc.local exists, source it to support host-local configs
if filereadable( $HOME.'/.vimrc.local' )
    source ~/.vimrc.local
endif

" If .vimrc.local exists in current directory, to support project-local configs
if filereadable( ".vimrc.local" )
    source .vimrc.local
endif
