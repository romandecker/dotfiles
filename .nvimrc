" vim: set foldmethod=marker

" Plugins {{{
" ===========
call plug#begin('~/.nvim/plugged')

" Libraries
Plug 'xolox/vim-misc'
Plug 'svermeulen/vim-repeat'
Plug 'kana/vim-submode'
Plug 'michaeljsmith/vim-indent-object'   " indent text-object
Plug 'kana/vim-operator-user'
Plug 'Shougo/unite.vim'         " needed by vimfiler

" Motions
Plug 'bkad/CamelCaseMotion'
Plug 'wellle/targets.vim'   " Add additional text-objects
Plug 'kana/vim-textobj-user'    " needed by vim-textobj-xmlattr
Plug 'kana/vim-textobj-function' " function text-objects for C, java, vim (f)
Plug 'kana/vim-textobj-lastpat' " last search with /
Plug 'thinca/vim-textobj-function-javascript' " JS function text-object (f)
Plug 'glts/vim-textobj-comment' " A comment text-object
Plug 'whatyouhide/vim-textobj-xmlattr' " XML/HTML attribute text objects (x)
Plug 'tmhedberg/matchit' " More uses for %
Plug 'ironhouzi/vim-stim' " Better *
Plug 'easymotion/vim-easymotion'
Plug 'jeetsukumaran/vim-indentwise'
Plug 'bronson/vim-visual-star-search'

" Language-specific
Plug 'plasticboy/vim-markdown', { 'for': 'markdown' }
Plug 'shime/vim-livedown', { 'for': 'markdown' }
Plug 'lervag/vimtex', { 'for': 'tex' }
Plug 'ekalinin/Dockerfile.vim', { 'for': 'Dockerfile' }
Plug 'mattn/emmet-vim', { 'for': ['html', 'htm', 'xml'] }
Plug 'digitaltoad/vim-jade', { 'for': 'jade' }
Plug 'jelera/vim-javascript-syntax'
Plug 'DeX3/vim-js-indent'
Plug 'elzr/vim-json', { 'for': 'json' }
Plug 'leafgarland/typescript-vim', { 'for': 'typescript' }
Plug 'kchmck/vim-coffee-script', { 'for': 'coffee' }
Plug 'IN3D/vim-raml', { 'for': 'raml' }
Plug 'aklt/plantuml-syntax'

" File management
Plug 'ctrlpvim/ctrlp.vim'
Plug 'danro/rename.vim'
Plug 'rking/ag.vim'
Plug 'dyng/ctrlsf.vim'
Plug 'Shougo/vimfiler.vim'
Plug 'tpope/vim-projectionist'

" editing/formatting
Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'DeX3/vim-argformat'
Plug 'dhruvasagar/vim-table-mode'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'mjbrownie/swapit'
Plug 'ntpeters/vim-better-whitespace'
Plug 'tommcdo/vim-exchange'

" Visual
Plug 'DeX3/vim-smartresize'
Plug 'bling/vim-airline'
Plug 'ntpeters/vim-airline-colornum'
Plug 'Valloric/MatchTagAlways'

" Colors
Plug 'flazz/vim-colorschemes'   " a lot of basic colorschemes
Plug 'dracula/vim'
Plug 'jnurmine/zenburn'

" Integration
Plug 'christoomey/vim-tmux-navigator'
Plug 'brauner/vimtux'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-obsession' " required for vim-prosession
Plug 'dhruvasagar/vim-prosession'
Plug 'vim-utils/vim-husk'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-eunuch' " Some unix commands as vim commands
Plug 'kopischke/vim-fetch' " open files with line numbers like file.c:22
Plug 'danielbmarques/vim-dialect' " file-local spellcheck-ignores

" Misc
Plug 'benekastah/neomake'
Plug 'editorconfig/editorconfig-vim'
Plug 'sjl/gundo.vim'    " the undo-tree
Plug 'SirVer/ultisnips'
Plug 'moll/vim-bbye'    " close buffers without messing up window layout
Plug 'xolox/vim-notes'

call plug#end()
" }}}

" Colors {{{
" ==========
set t_Co=256

set background=dark
colorscheme aurora
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
" Avoid swapfiles alltogether
set noswapfile

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

" Fold by syntax
set foldmethod=indent

" But unfold everthing when opening a new file, so nothing is folded
set foldlevel=99

" }}}

" Mappings {{{
" ============

let mapleader = " "
let maplocalleader = "\\"

" Toggle paste mode with F2, and show status
nnoremap <silent> <F2> :set invpaste<Bar>echo 'Paste '.(&paste? 'ON':'OFF')<CR>


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

" save with leader f s
nmap <leader>fs :w<CR>
" open current directory with leader f d
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

nnoremap c "_c

nnoremap x "_x
nnoremap <leader>x x

" Open the file name currently under the cursor even if it does not exist (for
" creating a new file). Must use "%:h" to properly get the file path relative
" to the current file
nmap <leader>gf :exe 'e ' . expand("%:h") . '/' . expand("<cfile>")<CR>

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
nmap <leader>w- :split<CR>
nmap <leader>w\| :vsplit<CR>
nmap <leader>wo :only<CR>

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

" Leader-p instead of =p for indented paste
nmap <Leader>p =p

nnoremap <Tab> %

" navigate jumplist
nnoremap [j <C-o>
nnoremap ]j <C-i>

" navigate changelist
nnoremap [c g;
nnoremap ]c g,

" Switch to alternate file via projectionist
nnoremap <Leader>a :A<CR>

" Go to the end of a block with <leader>e
nmap <Leader>e $%
vmap <Leader>e $%

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
let g:neomake_haskell_enabled_makers = ['hlint']

"disable folding for vim-markdown (to prevent everything being folded on open)
let g:vim_markdown_folding_disabled=1

"don't conceal quotes in json files
let g:vim_json_syntax_conceal=0

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

let g:EasyMotion_do_mapping = 0

nmap <leader>fj <Plug>(easymotion-bd-jk)
nmap <leader>fk <Plug>(easymotion-bd-jk)
nmap <leader>fw <Plug>(easymotion-bd-w)
nmap <leader>ff <Plug>(easymotion-bd-f)

" Delete a block
nmap dab $d%dd

let g:AutoPairsShortcutBackInsert = '<M-b>'

let g:vimfiler_as_default_explorer = 1

" }}}

" autocmd {{{
autocmd BufWrite * :Neomake

autocmd BufNewFile,BufRead *.ejs set filetype=html
autocmd BufNewFile,BufRead *.jade set filetype=jade

autocmd FileType less set filetype=less

autocmd FileType markdown,tex,gitcommit setlocal spell
autocmd FileType notes setlocal textwidth=120 colorcolumn=0

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

function! g:PulseCursorLine()
  setlocal cursorline
  redraw
  sleep 75m

  setlocal nocursorline
  redraw
  sleep 75m

  setlocal cursorline
  redraw
  sleep 75m

  setlocal nocursorline
  redraw
endfunction
autocmd FocusGained * call g:PulseCursorLine()

nmap gg gg:call g:PulseCursorLine()<CR>

" create non-existing directories on write
function! s:MkNonExDir(file, buf)
    if empty(getbufvar(a:buf, '&buftype')) && a:file!~#'\v^\w+\:\/'
        let dir=fnamemodify(a:file, ':h')
        if !isdirectory(dir)
            call mkdir(dir, 'p')
        endif
    endif
endfunction
augroup BWCCreateDir
    autocmd!
    autocmd BufWritePre * :call s:MkNonExDir(expand('<afile>'), +expand('<abuf>'))
augroup END
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

if isdirectory( '.vim/snippets' )
  let g:UltiSnipsSnippetDirectories = ['UltiSnips', getcwd() . '/.vim/snippets']
endif
" }}}
