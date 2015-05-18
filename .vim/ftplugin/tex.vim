" buffer-local settings for tex files

let &makeprg='docker run --rm -i
\ -v $PWD:/data theromanempire/latex
\ latexmk %'

nmap <F9> :Make<CR>

setlocal spell spelllang=en_us

set textwidth=80 " to enable auto-linebreaks
