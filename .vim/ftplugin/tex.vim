" buffer-local settings for tex files

let &makeprg='docker run --rm -i
\ -v $PWD:/data theromanempire/latex
\ /bin/sh -c "bibtex %:r && pdflatex --synctex=1 % && pdflatex --synctex=1 %"'

nmap <F9> :Make<CR>

setlocal spell spelllang=en_us

set textwidth=80 " to enable auto-linebreaks
