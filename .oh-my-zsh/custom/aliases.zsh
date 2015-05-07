alias 'new-project'='tmuxifier ns'
alias 'edit-project'='tmuxifier es'
alias project='tmuxifier s'

if [[ `uname` == 'Darwin' ]]
then
    # make sure to use macvim here, as it plays better with OSX
    alias vim='mvim -v'
    alias vimdiff='mvimdiff -v'
fi
