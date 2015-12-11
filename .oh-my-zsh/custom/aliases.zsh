# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
#
alias 'new-project'='tmuxifier ns'
alias 'edit-project'='tmuxifier es'
alias 'list-projects'='tmuxifier ls'
alias 'grep-projects'='tmuxifier ls | grep -i'
alias project='tmuxifier s'

if [[ `uname` == 'Darwin' ]]
then
    # make sure to use macvim here, as it plays better with OSX
    alias vim='nvim'
    alias vimdiff='nvimdiff -d'
fi

killport() {
  pid=$(lsof -i :$1 | tail -n+2 | head -1 | awk '{ print $2 }')

  kill -9 $pid
}
