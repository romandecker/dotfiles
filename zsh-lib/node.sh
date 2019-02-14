load_node () {
    (>&2 echo "\e[33mLoading node into PATH...\e[39m")
    
    export NVM_DIR=~/.nvm
    [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"

    export YVM_DIR="$HOME/.yvm"
    [ -s "/usr/local/bin/yvm" ] && . /usr/local/bin/yvm
}

lazy_load_node () {
  # find all binaries registered by node, and replace them with
  # aliases that load nvm and yvm first
  declare -a NODE_GLOBALS=(`find ~/.nvm/versions/node -maxdepth 3 -type l -wholename '*/bin/*' | xargs -n1 basename | sort | uniq`)

  NODE_GLOBALS+=("node")
  NODE_GLOBALS+=("nvm")
  NODE_GLOBALS+=("yvm")

  for cmd in "${NODE_GLOBALS[@]}"; do
      eval "${cmd}(){ unset -f ${NODE_GLOBALS}; load_node; ${cmd} \$@ }"
  done
}

if [[ ! -z "$LOAD_NODE" ]]; then
    load_node
else
    lazy_load_node
fi
