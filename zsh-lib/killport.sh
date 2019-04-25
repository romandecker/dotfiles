killport() (
    local array=( $@ )
    local len=${#array[@]}
    local port=${array[$len]}
    local options=${array[@]:0:$len-1}

    local pid=$(lsof -sTCP:Listen -ti:$port)

    local cmd="kill $options $pid"

    echo "$cmd"
    eval $cmd
)
