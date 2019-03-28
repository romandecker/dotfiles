docker-cleanup() {

    local interactive=""
    local force=""

    while [[ $# -gt 0 ]]
    do
        key="$1"

        case $key in
            -h|--help)
                echo "Usage: $0 ..."
                ;;
            -i|--interactive)
                interactive=1
                ;;

            -f|--force)
                force='-f'
                ;;
            *)
                echo "Unknown option: $1"
                exit 1
                ;;
        esac
        shift
    done



    if [ -z "$interactive" ]; then
        docker rmi $force $(docker images -q -f dangling=true)
    else
        local images=$(docker images \
                           | tail -n +2 \
                           | fzf -m \
                                 --reverse \
                                 --header "Select images to delete with TAB, ENTER to confirm" \
                                 --bind "ctrl-a:select-all,ctrl-x:accept" \
                           | awk '{ print $3 }')

        for image in $(echo $images); do
            echo docker rmi $force $image
            docker rmi $force $image
        done
    fi
}
