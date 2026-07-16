qr() {
    local ip=$(ipconfig getifaddr en0)
    local url=http://$ip
    local port=$1

    if [ ! -z $port ]; then
        url=$url:$port
    fi

    npx qrcode-terminal $url
    echo $url
}
