killport() {
  pid=$(lsof -i :$1 | tail -n+2 | head -1 | awk '{ print $2 }')

  kill -9 $pid
}
