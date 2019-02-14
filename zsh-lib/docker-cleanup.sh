docker-cleanup() {
  docker rmi $(docker images -q -f dangling=true)
}
