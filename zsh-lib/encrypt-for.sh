encrypt-for() {
  b64=$(gpg -e --recipient $1 | base64)

  echo "Send this to $1:"
  echo "echo $b64 | base64 -D | gpg -d"
}
