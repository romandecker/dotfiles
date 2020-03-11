save_displays() {
    displayplacer list | tail -1 > "$HOME/.displayconfig"
}

restore_displays() {
    [ -f "$HOME/.displayconfig" ] && sh "$HOME/.displayconfig"
}
