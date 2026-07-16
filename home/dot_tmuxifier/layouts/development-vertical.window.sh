# Set window root path. Default is `$session_root`.
# Must be called before `new_window`.
#window_root "~/Projects/Development"

# Create new window. If no argument is given, window name will be based on
# layout file name.
new_window "development"

# Split window into panes.
split_v 24

# Run commands.
run_cmd "vim" 0     # runs in active pane

# Set active pane.
select_pane 0
