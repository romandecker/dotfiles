session_root "~/projects/{{SESSION_NAME}}"

if initialize_session "{{SESSION_NAME}}"; then

  load_window "development"
fi

finalize_and_go_to_session
