import os
from tmux_super_fingers.targets.file_target import FileTarget
from tmux_super_fingers.actions.send_to_vim_in_tmux_pane_action import SendToVimInTmuxPaneAction
from tmux_super_fingers.actions.action import Action
from tmux_super_fingers.targets.target_payload import EditorOpenable

class SendToVsCodeAction(Action):
    def __init__(self, target_payload: EditorOpenable):
        self.target_payload = target_payload

    def perform(self):
        path = self.target_payload.file_path
        if self.target_payload.line_number:
            # VS Code --goto supports "file:line" (and also "file:line:column")
            path += f":{self.target_payload.line_number}"

        os.system(f'code -g "{path}"')

# Make VS Code the primary action for files
FileTarget.primary_action = SendToVsCodeAction
# Keep vim as secondary (space toggles secondary mode)
FileTarget.secondary_action = SendToVimInTmuxPaneAction
