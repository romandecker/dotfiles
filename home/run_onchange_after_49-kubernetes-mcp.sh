#!/usr/bin/env bash
# Kubernetes MCP server for Claude Code
# (https://github.com/containers/kubernetes-mcp-server/blob/main/docs/getting-started-claude-code.md).
# Registers it at user scope, so it works in every project. Needs `claude`,
# Node >=18 and jq already present. The server runs read-only, set in
# ~/.config/kubernetes-mcp-server.toml (managed by chezmoi).
# The guide uses a dedicated viewer kubeconfig with a ServiceAccount. That file
# holds cluster credentials, so chezmoi does not manage it. If it is missing,
# the server falls back to the default kubeconfig. Re-run `chezmoi apply` after
# creating ~/.kube/mcp-viewer.kubeconfig to switch to it.
# Remove then add makes the script safe to re-run.
set -eu

if command -v claude >/dev/null 2>&1 && command -v node >/dev/null 2>&1 && command -v jq >/dev/null 2>&1; then
    viewer="$HOME/.kube/mcp-viewer.kubeconfig"

    config=$(jq -n -c \
        --arg toml "$HOME/.config/kubernetes-mcp-server.toml" \
        --arg kubeconfig "$viewer" \
        --argjson use_viewer "$([ -f "$viewer" ] && echo true || echo false)" \
        '{command: "npx", args: ["-y", "kubernetes-mcp-server@latest", "--config", $toml]}
         + (if $use_viewer then {env: {KUBECONFIG: $kubeconfig}} else {} end)')

    claude mcp remove kubernetes-mcp-server -s user >/dev/null 2>&1 || true
    claude mcp add-json kubernetes-mcp-server "$config" -s user
fi
