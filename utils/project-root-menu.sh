#!/usr/bin/env bash
set -eu

CACHE_FILE="${XDG_CACHE_HOME:-$HOME/.cache}/tmux-projects-root"
mkdir -p "$(dirname "$CACHE_FILE")"

MENU=$(cat <<EOF
Add new project
Remove a project
Clear all projects
EOF
)

CHOICE=$(echo "$MENU" | fzf --prompt="Select action: ")

case "$CHOICE" in
    "Add new project")
    NEWROOT=$(find ~ -type d -maxdepth 3 2>/dev/null | fzf --prompt="Select project root: ")
	
    if [[ -d "$NEWROOT" ]]; then
      grep -qxF "$NEWROOT" "$CACHE_FILE" 2>/dev/null || echo "$NEWROOT" >>"$CACHE_FILE"
      tmux display-message "Added: $NEWROOT"
    else
      tmux display-message "Invalid path: $NEWROOT"
    fi
    ;;
  "Remove a project")
    ROOT=$(cat "$CACHE_FILE" | fzf --prompt="Select root to remove: ")
    [[ -n "$ROOT" ]] && grep -vxF "$ROOT" "$CACHE_FILE" > "${CACHE_FILE}.tmp" && mv "${CACHE_FILE}.tmp" "$CACHE_FILE" && echo "Removed: $ROOT"
    ;;
  "Clear all projects")
    > "$CACHE_FILE"
    tmux display-message "Cleared all roots."
    ;;
esac
