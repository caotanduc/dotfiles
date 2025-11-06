#!/usr/bin/env bash
set -euo pipefail

CACHE_FILE="${XDG_CACHE_HOME:-$HOME/.cache}/tmux-projects-root"
mkdir -p "$(dirname "$CACHE_FILE")"
touch "$CACHE_FILE"

MENU=$(cat <<EOF
Add new project root
Remove a project root
List all project roots
Clear all roots
Cancel
EOF
)

# Use fzf or fallback to select with read
if command -v fzf &>/dev/null; then
  CHOICE=$(echo "$MENU" | fzf --prompt="Select action: ")
else
  echo "$MENU" | nl
  read -rp "Choose an action number: " n
  CHOICE=$(echo "$MENU" | sed -n "${n}p")
fi

case "$CHOICE" in
  "Add new project root")
    read -rp "Enter new project root path: " NEWROOT
    if [[ -d "$NEWROOT" ]]; then
      grep -qxF "$NEWROOT" "$CACHE_FILE" 2>/dev/null || echo "$NEWROOT" >>"$CACHE_FILE"
      echo "Added: $NEWROOT"
    else
      echo "Invalid path: $NEWROOT"
    fi
    ;;
  "Remove a project root")
    ROOT=$(cat "$CACHE_FILE" | fzf --prompt="Select root to remove: ")
    [[ -n "$ROOT" ]] && grep -vxF "$ROOT" "$CACHE_FILE" > "${CACHE_FILE}.tmp" && mv "${CACHE_FILE}.tmp" "$CACHE_FILE" && echo "Removed: $ROOT"
    ;;
  "List all project roots")
    echo "Project roots:"
    cat "$CACHE_FILE" || echo "(none)"
    read -rp "Press ENTER to close..."
    ;;
  "Clear all roots")
    > "$CACHE_FILE"
    echo "Cleared all roots."
    ;;
  "Cancel"|*)
    echo "Cancelled."
    ;;
esac
