#!/usr/bin/env bash
CACHE_FILE="${XDG_CACHE_HOME:-$HOME/.cache}/tmux-projects-root"
read -rp "Enter new project root path: " NEWROOT
[[ -d "$NEWROOT" ]] || { echo "Invalid path."; exit 1; }
grep -qxF "$NEWROOT" "$CACHE_FILE" 2>/dev/null || echo "$NEWROOT" >>"$CACHE_FILE"
echo "Added: $NEWROOT"
