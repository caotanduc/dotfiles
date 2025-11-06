#!/usr/bin/env bash
set -euo pipefail

CACHE_DIR="${XDG_CACHE_HOME:-$HOME/.cache}"
ROOT_CACHE="$CACHE_DIR/tmux-projects-root"
LOGFILE="$CACHE_DIR/tmux-project-switcher.log"
HELPER="$HOME/utils/conda-helper.sh"

exec >>"$LOGFILE" 2>&1
echo "[INFO] ---------- $(date) ----------"

mkdir -p "$CACHE_DIR"

# ─────────────────────────────────────────────
# 1. Ensure at least one root exists
# ─────────────────────────────────────────────
if [[ ! -s "$ROOT_CACHE" ]]; then
  echo "No project roots configured."
  read -rp "Enter full path to a project root: " NEWROOT
  [[ -d "$NEWROOT" ]] || { echo "Invalid path."; exit 1; }
  echo "$NEWROOT" >"$ROOT_CACHE"
fi

# ─────────────────────────────────────────────
# 2. Choose which root to use (if multiple)
# ─────────────────────────────────────────────
ROOT_COUNT=$(wc -l <"$ROOT_CACHE")
if (( ROOT_COUNT > 1 )); then
  if command -v fzf &>/dev/null; then
    ROOT=$(fzf --prompt="Select project root: " <"$ROOT_CACHE")
  else
    cat -n "$ROOT_CACHE"
    read -rp "Choose root number: " n
    ROOT=$(sed -n "${n}p" "$ROOT_CACHE")
  fi
else
  ROOT=$(head -n1 "$ROOT_CACHE")
fi

[[ -z "$ROOT" ]] && exit 0
cd "$ROOT" || { echo "Cannot cd to $ROOT"; exit 1; }

# ─────────────────────────────────────────────
# 3. Choose project from selected root
# ─────────────────────────────────────────────
PROJECT=$(find . -maxdepth 1 -type d ! -name '.' | sed 's|^\./||' | fzf --prompt="Select project: ")
[[ -z "$PROJECT" ]] && exit 0

PROJECT_PATH="$ROOT/$PROJECT"
SESSION_NAME="$(basename "$PROJECT")"

if tmux has-session -t "$SESSION_NAME" 2>/dev/null; then
  tmux switch-client -t "$SESSION_NAME"
else
  tmux new-session -d -s "$SESSION_NAME" -c "$PROJECT_PATH"
  tmux switch-client -t "$SESSION_NAME"
fi

tmux display-message "Opened project '$PROJECT' at '$ROOT'"
