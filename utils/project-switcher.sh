#!/usr/bin/env bash
set -eu

CACHE_DIR="${XDG_CACHE_HOME:-$HOME/.cache}"
ROOT_CACHE="$CACHE_DIR/tmux-projects-root"
HELPER="$HOME/utils/conda-helper.sh"

mkdir -p "$CACHE_DIR"

if [[ ! -s "$ROOT_CACHE" ]]; then
  ROOT=$(find ~ -type d -maxdepth 3 2>/dev/null | fzf --prompt="Select project root: ")
  echo "$ROOT" >"$ROOT_CACHE"
else
  ROOT=$(fzf --prompt="Select project root: " <"$ROOT_CACHE")
fi

[[ -z "$ROOT" ]] && exit 0

SESSION_NAME="$(basename "$ROOT")"

if ! tmux has-session -t "$SESSION_NAME" 2>/dev/null; then
  tmux new-session -d -s "$SESSION_NAME" -c "$ROOT"
fi

tmux switch-client -t "$SESSION_NAME"
