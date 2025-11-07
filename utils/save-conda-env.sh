#!/usr/bin/env bash
set -eu

CURRENT_PATH=$(pwd)
HELPER="$HOME/utils/conda-helper.sh"

ENVS=$(conda env list 2>/dev/null | awk 'NR>2 && NF {print $1}')

if [[ -z "$ENVS" ]]; then
  tmux display-message "No Conda environments found."
  exit 1
fi

SELECTED=$(echo "$ENVS" | fzf --prompt "Conda envs: ")

"$HELPER" set "$CURRENT_PATH" "$SELECTED"
tmux display-message "Saved and activate Conda env '$SELECTED' for '$CURRENT_PATH'"
