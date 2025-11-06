#!/usr/bin/env bash
# ~/.utils/conda-helper.sh
# Store or retrieve Conda environments for tmux project paths

TMUX_CONDA_MAP="${XDG_CACHE_HOME:-$HOME/.cache}/tmux-conda-map"
mkdir -p "$(dirname "$TMUX_CONDA_MAP")"
touch "$TMUX_CONDA_MAP"

CMD="$1"
PROJECT_PATH="$(realpath "$2" 2>/dev/null)"
ENV_NAME="$3"

case "$CMD" in
  set)
    grep -v "^${PROJECT_PATH}|" "$TMUX_CONDA_MAP" > "${TMUX_CONDA_MAP}.tmp"
    echo "${PROJECT_PATH}|${ENV_NAME}" >> "${TMUX_CONDA_MAP}.tmp"
    mv "${TMUX_CONDA_MAP}.tmp" "$TMUX_CONDA_MAP"
    ;;
  get)
    grep "^${PROJECT_PATH}|" "$TMUX_CONDA_MAP" | cut -d'|' -f2
    ;;
  list)
    cat "$TMUX_CONDA_MAP"
    ;;
  clear)
    > "$TMUX_CONDA_MAP"
    ;;
  *)
    echo "Usage: $0 {set|get|list|clear} [path] [env]"
    ;;
esac
