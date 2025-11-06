#!/usr/bin/env bash
set -euo pipefail

LOGFILE="${XDG_CACHE_HOME:-$HOME/.cache}/tmux-conda-debug.log"
exec >>"$LOGFILE" 2>&1
echo "[INFO] ---------- $(date) ----------"

CURRENT_PATH=$(pwd)
HELPER="$HOME/utils/conda-helper.sh"
echo "[INFO] Path: $CURRENT_PATH"

# ─────────────────────────────────────────────
# 1. Get all conda environments
# ─────────────────────────────────────────────
ENVS=$(conda env list 2>/dev/null | awk 'NR>2 && NF {print $1}')

if [[ -z "$ENVS" ]]; then
  echo "⚠️  No Conda environments found."
  exit 1
fi

# ─────────────────────────────────────────────
# 2. Let user select one
# ─────────────────────────────────────────────
SELECTED=$(echo "$ENVS" | fzf)

# ─────────────────────────────────────────────
# 3. Save mapping (project path → selected env)
# ─────────────────────────────────────────────
"$HELPER" set "$CURRENT_PATH" "$SELECTED"
tmux display-message "✅ Saved and activate Conda env '$SELECTED' for '$CURRENT_PATH'"
