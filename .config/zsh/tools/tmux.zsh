if [[ -n "$TMUX" ]]; then
  ENV_NAME="$($HOME/utils/conda-helper.sh get "$(pwd)" 2>/dev/null)"
  [[ -n "$ENV_NAME" ]] && conda activate "$ENV_NAME" 2>/dev/null
fi
