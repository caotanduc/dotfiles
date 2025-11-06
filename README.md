TODO

```bash
[[ -f ~/.config/zsh/shared.zsh ]] && source ~/.config/zsh/shared.zsh

# Auto activate Conda env in tmux
if [[ -n "$TMUX" ]]; then
  ENV_NAME="$($HOME/utils/conda-helper.sh get "$(pwd)" 2>/dev/null)"
  if [[ -n "$ENV_NAME" ]]; then
    conda activate "$ENV_NAME" 2>/dev/null
  fi
fi

```