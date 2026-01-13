if command -v fzf >/dev/null; then
  # Cache brew prefix to avoid slow lookup
  BREW_PREFIX="${BREW_PREFIX:-$(brew --prefix)}"

  [[ -f "$BREW_PREFIX/opt/fzf/shell/key-bindings.zsh" ]] && source "$BREW_PREFIX/opt/fzf/shell/key-bindings.zsh"
  [[ -f "$BREW_PREFIX/opt/fzf/shell/completion.zsh" ]] && source "$BREW_PREFIX/opt/fzf/shell/completion.zsh"

  export FZF_DEFAULT_COMMAND='rg --files --hidden --glob "!.git/*"'
  export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
  export FZF_CTRL_T_OPTS="
    --preview 'bat --style=numbers --color=always {}'
  "
fi
