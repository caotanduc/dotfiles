export NVM_DIR="$HOME/.nvm"

# Lazy load nvm - only initialize when nvm/node/npm commands are called
if [[ -d "$NVM_DIR" ]]; then
  # Add nvm's default node to path if it exists (fast)
  [[ -d "$NVM_DIR/versions/node" ]] && export PATH="$NVM_DIR/versions/node/$(ls -t "$NVM_DIR/versions/node" | head -1)/bin:$PATH"

  # Lazy-load nvm when actually needed
  nvm() {
    unset -f nvm
    BREW_PREFIX="${BREW_PREFIX:-$(brew --prefix)}"
    [[ -s "$BREW_PREFIX/opt/nvm/nvm.sh" ]] && source "$BREW_PREFIX/opt/nvm/nvm.sh"
    nvm "$@"
  }
fi
