# ========================
# Core
# ========================
export EDITOR="emacsclient -nw"
export TERM="xterm-256color"

# ========================
# Files
# ========================
source $ZSH_CFG/prompt.zsh
source $ZSH_CFG/completion.zsh
source $ZSH_CFG/options.zsh
source $ZSH_CFG/envs.zsh
source $ZSH_CFG/functions.zsh
source $ZSH_CFG/keybindings.zsh
source $ZSH_CFG/paths.zsh
source $ZSH_CFG/aliases.zsh

# ========================
# Tools
# ========================
source $ZSH_CFG/tools/zsh-autosuggestions.zsh
source $ZSH_CFG/tools/zoxide.zsh
source $ZSH_CFG/tools/conda.zsh
source $ZSH_CFG/tools/nvm.zsh
source $ZSH_CFG/tools/bun.zsh
source $ZSH_CFG/tools/pnpm.zsh
source $ZSH_CFG/tools/fzf.zsh

# ========================
# TMUX helpers
# ========================
source $ZSH_CFG/tools/tmux.zsh
