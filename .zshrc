# ---- Load modular config ----
ZSH_CFG="$HOME/.config/zsh"

source $ZSH_CFG/shared.zsh
[[ -f $ZSH_CFG/local.zsh ]] && source $ZSH_CFG/local.zsh
