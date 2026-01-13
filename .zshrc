# ---- Oh My Zsh ----
export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="tmux-clean"
plugins=(git z zsh-autosuggestions)

source $ZSH/oh-my-zsh.sh

# ---- Load modular config ----
ZSH_CFG="$HOME/.config/zsh"

source $ZSH_CFG/shared.zsh
[[ -f $ZSH_CFG/local.zsh ]] && source $ZSH_CFG/local.zsh

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
