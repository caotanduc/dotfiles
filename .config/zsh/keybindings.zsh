bindkey -e

autoload edit-command-line
zle -N edit-command-line
bindkey '^Xe' edit-command-line

bindkey '^R' fzf-history-widget
bindkey '^T' fzf-file-widget
bindkey '^[c' fcd    # Alt+c
