# Initialize completion system
autoload -Uz compinit
compinit

# Completion styles
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
