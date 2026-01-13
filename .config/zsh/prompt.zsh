# ─────────────────────────────────────────────────────────────
# Minimal Vanilla Zsh Prompt (git + virtualenv)
# ─────────────────────────────────────────────────────────────

# Enable colors
autoload -U colors && colors

# Enable prompt command substitution
setopt PROMPT_SUBST

# -------------------------------------------------------------
# preexec — show last command above prompt
# -------------------------------------------------------------
preexec() {
  echo -ne "\033[1A\033[2K\033[1A\033[2K"
  local cmd="${1#"${1%%[![:space:]]*}"}"
  cmd="${cmd%"${cmd##*[![:space:]]}"}"
  [[ -n "$cmd" ]] && echo -e "\n\033[1;3;4;37m$cmd\033[0m"
}

# -------------------------------------------------------------
# Git info (replacement for git_prompt_info)
# -------------------------------------------------------------
autoload -Uz vcs_info

zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git:*' formats '(%b)'
zstyle ':vcs_info:git:*' actionformats '(%b|%a)'

precmd() {
  vcs_info
}

git_prompt_info() {
  [[ -n "$vcs_info_msg_0_" ]] || return

  if git diff --quiet 2>/dev/null && git diff --cached --quiet 2>/dev/null; then
    echo "%{$fg[yellow]%}${vcs_info_msg_0_}%{$reset_color%} "
  else
    echo "%{$fg[yellow]%}${vcs_info_msg_0_}%{$fg[red]%}*%{$reset_color%} "
  fi
}

# -------------------------------------------------------------
# Python virtualenv (replacement for virtualenv_prompt_info)
# -------------------------------------------------------------
virtualenv_prompt_info() {
  [[ -n "$VIRTUAL_ENV" ]] || return
  local venv="${VIRTUAL_ENV:t}"
  echo "%{$fg[blue]%}${venv}!%{$reset_color%} "
}

# -------------------------------------------------------------
# Prompt
# -------------------------------------------------------------
NEWLINE=$'\n'

PROMPT='$(virtualenv_prompt_info)%d $(git_prompt_info)\
${NEWLINE}%(?..%{$fg[red]%}[%?] %{$reset_color%})\
%{$fg[red]%}%(!.#.$)%{$reset_color%} '
