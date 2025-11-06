# ─── Minimal Prompt (no path, keeps git + virtualenv) ─────────────────────

preexec() {
  echo -ne "\033[1A\033[2K\033[1A\033[2K"
  local cmd="${1#"${1%%[![:space:]]*}"}"
  cmd="${cmd%"${cmd##*[![:space:]]}"}"
  [[ -n "$cmd" ]] && echo -e "\033[1;3;4;37m$cmd\033[0m"
}

NEWLINE=$'\n'

PROMPT='$(virtualenv_prompt_info)$(git_prompt_info)\
${NEWLINE}%(?..%{$fg[red]%}[%?] %{$reset_color%})\
%{$fg[red]%}%(!.#.$)%{$reset_color%} '

# Git info appearance
ZSH_THEME_GIT_PROMPT_PREFIX="%{$reset_color%}%{$fg[yellow]%}("
ZSH_THEME_GIT_PROMPT_SUFFIX=")%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_CLEAN=""
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%}*%{$fg[yellow]%}"

# Python virtualenv info appearance
ZSH_THEME_VIRTUAL_ENV_PROMPT_PREFIX="%{$reset_color%}%{$fg[blue]%}"
ZSH_THEME_VIRTUAL_ENV_PROMPT_SUFFIX="!%{$reset_color%} "
