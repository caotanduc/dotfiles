# ─── Minimal Prompt (no path, keeps git + virtualenv) ─────────────────────
PROMPT='$(virtualenv_prompt_info)$(git_prompt_info)\
%(?..%{$fg[red]%}[%?] %{$reset_color%})\
%{$fg[red]%}%(!.#.»)%{$reset_color%} '

# Git info appearance
ZSH_THEME_GIT_PROMPT_PREFIX="%{$reset_color%}%{$fg[yellow]%}("
ZSH_THEME_GIT_PROMPT_SUFFIX=")%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_CLEAN=""
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%}*%{$fg[yellow]%}"

# Python virtualenv info appearance
ZSH_THEME_VIRTUAL_ENV_PROMPT_PREFIX="%{$reset_color%}%{$fg[blue]%}"
ZSH_THEME_VIRTUAL_ENV_PROMPT_SUFFIX="!%{$reset_color%} "
