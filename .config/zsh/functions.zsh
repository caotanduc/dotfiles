fcd() {
  local dir
  dir=$(fd --type d --hidden --exclude .git | fzf) || return
  cd "$dir"
}

fh() {
  print -rl -- ${(u)history} | fzf | sed 's/^ *[0-9]* *//' | xargs -r zsh -c
}

fkill() {
  ps -ef | sed 1d | fzf | awk '{print $2}' | xargs kill -9
}
