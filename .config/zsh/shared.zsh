if [[ -n "$TMUX" ]]; then
  # ─── Minimal Prompt for tmux ─────────────────────────────
  ZSH_THEME="tmux-clean"
else
  # ─── Normal prompt when NOT in tmux ───────────────────────
  ZSH_THEME="kennethreitz"
fi
