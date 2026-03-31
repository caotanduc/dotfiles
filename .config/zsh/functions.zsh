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

ssm() {
  local profile="${1:-bruno}"
  local region="${2:-ap-southeast-1}"

  # Check if credentials are valid; if not, trigger SSO login
  if ! aws sts get-caller-identity --profile "$profile" &>/dev/null; then
    aws sso login --profile "$profile" || return 1
  fi

  # Get instances as JSON
  local json
  json="$(
    aws ec2 describe-instances \
      --profile "$profile" --region "$region" \
      --filters "Name=tag:allow,Values=ssm" "Name=instance-state-name,Values=running" \
      --output json
  )" || return 1

  # Use jq to build pickable rows, fzf to choose one, then extract instance id
  local target
  target="$(
    jq -r '
      .Reservations[].Instances[]
      | {
          id: .InstanceId,
          name: ((.Tags // []) | map(select(.Key=="Name") | .Value) | .[0] // "-"),
          ip: (.PrivateIpAddress // "-"),
          type: (.InstanceType // "-"),
          az: (.Placement.AvailabilityZone // "-")
        }
      | "\(.id)\t\(.name)\t\(.ip)\t\(.type)\t\(.az)"
    ' <<<"$json" \
    | sort -k2,2 \
    | fzf --with-nth=2.. --delimiter=$'\t' --prompt="SSM target > " \
    | cut -f1
  )"

  [[ -z "$target" ]] && return 1

  aws ssm start-session --profile "$profile" --region "$region" --target "$target"
}

