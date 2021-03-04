# make command line prompt the current path
PS1='\w\$ '

alias ls="ls --color=auto"
alias grep="grep --color=auto -i"
alias h="history"
alias tree="tree -I 'node_modules|venv|env|*.pyc'"
alias g="git"
alias p="python"
alias p3="python3"
alias vea="source venv/bin/activate"
alias ved="deactivate"
alias pag="ps aux | grep -v \"grep\" | grep"
alias xxx="xmodmap ~/.Xmodmap"
alias d="docker"
alias kc="kubectl"
alias w1="watch -n1"
alias kubectl="microk8s.kubectl"
alias kc="kubectl"
alias sd="skaffold dev"

# nvm
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh" # This loads nvm

[ -f ~/.fzf.bash ] && source ~/.fzf.bash


FZF_CTRL_J_COMMAND="docker ps"
__fzf_get_docker_container() {
  local cmd="${FZF_CTRL_J_COMMAND:-"command find -L . -mindepth 1 \\( -path '*/\\.*' -o -fstype 'sysfs' -o -fstype 'devfs' -o -fstype 'devtmpfs' -o -fstype 'proc' \\) -prune \
    -o -type f -print \
    -o -type d -print \
    -o -type l -print 2> /dev/null | cut -b3-"}"
  eval "$cmd" | FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} --reverse --bind=ctrl-z:ignore $FZF_DEFAULT_OPTS $FZF_CTRL_T_OPTS" $(__fzfcmd) -m "$@" | while read -r item; do
    rv=$(echo $item | awk '{print $1}')
    printf '%q' "$rv"
  done
  echo
}

__find_docker_container() {
  local selected="$(__fzf_get_docker_container)"
  READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}$selected${READLINE_LINE:$READLINE_POINT}"
  READLINE_POINT=$(( READLINE_POINT + ${#selected} ))
}

#bind -m emacs-standard -x '"\ed": __find_docker_container'
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
