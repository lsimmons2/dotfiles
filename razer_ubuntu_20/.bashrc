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

#fzf
[ -f ~/.fzf.bash ] && source ~/.fzf.bash

#fasd
eval "$(fasd --init bash-hook bash-ccomp bash-ccomp-install)"


__fzf_get_docker_container() {
  local cmd="docker ps"
  eval "$cmd" | FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} --reverse --bind=ctrl-z:ignore $FZF_DEFAULT_OPTS $FZF_CTRL_T_OPTS" $(__fzfcmd) -m "$@" | while read -r item; do
    rv=$(echo $item | awk '{print $1}')
    printf '%q' "$rv"
  done
  echo
}
__find_docker_container() {
	local selected="$(__fzf_get_docker_container)"
	local containerId=${selected%:*}

	if [[ -z "${containerId// }" ]]; then
		return
	fi

	if [ "$READLINE_LINE" ]; then
		READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}$containerId${READLINE_LINE:$READLINE_POINT}"
		READLINE_POINT=$(( READLINE_POINT + ${#containerId} ))
	else
		docker exec -it $containerId bash
	fi
}
bind -x '"\ev": __find_docker_container'


__directory_function() {
   local cmd="fasd -d"
  eval "$cmd" | FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} --reverse --bind=ctrl-z:ignore $FZF_DEFAULT_OPTS $FZF_CTRL_T_OPTS" $(__fzfcmd) -m "$@" | while read -r item; do
    rv=$(echo $item | awk '{print $2}')
    printf '%q' "$rv"
  done
  echo
}
__directory_thing() {

	local selected="$(__directory_function)"
	if [[ -z "${selected// }" ]]; then
		return
	fi

	if [ "$READLINE_LINE" ]; then
		#echo something
		READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}$selected${READLINE_LINE:$READLINE_POINT}"
		READLINE_POINT=$(( READLINE_POINT + ${#selected} ))
	else
		#echo nothing
		cd $selected
	fi

	#cd "$thing"
	#printf 'cd %q' "$thing"

}

bind -x '"\em": __directory_thing'


[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

#https://medium.com/@christoph.schranz/set-up-your-own-gpu-based-jupyterlab-e0d45fcacf43
export PATH=/usr/local/cuda/bin${PATH:+:${PATH}}
export LD_LIBRARY_PATH=/usr/local/cuda/lib64:${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}
