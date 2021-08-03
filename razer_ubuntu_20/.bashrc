# make command line prompt the current path
PS1='\w\$ '

alias ls="ls --color=auto"
alias ll="ls -lha"
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
alias sb="source ~/.bashrc"
alias wn="watch -n 1 nvidia-smi"
alias m="make"
alias op="open_pdfs"


open_pdfs(){
	for var in "$@"
	do
		xdg-open "$var"
	done
}

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


_get_dir_fzf() {
   local cmd="fasd -d"
  eval "$cmd" | FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} --reverse --bind=ctrl-z:ignore $FZF_DEFAULT_OPTS $FZF_CTRL_T_OPTS" $(__fzfcmd) -m "$@" | while read -r item; do
    rv=$(echo $item | awk '{print $2}')
    printf '%q' "$rv"
  done
  echo
}
__insert_dir_or_cd() {

	local selected="$(_get_dir_fzf)"
	if [[ -z "${selected// }" ]]; then
		return
	fi

	if [ "$READLINE_LINE" ]; then
		READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}$selected${READLINE_LINE:$READLINE_POINT}"
		READLINE_POINT=$(( READLINE_POINT + ${#selected} ))
	else
		cd $selected
	fi

}

foo(){
	dir="/home/leo/notes"
	cd $dir
}

bind '"\em": "\C-ex\C-u __insert_dir_or_cd\C-m\C-y\C-b\C-d"'

[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

#https://medium.com/@christoph.schranz/set-up-your-own-gpu-based-jupyterlab-e0d45fcacf43
export PATH=/usr/local/cuda/bin${PATH:+:${PATH}}
export LD_LIBRARY_PATH=/usr/local/cuda/lib64:${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}

alias sd="bash /home/leo/dotfiles/razer_ubuntu_20/change_color_scheme.sh dark"
alias sl="bash /home/leo/dotfiles/razer_ubuntu_20/change_color_scheme.sh light"

#my inotify-wait
mi () {
	echo $@
	eval $@
	sleep 0.5
	# -q specified twice on purpose - see man inotifywait
	if [ -d "/home/leo/playground/" ] 
	then
		#cmd="while inotifywait -q -q -r $pwd -e modify; do { echo; echo $@; eval $@; }; done"
		cmd="while inotifywait -q -q --exclude=tags* -e modify -r .; do { echo; echo $@; eval $@; }; done"
	fi
	#echo "$cmd"
	eval "$cmd"
}
