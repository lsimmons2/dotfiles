# make command line prompt the current path

#function git_branch {
  #branch=$(git rev-parse --abbrev-ref HEAD 2>/dev/null)
  #if [ "$branch" ]; then
    #echo "($branch)"
  #fi
#}
#function git_branch {
  #git rev-parse --abbrev-ref HEAD 2>/dev/null
#}

#export PROMPT='%~%b %# '
#export PS1='%~ $(git_branch) %# '

#export PS1='%n@%m:%~$(git_branch | awk '{print " ("$0")"}') %# '

# Load version control information
autoload -Uz vcs_info
precmd() { vcs_info }

# Format the vcs_info_msg_0_ variable
zstyle ':vcs_info:git:*' formats '(%b) '
setopt PROMPT_SUBST
PROMPT='${PWD/#$HOME/~} ${vcs_info_msg_0_}%# '

#PS1='%~ %F{purple}xxx%{$reset_color%} \$ '
#PS1='%n@%m:%~ %{$fg[magenta]%}[%D{%H:%M:%S}]%{$reset_color%} %F{purple}xxx%{$reset_color%}\$ '

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
eval "$(fasd --init auto)"
#eval "$(fasd --init bash-hook bash-ccomp bash-ccomp-install)"


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
#piping to /dev/null from https://superuser.com/a/1490101/762039
bind -x '"\ev": __find_docker_container' 2>/dev/null
bind -x '"\ec": __find_docker_container' 2>/dev/null


__fzfcmd() {
  [ -n "$TMUX_PANE" ] && { [ "${FZF_TMUX:-0}" != 0 ] || [ -n "$FZF_TMUX_OPTS" ]; } &&
    echo "fzf-tmux ${FZF_TMUX_OPTS:--d${FZF_TMUX_HEIGHT:-40%}} -- " || echo "fzf"
}

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

bind '"\em": "\C-ex\C-u __insert_dir_or_cd\C-m\C-y\C-b\C-d"' 2>/dev/null

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

#for Makefile tab completion in bash:
#https://stackoverflow.com/a/38415982/6058175
#complete -W "\`grep -oE '^[a-zA-Z0-9_.-]+:([^=]|$)' ?akefile | sed 's/[^a-zA-Z0-9_.-]*$//'\`" make
#
#
#unbindkey "^Y"

#bindkey "^Y" " "
#bindkey -r "^[y"

#bindkey -s "^[y" ''
#bindkey -s "^Y" ""
#
#

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/leo/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/leo/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/leo/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/Users/leo/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<
export PATH="/opt/homebrew/opt/postgresql@16/bin:$PATH"

[ -f "/Users/leo/.ghcup/env" ] && . "/Users/leo/.ghcup/env" # ghcup-env
