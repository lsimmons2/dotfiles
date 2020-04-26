parse_git_branch() {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}
PS1='\w\[\033[01;35m\] $(parse_git_branch)\[\033[00m\]\$ '

# make directories colored when shown with ls
LS_COLORS=$LS_COLORS:'di=01;35:';
export LS_COLORS

alias ls="ls --color=auto"
alias grep="grep --color=auto -i"
alias grepp="grep --color=auto --binary-files=without-match --exclude-dir=venv --exclude-dir=node_modules --exclude-dir=bower_components --exclude-dir=.git --exclude-dir=.cache --exclude-dir=.config --exclude-dir=.nvm --exclude-dir=.npm --exclude-dir=.local --exclude='*.osp'"
alias h="history"
alias tree="tree -I 'node_modules|venv|env|*.pyc'"
alias g="git"
alias p="python"
alias p3="python3"
alias vea="source venv/bin/activate"
alias ved="deactivate"
alias nmp="nodemon --exec python"
alias pag="ps aux | grep -v \"grep\" | grep"
alias xxx="xmodmap ~/.Xmodmap"
alias d="docker"
alias kc="kubectl"
alias w1="watch -n1"
alias kubectl="microk8s.kubectl"
alias kc="kubectl"
alias sd="skaffold dev"
alias wk="watch -n1 microk8s.kubectl"
alias pkr="watch -n1 \"echo DEPLOYMENTS && kubectl get deploy && echo && echo PODS && kubectl get po && echo && echo SERVICES && kubectl get svc\""
alias dps="watch -n1 'docker ps --format \"table {{.ID}} \t{{.Image}} \t{{.Labels}} \t{{.RunningFor}} \t{{.Status}}\"'"
alias study="python3 /home/leo/org/cards/client/study.py"
alias sc="python3 /home/leo/org/cards/client/save_cards.py"
alias scb="python3 /home/leo/org/cards/client/save_cards.py /home/leo/org/notes/blah.txt"

tb_dir(){
  tensorboard --logdir="$1"
}
alias tb="tb_dir"

# nvm
export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm

PATH=$PATH:/usr/local/cuda-10.1/bin

PATH=$PATH:/usr/local/go/bin
GOPATH=$HOME/go
GOBIN=$GOPATH/bin
PATH=$PATH:$GOBIN

xmodmap ~/.Xmodmap

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'

