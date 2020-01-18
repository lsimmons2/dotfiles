# make command line prompt the current path
PS1='\w\$ '

# make directories colored when shown with ls
LS_COLORS=$LS_COLORS:'di=01;35:';
export LS_COLORS

alias ls="ls --color=auto"
alias grep="grep --color=auto -i"
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

tb_dir(){
    tensorboard --logdir="$1"
}
alias tb="tb_dir"

# nvm
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh" # This loads nvm

PATH=$PATH:/usr/local/cuda-10.1/bin

PATH=$PATH:/usr/local/go/bin
GOPATH=$HOME/go
GOBIN=$GOPATH/bin
PATH=$PATH:$GOBIN

xmodmap ~/.Xmodmap
