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
alias w1="watch -n1"
alias kc="kubectl"
alias sb="source ~/.bashrc"
alias wn="watch -n 1 nvidia-smi"

#for Makefile tab completion in bash:
#https://stackoverflow.com/a/38415982/6058175
complete -W "\`grep -oE '^[a-zA-Z0-9_.-]+:([^=]|$)' ?akefile | sed 's/[^a-zA-Z0-9_.-]*$//'\`" make

export PATH=/usr/local/cuda-12.0/bin${PATH:+:${PATH}}
export LD_LIBRARY_PATH=/usr/local/cuda-12.0/lib64${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}
