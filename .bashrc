source ~/.sensitive.sh
source ~/projects/dotfiles/convenience_commands.sh

# general
PS1='\w\$ ' # make command line prompt the current path
LS_COLORS=$LS_COLORS:'di=32:'; # make directories colored when shown with ls
export LS_COLORS
alias ls="ls --color=auto"
alias grep="grep --color=auto"
alias h="history"
alias tree="tree -I 'node_modules|venv|env|*.pyc'"
alias g="git"
alias p="python"

# custom aliases
alias vea="source venv/bin/activate"
alias ved="deactivate"
alias nmp="nodemon --exec python"
alias nm="nodemon --exec"

# misc
export NVM_DIR="$HOME/.nvm" # nvm
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh" # This loads nvm

