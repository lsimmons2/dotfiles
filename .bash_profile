
# make command line prompt the current path
PS1='\w\$ '

# make directories colored when shown with ls
export LSCOLORS=GxFxCxDxBxegedabagaced
alias ls="ls -G"

# nvm
export NVM_DIR="/Users/leosimmons/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

# mysql
export PATH=$PATH:/usr/local/mysql/bin

