
# make command line prompt the current path
PS1='\w\$ '

# make directories colored when shown with ls
LS_COLORS=$LS_COLORS:'di=32:';
export LS_COLORS

alias ls="ls --color=auto"
alias grep="grep --color=auto --binary-files=without-match --exclude-dir=venv --exclude-dir=node_modules --exclude-dir=bower_components --exclude-dir=.git --exclude-dir=.cache --exclude-dir=.config --exclude-dir=.nvm --exclude-dir=.npm --exclude-dir=.local --exclude='*.osp'"
alias h="history"
alias tree="tree -I 'node_modules|venv|env|*.pyc'"
alias g="git"
alias p="python"
alias p3="python3"
alias o="gnome-open"
alias vea="source venv/bin/activate"
alias ved="deactivate"
alias nmp="nodemon --exec python"
alias nm="nodemon --exec"
alias pag="ps aux | grep -v \"grep\" | grep"
alias xxx="xmodmap ~/.Xmodmap"
alias sag="sudo apt-get"
alias sagu="sudo apt-get update"
alias sagi="sudo apt-get install"
alias gr="./gradlew"

java_compile_and_run() {
        className=$1;
        dot="."
        dotsCount=$(echo "${className}" | awk -F"${dot}" '{print NF-1}')
        if (( $dotsCount > 1 )); then
                echo "$1 has more than one dot and this function doesn't wanna deal with it"
            return
        fi

        if [[ $className == *.java  ]] || [[ $className == *.  ]];
        then
                rest=${className#*$dot}
                dotIndex=$(( ${#className} - ${#rest} - ${#dot} ))
                className=${className:0:dotIndex}
        fi
        javac *.java && java $className ;
}
alias j="java_compile_and_run"

export GOPATH=$HOME/go
export PATH=$PATH:/usr/local/go/bin
export PATH=$PATH:$GOPATH/bin

alias tb="tensorboard --logdir=."
tb_dir(){
    tensorboard --logdir="$1"
}
alias tbd="tb_dir"

# nvm
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh" # This loads nvm
