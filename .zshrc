

eval "$(fasd --init auto)"

my_cd() {
		if [[ "$#" -ne 0 ]]; then
			cd $(autojump $@)
			return
		fi
		dir_to_cd_to=$(fasd_cd -dl |  fzf --height 40% --reverse --inline-info)
		cd "$dir_to_cd_to"
		zle reset-prompt
}

zle -N my_cd
bindkey '^K' 'my_cd'

say_hello(){
    #echo "hello"
		cd "/home/leo/org"
}
zle -N say_hello
bindkey '^Y' say_hello


PROMPT='%~%% '
##PROMPT='$(_user_host)${_current_dir} $(git_prompt_info) $(ruby_prompt_info)
##%{%(!.%F{red}.%F{white})%}▶%'

#git_prompt() {
 #ref=$(git symbolic-ref HEAD | cut -d'/' -f3)
 #echo $ref
#}
#setopt prompt_subst
##PS1=$(git_prompt)%#

#PROMPT='%B%~%b git($(git_prompt))$ '

#autoload -U promptinit
#promptinit


alias ls="ls --color=tty"
alias g=git
alias p=python
alias p3=python3
alias vea="source venv/bin/activate"
alias ved="deactivate"
alias xxx="xmodmap ~/.Xmodmap"
alias w1="watch -n1 "
alias grep="grep --color=auto -i"
alias scb="python3 /home/leo/org/cards/client/save_cards.py /home/leo/org/notes/blah.txt"
alias pag="ps aux | grep -v \"grep\" | grep"
alias lc="leetcode"
alias la="ls -a"
alias ll="ls -lh"
