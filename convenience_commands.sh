#!/bin/bash

manage_dotfiles(){

    declare -a dotfile_names=(
        .bashrc
        .vimrc
        .Xmodmap
        .gitconfig
    )

    action=$1

    if [[ $action == "collect" ]]; then
        echo "collecting dotfiles into dotfiles repo"
    elif [[ $action == "distribute" ]]; then
        echo "distributing dotfiles from repo"
    fi

    for file_name in "${dotfile_names[@]}"
    do
        echo $file_name
        if [[ $action == "collect" ]]; then
            cp ~/$file_name ~/projects/dotfiles/$file_name
        elif [[ $action == "distribute" ]]; then
            cp ~/projects/dotfiles/$file_name ~/$file_name
        fi
    done

    # terminator config is special because it doesn't go directly
    # in home dir like the other files
    terminator_config_file=~/.config/terminator/config
    echo $terminator_config_file
    if [[ $action == "collect" ]]; then
        cp $terminator_config_file ~/projects/dotfiles/terminator_config
    elif [[ $action == "distribute" ]]; then
        cp ~/projects/dotfiles/terminator_config $terminator_config_file
    fi

}

alias dotcol="manage_dotfiles collect"
alias dotdis="manage_dotfiles distribute"
