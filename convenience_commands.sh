#!/bin/bash

copy_dotfiles(){

    declare -a dotfile_names=(
        ~/.bashrc
        ~/.vimrc
        ~/.Xmodmap
        ~/.gitconfig
    )

    echo "copying dotfiles to dotfiles repo"
    for file_name in "${dotfile_names[@]}"
    do
        echo $file_name
        cp $file_name ~/projects/dotfiles/
    done

    terminator_config_file=~/.config/terminator/config
    echo $terminator_config_file
    cp $terminator_config_file ~/projects/dotfiles/terminator_config
}

alias copydot="copy_dotfiles"
