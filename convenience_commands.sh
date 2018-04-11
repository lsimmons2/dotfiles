
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


screenshot_study_image(){
    # images in dir are all numbers (1.png, 2.png) -
    # take a screenshot with a file name 1 higher than last
    STUDY_IMAGES_DIR=~/org/.study_images
    mkdir -p $STUDY_IMAGES_DIR
    last_image_file_name=$(ls $STUDY_IMAGES_DIR | egrep '[0-9]+\.png' | sort -g | tail -n 1)
    last_image_file_number="$(echo $last_image_file_name | cut -d'.' -f1)"
    new_image_file_name=$((last_image_file_number+1)).png
    new_image_file_path=$STUDY_IMAGES_DIR/$new_image_file_name
    echo $new_image_file_name | xclip -selection clipboard
    gnome-screenshot -a -f $new_image_file_path
    notify-send --expire-time=500 "$new_image_file_path saved"
}
