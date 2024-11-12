#!/bin/bash

#MAKE AND CHANGE TO SETUP DIR WHERE ANY REMNANTS OF THIS PROCESS WILL LIE
mkdir -p /home/leo/dotfiles/razer_as_gpu_workhorse/setup
cd /home/leo/dotfiles/razer_as_gpu_workhorse/setup

#APT-GET PACKAGES
sudo apt-get update
sudo apt-get -y install git make python3-pip curl terminator vim-gtk gnome-tweaks cheese xdotool libheif-examples python3-autopep8 htop

# KEY REPEAT AND DELAY
gsettings set org.gnome.desktop.peripherals.keyboard delay 165
gsettings set org.gnome.desktop.peripherals.keyboard repeat-interval 30


#SYMLINK DOTFILES
ln -s /home/leo/dotfiles/razer_as_gpu_workhorse/.gitconfig /home/leo/.gitconfig
ln -s /home/leo/dotfiles/razer_as_gpu_workhorse/.Xmodmap /home/leo/.Xmodmap
rm -f /home/leo/.bashrc
ln -s /home/leo/dotfiles/razer_as_gpu_workhorse/.bashrc /home/leo/.bashrc
ln -s /home/leo/dotfiles/razer_as_gpu_workhorse/.bash_profile /home/leo/.bash_profile
mkdir /home/leo/.config/terminator
ln -s /home/leo/dotfiles/razer_as_gpu_workhorse/terminator_config /home/leo/.config/terminator/config
ln -s /home/leo/dotfiles/razer_as_gpu_workhorse/.vimrc /home/leo/.vimrc

#CHROME
wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
sudo dpkg -i google-chrome-stable_current_amd64.deb

echo "FINISHED SETUP"

#WILL be appending things I do here as time goes on:
# 09.05.2024
# 
# sudo apt install python3.10-venv
# 
# b/c:
# leo@greta:~/dev/easy-local-rag$ p3 -m venv venv
# The virtual environment was not created successfully because ensurepip is not
# available.  On Debian/Ubuntu systems, you need to install the python3-venv
# package using the following command.
# 
#     apt install python3.10-venv
# 
# You may need to use sudo with that command.  After installing the python3-venv
# package, recreate your virtual environment.

# *** at some point did ***
# sudo apt-get install openssh-server
