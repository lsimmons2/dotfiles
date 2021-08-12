#!/bin/bash

#MAKE AND CHANGE TO SETUP DIR WHERE ANY REMNANTS OF THIS PROCESS WILL LIE
mkdir -p setup
cd setup

#APT-GET PACKAGES
sudo apt-get update
sudo apt-get -y install wmctrl git fzf ripgrep make python3-pip fasd exuberant-ctags mpg123 curl terminator vim-gtk gnome-tweaks

#FASD
eval "$(fasd --init auto)"

# NVM/NODE
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.1/install.sh | bash
nvm install node

# YARN
curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | sudo apt-key add -
echo "deb https://dl.yarnpkg.com/debian/ stable main" | sudo tee /etc/apt/sources.list.d/yarn.list
sudo apt update && sudo apt install --no-install-recommends yarn

# KEY REPEAT AND DELAY
gsettings set org.gnome.desktop.peripherals.keyboard delay 165
gsettings set org.gnome.desktop.peripherals.keyboard repeat-interval 30

# DOCKER
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
sudo add-apt-repository \
        "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
        $(lsb_release -cs) \
        stable"
sudo apt-get update
sudo apt-get install -y docker-ce docker-ce-cli containerd.io
sudo groupadd docker
sudo usermod -aG docker $USER
newgrp docker

# VIM
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

#SYMLINK DOTFILES
ln -s /home/leo/dotfiles/razer_ubuntu_20/.gitconfig /home/leo/.gitconfig
ln -s /home/leo/dotfiles/razer_ubuntu_20/.Xmodmap /home/leo/.Xmodmap
ln -s /home/leo/dotfiles/razer_ubuntu_20/.vimrc /home/leo/.vimrc
ln -s /home/leo/dotfiles/razer_ubuntu_20/.bashrc /home/leo/.bashrc
ln -s /home/leo/dotfiles/razer_ubuntu_20/.bash_profile /home/leo/.bash_profile
mkdir /home/leo/.config/terminator
ln -s /home/leo/dotfiles/razer_ubuntu_20/terminator_config /home/leo/.config/terminator/config

#SNAP PACKAGES
snap install spotify zoom-client

#TO HAVE DESIRED BEHAVIOR WHEN SHUTTING LAPTOP
echo GRUB_CMDLINE_LINUX_DEFAULT="quiet splash button.lid_init_state=open acpi=on" | sudo tee -a /etc/default/grub

#CHROME
wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
sudo dpkg -i google-chrome-stable_current_amd64.deb

#CHANGING WORKSPACE KEYBINDINGS
gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-1 []
gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-2 []
gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-3 []
gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-4 []
gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-5 []
gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-6 []
gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-7 []
gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-8 []
gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-9 []
gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-10 []
gsettings set org.gnome.shell.keybindings switch-to-application-1 []
gsettings set org.gnome.shell.keybindings switch-to-application-2 []
gsettings set org.gnome.shell.keybindings switch-to-application-3 []
gsettings set org.gnome.shell.keybindings switch-to-application-4 []
gsettings set org.gnome.shell.keybindings switch-to-application-5 []
gsettings set org.gnome.shell.keybindings switch-to-application-6 []
gsettings set org.gnome.shell.keybindings switch-to-application-7 []
gsettings set org.gnome.shell.keybindings switch-to-application-8 []
gsettings set org.gnome.shell.keybindings switch-to-application-9 []
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-1 "['<Super>0']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-2 "['<Super>1']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-3 "['<Super>2']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-4 "['<Super>3']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-5 "['<Super>4']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-6 "['<Super>5']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-7 "['<Super>6']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-8 "['<Super>7']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-9 "['<Super>8']"

#DONE MANUALLY
#- hide sidebar, arrange apps on it
#- AM/PM time
#- create workspaces
#- have startup scripts run on startup
#- create "static workspaces" with 9 static workspaces as described here https://askubuntu.com/a/1081253
#- emacs input in gnome-tweaks
