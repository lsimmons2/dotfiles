#!/bin/bash

#sudo apt-get install wmctrl, git, fzf, ripgrep, make, python3-pip, fasd


#FASD
#eval "$(fasd --init auto)"

# TERMINATOR
#mkdir /home/leo/.config/terminator
#curl https://raw.githubusercontent.com/lsimmons2/dotfiles/master/razer/terminator_config > ~/.config/terminator/config
#
## NVM/NODE
#curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.1/install.sh | bash
#[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
#[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
#nvm install node
#npm install -g nodemon
#
## YARN
#curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | sudo apt-key add -
#echo "deb https://dl.yarnpkg.com/debian/ stable main" | sudo tee /etc/apt/sources.list.d/yarn.list
#sudo apt update && sudo apt install --no-install-recommends yarn
#
## XMODMAP
#curl https://raw.githubusercontent.com/lsimmons2/dotfiles/master/razer/.Xmodmap > ~/.Xmodmap

# KEY REPEAT AND DELAY
#gsettings set org.gnome.desktop.peripherals.keyboard delay 165
#gsettings set org.gnome.desktop.peripherals.keyboard repeat-interval 30

# DOCKER
#curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
#sudo add-apt-repository \
#        "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
#        $(lsb_release -cs) \
#        stable"
#sudo apt-get update
#sudo apt-get install -y docker-ce docker-ce-cli containerd.io
#sudo groupadd docker
#sudo usermod -aG docker $USER
#newgrp docker
#
## VIM
#curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
#        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
#curl https://raw.githubusercontent.com/lsimmons2/dotfiles/master/.vimrc > ~/.vimrc

# GITCONFIG
#curl https://raw.githubusercontent.com/lsimmons2/dotfiles/master/.gitconfig > ~/.gitconfig
#
## BASHRC/PROFILE
#curl https://raw.githubusercontent.com/lsimmons2/dotfiles/master/razer/.bashrc > ~/.bashrc
#curl https://raw.githubusercontent.com/lsimmons2/dotfiles/master/razer/.bash_profile > ~/.bash_profile

#ln -s /home/leo/dotfiles/razer_ubuntu_20/.vimrc /home/leo/.vimrc
#ln -s /home/leo/dotfiles/razer_ubuntu_20/.bashrc /home/leo/.bashrc
#ln -s /home/leo/dotfiles/razer_ubuntu_20/.bash_profile /home/leo/.bash_profile

#NVIDIA/JUPYTER/DOCKER STUFF
#https://medium.com/@christoph.schranz/set-up-your-own-gpu-based-jupyterlab-e0d45fcacf43
#ubuntu-drivers devices
#sudo ubuntu-drivers autoinstall

distribution=$(. /etc/os-release;echo $ID$VERSION_ID)
echo $distribution   # this shows your version
curl -s -L https://nvidia.github.io/nvidia-docker/gpgkey | sudo apt-key add -
curl -s -L https://nvidia.github.io/nvidia-docker/$distribution/nvidia-docker.list | sudo tee /etc/apt/sources.list.d/nvidia-docker.list  # if not available, "ubuntu18.04" must be used as fallback
# fallback case: curl -s -L https://nvidia.github.io/nvidia-docker/ubuntu18.04/nvidia-docker.list | sudo tee /etc/apt/sources.list.d/nvidia-docker.list
sudo apt-get update && sudo apt-get install -y nvidia-container-toolkit
sudo apt-get install nvidia-container-runtime

sudo systemctl daemon-reload
sudo systemctl restart docker

#bash hotkey to fuzzy find most recent directories ✓
#bash hotkey to fuzzy find most recent files
    #- these can be used just as find would be
#bash hotkey to search from current cwd downward
#bash hotkey to get current docker container ✓

#grep in current directory
#grep from home directory

#vim shortcut to lcd to directory ✓

#command for running program on change
