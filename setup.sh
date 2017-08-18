#!/bin/bash



##################################
############ PACKAGES ############
##################################

apt-get update
apt-get upgrade
apt-get -y install vim-gtk\
 htop\
 git\
 terminator\
 curl\
 golang-go\
 python-pip\
 mysql-server\
 libmysqlclient-dev\
 build-essential\
 libssl-dev\
 libffi-dev\
 python-dev\
 inotify-tools

# chrome
wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | sudo apt-key add - 
sudo sh -c 'echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google-chrome.list'
apt-get update
apt-get install google-chrome-stable

# vim
git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim

# terminator
curl https://raw.githubusercontent.com/lsimmons2/dotfiles/master/terminator_config > ~/.config/terminator/config

# python
pip install virtualenv
pip install --upgrade pip

# spotify
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys BBEBDCB318AD50EC6865090613B00F1FD2C19886 0DF731E45CE24F27EEEB1450EFDC8610341D9410
echo deb http://repository.spotify.com stable non-free | sudo tee /etc/apt/sources.list.d/spotify.list
sudo apt-get update
sudo apt-get install spotify-client

# nvm
curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.2/install.sh | bash

# redis
wget http://download.redis.io/redis-stable.tar.gz
tar xvzf redis-stable.tar.gz
cd redis-stable
make
make test
sudo cp src/redis-server /usr/local/bin/
sudo cp src/redis-cli /usr/local/bin



##################################
############ CONFIG ############
##################################

# touchpad
curl https://raw.githubusercontent.com/lsimmons2/dotfiles/master/touchpad_settings > ~/.config/touchpad_settings

# fn keys
sudo echo "echo -n 0x02 > /sys/module/hid_apple/parameters/fnmode" > /etc/rc.local

# .vimrc
curl https://raw.githubusercontent.com/lsimmons2/dotfiles/master/.vimrc > ~/.vimrc


# git
curl https://raw.githubusercontent.com/lsimmons2/dotfiles/master/git/.gitconfig > ~/.gitconfig

# .bashrc/_profile
curl https://raw.githubusercontent.com/lsimmons2/dotfiles/master/.bashrc > ~/.bashrc
curl https://raw.githubusercontent.com/lsimmons2/dotfiles/master/.bash_profile > ~/.bash_profile

# remove unused default dirs
cd /home/leo
rm -r Documents/ Music/ Templates/ Videos/
