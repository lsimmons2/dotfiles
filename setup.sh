#!/bin/bash



##################################
############ PACKAGES ############
##################################

sudo apt-get update
sudo apt-get upgrade
sudo apt-get -y install vim-gtk\
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
 inotify-tools\
 xvfb\
 silversearcher-ag\
 libfreetype6-dev\
 lib32ncurses5-dev\
 xclip

# chrome
wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | sudo apt-key add - 
sudo sh -c 'echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google-chrome.list'
sudo apt-get update
sudo apt-get install google-chrome-stable

# chromedriver for selenium
curl -o /home/leo/Downloads/chromedriver.zip https://chromedriver.storage.googleapis.com/2.31/chromedriver_linux64.zip 
unzip /home/leo/Downloads/chromedriver.zip -d /home/leo/Downloads
sudo mv /home/leo/Downloads/chromedriver /usr/local/bin

# vim
git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim

# terminator
mkdir /home/leo/.config/terminator
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

# mongo
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 0C49F3730359A14518585931BC711F9BA15703C6
echo "deb [ arch=amd64,arm64 ] http://repo.mongodb.org/apt/ubuntu xenial/mongodb-org/3.4 multiverse" | sudo tee /etc/apt/sources.list.d/mongodb-org-3.4.list
sudo apt-get update
sudo apt-get install -y mongodb-org

# rsyslog and dependencies
cd
curl -O http://libestr.adiscon.com/files/download/libestr-0.1.10.tar.gz
tar xzf libestr-0.1.10.tar.gz
cd libestr-0.1.10
./configure --libdir=/usr/lib --includedir=/usr/include
sudo make
sudo make install

cd
curl -O http://www.libee.org/files/download/libee-0.4.1.tar.gz
tar xzf libee-0.4.1.tar.gz
cd libee-0.4.1
./configure --libdir=/usr/lib --includedir=/usr/include
sudo make
sudo make install

cd
curl -O http://download.rsyslog.com/liblogging/liblogging-1.0.6.tar.gz
tar xfz liblogging-1.0.6.tar.gz
cd liblogging-1.0.6
./configure --libdir=/usr/lib --includedir=/usr/include

sudo apt-get install dh-autoreconf

cd
git clone https://github.com/json-c/json-c.git
cd json-c/
sh autogen.sh
sudo make
sudo make install

sudo apt-get install uuid-dev
sudo apt-get install -y libgcrypt11-dev

sudo apt-get install docutils-common

cd /home/leo/rsyslog-7.6.1
./configure --prefix=/usr

##################################
############ CONFIG ############
##################################

# touchpad
curl https://raw.githubusercontent.com/lsimmons2/dotfiles/master/touchpad_settings > ~/.config/touchpad_settings

# fn keys
sudo echo "echo -n 0x02 > /sys/module/hid_apple/parameters/fnmode" > /etc/rc.local

#key repeat and delay
gsettings set org.gnome.desktop.peripherals.keyboard delay 165
gsettings set org.gnome.desktop.peripherals.keyboard repeat-interval 30

# .vimrc
curl https://raw.githubusercontent.com/lsimmons2/dotfiles/master/.vimrc > ~/.vimrc

# git
curl https://raw.githubusercontent.com/lsimmons2/dotfiles/master/.gitconfig > ~/.gitconfig

# .bashrc/_profile
curl https://raw.githubusercontent.com/lsimmons2/dotfiles/master/.bashrc > ~/.bashrc
curl https://raw.githubusercontent.com/lsimmons2/dotfiles/master/.bash_profile > ~/.bash_profile
# .Xmodmap
curl https://raw.githubusercontent.com/lsimmons2/dotfiles/master/.Xmodmap > ~/.Xmodmap

# remove unused default dirs
cd /home/leo
rm -r Documents/ Music/ Templates/ Videos/

# install node
source ~/.bashrc
nvm install 6.11.2
npm install -g gulp bower nodemon

#thinkpap trackpoint settings
echo 255 | sudo tee /sys/devices/platform/i8042/serio1/serio2/sensitivity > /dev/null
echo 150 | sudo tee /sys/devices/platform/i8042/serio1/serio2/speed > /dev/null
echo 1 | sudo tee /sys/devices/platform/i8042/serio1/serio2/press_to_select > /dev/null

curl https://raw.githubusercontent.com/lsimmons2/dotfiles/master/.lesskey > ~/.lesskey
lesskey



# ==========================
# THINGS I STILL DO MANUALLY
# ==========================

# hide launcher
# sign into chrome
# hide launcher and only show in built-in display
# disable left alt as HUD key
# change screenshot shortcuts
