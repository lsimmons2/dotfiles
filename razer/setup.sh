# APT-GET STUFF
sudo apt-get update
sudo apt-get upgrade
sudo apt-get -y install\
        vim-gtk\
        htop\
        git\
        terminator\
        curl
        apt-transport-https \
        ca-certificates \
        curl \
        gnupg-agent \
        software-properties-common\
        build-essential\
        chrome-gnome-shell\
        python3-venv\
        gnome-tweaks

# apt-transport-https -> software-properties-common for docker

# DO EVERYTHING IN HOME DIR FOR NOW
cd 

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


# TERMINATOR
mkdir /home/leo/.config/terminator
curl https://raw.githubusercontent.com/lsimmons2/dotfiles/master/razer/terminator_config > ~/.config/terminator/config

# NVM/NODE
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.1/install.sh | bash
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
nvm install 8.10.0
npm install -g nodemon

# YARN
curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | sudo apt-key add -
echo "deb https://dl.yarnpkg.com/debian/ stable main" | sudo tee /etc/apt/sources.list.d/yarn.list
sudo apt update && sudo apt install --no-install-recommends yarn

# XMODMAP
curl https://raw.githubusercontent.com/lsimmons2/dotfiles/master/razer/.Xmodmap > ~/.Xmodmap

# VIM
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
curl https://raw.githubusercontent.com/lsimmons2/dotfiles/master/.vimrc > ~/.vimrc

# CHROME
wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
sudo dpkg -i google-chrome-stable_current_amd64.deb

# SPOTIFY
curl -sS https://download.spotify.com/debian/pubkey.gpg | sudo apt-key add - 
echo "deb http://repository.spotify.com stable non-free" | sudo tee /etc/apt/sources.list.d/spotify.list
sudo apt-get update && sudo apt-get install spotify-client

# SLACK
wget https://downloads.slack-edge.com/linux_releases/slack-desktop-4.0.2-amd64.deb
sudo apt-get install ./slack-desktop-4.0.2-amd64.deb

# KEY REPEAT AND DELAY
gsettings set org.gnome.desktop.peripherals.keyboard delay 165
gsettings set org.gnome.desktop.peripherals.keyboard repeat-interval 30

# GITCONFIG
curl https://raw.githubusercontent.com/lsimmons2/dotfiles/master/.gitconfig > ~/.gitconfig

# BASHRC/PROFILE
curl https://raw.githubusercontent.com/lsimmons2/dotfiles/razer//master/.bashrc > ~/.bashrc
curl https://raw.githubusercontent.com/lsimmons2/dotfiles/razer/master/.bash_profile > ~/.bash_profile


# MANUALLY
# make dock small, on the bottom, hidden
# make workspace switching animation fast - https://extensions.gnome.org/extension/1328 / https://askubuntu.com/a/1083797/725311
# solarized
# node/yarn install doesn't work
# copy keys and make ~/.ssh/config
