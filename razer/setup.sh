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
        software-properties-common

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

# TERMINATOR
mkdir /home/leo/.config/terminator
curl https://raw.githubusercontent.com/lsimmons2/dotfiles/master/terminator_config > ~/.config/terminator/config

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

# VIM
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
curl https://raw.githubusercontent.com/lsimmons2/dotfiles/master/.vimrc > ~/.vimrc

# KEY REPEAT AND DELAY
gsettings set org.gnome.desktop.peripherals.keyboard delay 165
gsettings set org.gnome.desktop.peripherals.keyboard repeat-interval 30

# GITCONFIG
curl https://raw.githubusercontent.com/lsimmons2/dotfiles/razer/master/.gitconfig > ~/.gitconfig

# BASHRC/PROFILE
curl https://raw.githubusercontent.com/lsimmons2/dotfiles/razer//master/.bashrc > ~/.bashrc
curl https://raw.githubusercontent.com/lsimmons2/dotfiles/razer/master/.bash_profile > ~/.bash_profile

# FOR TRACKPAD
sudo apt remove xserver-xorg-input-synaptics
sudo apt install xserver-xorg-core
sudo apt install xserver-xorg-input-libinput


# MANUALLY
# download chrome, spotify, slack
 # make dock small, on the bottom, hidden
