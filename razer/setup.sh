# SETUP DIR
mkdir -p ~/setup
cd ~/setup

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
        gnome-tweaks\
        tree

# apt-transport-https -> software-properties-common for docker

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
curl https://raw.githubusercontent.com/lsimmons2/dotfiles/razer/master/.bashrc > ~/.bashrc
curl https://raw.githubusercontent.com/lsimmons2/dotfiles/razer/master/.bash_profile > ~/.bash_profile


# enables gpu support for docker containers... which comes by default with docker 19.03
# sudo apt-get install -y docker nvidia-container-toolkit

# CUDA DRIVERS/TOOLKIT
#wget https://developer.download.nvidia.com/compute/cuda/repos/ubuntu1804/x86_64/cuda-ubuntu1804.pin
#sudo mv cuda-ubuntu1804.pin /etc/apt/preferences.d/cuda-repository-pin-600
#wget http://developer.download.nvidia.com/compute/cuda/10.1/Prod/local_installers/cuda-repo-ubuntu1804-10-1-local-10.1.243-418.87.00_1.0-1_amd64.deb
#sudo dpkg -i cuda-repo-ubuntu1804-10-1-local-10.1.243-418.87.00_1.0-1_amd64.deb
#sudo apt-key add /var/cuda-repo-10-1-local-10.1.243-418.87.00/7fa2af80.pub
#sudo apt-get update
#sudo apt-get -y install cuda

# NVIDIA DOCKER
#distribution=$(. /etc/os-release;echo $ID$VERSION_ID)
#curl -s -L https://nvidia.github.io/nvidia-docker/gpgkey | sudo apt-key add -
#curl -s -L https://nvidia.github.io/nvidia-docker/ubuntu18.04/nvidia-docker.list | sudo tee /etc/apt/sources.list.d/nvidia-docker.list
#curl -s -L https://nvidia.github.io/nvidia-docker/$distribution/nvidia-docker.list | sudo tee /etc/apt/sources.list.d/nvidia-docker.list
#sudo apt-get update
#sudo apt-get install -y nvidia-container-toolkit
#sudo systemctl restart docker

# PREY
wget https://downloads.preyproject.com/prey-client-releases/node-client/1.9.2/prey_1.9.2_amd64.deb
sudo dpkg -i prey_1.9.2_amd64.deb # will error out due to missing dependencies
sudo apt-get -f -y install # updates the missing dependencies for prey and prey itself

# RIPGREP
wget https://github.com/BurntSushi/ripgrep/releases/download/11.0.2/ripgrep_11.0.2_amd64.deb
sudo dpkg -i ripgrep_11.0.2_amd64.deb


# MANUALLY
# make dock small, on the bottom, hidden
# solarized
# node/yarn install doesn't work
# copy keys and make ~/.ssh/config
