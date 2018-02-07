#/bin/bash

git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
curl https://raw.githubusercontent.com/lsimmons2/dotfiles/master/.vimrc > ~/.vimrc
vim +PluginInstall +qall
