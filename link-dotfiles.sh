#!/bin/bash

DOTFILES_DIR=~/dotfiles

files=(".vimrc" ".zshrc" ".gitconfig" ".emacs.d")

for file in "${files[@]}"; do
    ln -sf "$DOTFILES_DIR/$file" ~/"$file"
    echo "Symlinked $file to home directory."
done
