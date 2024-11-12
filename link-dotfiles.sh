#!/bin/bash

# Define the directory containing your dotfiles
DOTFILES_DIR=~/dotfiles

# List of files to symlink
files=(".vimrc" ".emacs" ".zshrc" ".gitconfig")

for file in "${files[@]}"; do
    # Create a symlink in the home directory pointing to the file in DOTFILES_DIR
    ln -sf "$DOTFILES_DIR/$file" ~/"$file"
    echo "Symlinked $file to home directory."
done
