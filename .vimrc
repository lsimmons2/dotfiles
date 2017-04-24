
"vundle
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Bundle 'altercation/vim-colors-solarized'
call vundle#end()
filetype plugin indent on
syntax enable
set background=dark
colorscheme solarized

"mappings
imap jk <ESC>
noremap <C-j> 3j
noremap <C-k> j3k

"python 
au BufNewFile,BufRead *.py
    \ set tabstop=4
    \ set softtabstop=4
    \ set shiftwidth=4
    \ set textwidth=79
    \ set expandtab
    \ set autoindent
    \ set fileformat=unix


