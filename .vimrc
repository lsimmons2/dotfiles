
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

"color
try
	colorscheme desert
catch
endtry
set background=dark

"python 
autocmd BufNewFile,BufRead *.py
    \ set tabstop=4
    \ set softtabstop=4
    \ set shiftwidth=4
    \ set textwidth=79
    \ set expandtab
    \ set autoindent
    \ set fileformat=unix

"other
set nobackup
set nowb
set noswapfile
let mapleader=","

"split window
map <leader>swh :topleft  vnew<CR>
nmap <leader>swl :botright vnew<CR>
nmap <leader>swk    :topleft  new<CR>
nmap <leader>swj  :botright new<CR>

"split buffer
nmap <leader>sh   :leftabove  vnew<CR>
nmap <leader>sl  :rightbelow vnew<CR>
nmap <leader>sk     :leftabove  new<CR>
nmap <leader>sj   :rightbelow new<CR>




