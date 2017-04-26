
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
inoremap jk <ESC>
inoremap <C-b> <ESC>ha
inoremap <C-n> <ESC>ja
inoremap <C-p> <ESC>ka
inoremap <C-f> <ESC>la
inoremap ( ()<ESC>i
inoremap { {}<ESC>i
inoremap " ""<ESC>i
inoremap ' ''<ESC>i
nnoremap <C-j> 3j
nnoremap <C-k> j3k
vnoremap jk <ESC>

"color
try
	colorscheme desert
catch
endtry
set background=dark

"python 
au BufNewFile,BufRead *.py
    \ setlocal tabstop=4
    \ softtabstop=4
    \ shiftwidth=4
    \ textwidth=79
    \ expandtab
    \ autoindent
    \ fileformat=unix

"other
set nobackup
set nowb
set noswapfile
let mapleader=" "
set hlsearch
if $TERM_PROGRAM =~ "iTerm"
    let &t_SI = "\<Esc>]50;CursorShape=1\x7"
    let &t_EI = "\<Esc>]50;CursorShape=0\x7"
endif

"split window
map <leader>swh :topleft  vnew<CR>
nmap <leader>swl :botright vnew<CR>
nmap <leader>swk    :topleft  new<CR>
nmap <leader>swj  :botright new<CR>

"split buffer
nmap <leader>sh   :leftabove  vnew :e<CR>
nmap <leader>sl  :rightbelow vnew :e<CR>
nmap <leader>sk     :leftabove  new :e<CR>
nmap <leader>sj   :rightbelow new :e<CR>

