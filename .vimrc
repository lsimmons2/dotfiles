
"vundle
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Bundle 'altercation/vim-colors-solarized'
Bundle 'scrooloose/nerdcommenter'
Bundle 'Vimjas/vim-python-pep8-indent'
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
inoremap <C-e> <ESC>$a
inoremap <C-a> <ESC>0a
inoremap <C-d> <ESC>lxj
inoremap ( ()<ESC>i
inoremap { {}<ESC>i
inoremap " ""<ESC>i
inoremap ' ''<ESC>i
nnoremap S :w<CR>
nnoremap AS :wa<CR>
nnoremap ∆ 3j
nnoremap ˚ 3k
nnoremap <C-w>b :bp<bar>sp<bar>bn<bar>bd<CR>
vnoremap jk <ESC>

"color
try
	colorscheme desert
catch
endtry
set background=dark

""python 
set expandtab
set textwidth=120
set tabstop=4
set softtabstop=4
set shiftwidth=4
set autoindent
set ruler
nnoremap <leader>l :set invnumber<CR>

"other
set nobackup
set nowb
set noswapfile
let mapleader=" "
""set hlsearch
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
nmap <leader>sh   :leftabove  vnew <CR>:
nmap <leader>sl  :rightbelow vnew <CR>:
nmap <leader>sk     :leftabove  new <CR>:
nmap <leader>sj   :rightbelow new<CR>:

"resize windows
nnoremap <S-Left> :vertical resize -2<CR>
nnoremap <S-Right> :vertical resize +2<CR>
nnoremap <S-Up> : resize +2<CR>
nnoremap <S-Down> : resize -2<CR>

"navigate windows
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>


"todo
"- make command execution behave like bash, and have line cursor
"- figure out left option iterm mapping

