
"vundle
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'bling/vim-bufferline'
Bundle 'scrooloose/nerdcommenter'
Bundle 'Vimjas/vim-python-pep8-indent'
Bundle 'joshdick/onedark.vim'
Bundle 'vim-airline/vim-airline'
call vundle#end()


"MAPPINGS
inoremap jk <ESC>
inoremap <C-b> <ESC>ha
inoremap <C-n> <ESC>ja
inoremap <C-p> <ESC>ka
inoremap <C-f> <ESC>la
inoremap <C-e> <ESC>$a
inoremap <C-a> <ESC>0a
inoremap <C-d> <ESC>lxi
inoremap ( ()<ESC>i
inoremap { {}<ESC>i
inoremap " ""<ESC>i
inoremap ' ''<ESC>i
nnoremap S :w<CR>
nnoremap AS :wa<CR>
nnoremap ∆ 3j
nnoremap ˚ 3k
"close buffer in window without closing window itself
nnoremap <C-w>b :bp<bar>sp<bar>bn<bar>bd<CR>
" show time
nnoremap time :echo 'Current time is ' . strftime('%c')<CR>
" toggle lines
nnoremap <leader>l :set invnumber<CR>
" toggle search highlight
nnoremap <leader>h :set hls!<CR>
" search visually selected text 
vnoremap // y/<C-R>"<CR>

"COLOR
if (empty($TMUX))
    if (has("termguicolors"))
        set termguicolors
    endif
endif
syntax on
colorscheme onedark

"PYTHON 
set expandtab
set textwidth=120
set tabstop=4
set softtabstop=4
set shiftwidth=4
set autoindent
set ruler

"create new window entire width or height of
map <leader>swh :topleft  vnew<CR>
nmap <leader>swl :botright vnew<CR>
nmap <leader>swk    :topleft  new<CR>
nmap <leader>swj  :botright new<CR>

"split current window
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

"OTHER
set nobackup
set nowb
set noswapfile
let mapleader=" "
if $TERM_PROGRAM =~ "iTerm"
    let &t_SI = "\<Esc>]50;CursorShape=1\x7"
    let &t_EI = "\<Esc>]50;CursorShape=0\x7"
endif
set clipboard=unnamed
filetype plugin indent on
"for vim-airline
set laststatus=2

"todo
"- make command execution behave like bash, and have line cursor
"- figure out left option iterm mapping
"- make vim know to open a file in running instance of vim when using the vim command from bash
"- figure navigating by words w/o punctuation
