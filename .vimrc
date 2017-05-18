"general
set nobackup
set nowb
set noswapfile
let mapleader=" "
filetype plugin indent on

"GENERAL
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
call vundle#end()

"scripts
source ~/.vim/scripts/autoclose.vim

"mappings
inoremap jk <ESC>
inoremap <C-b> <ESC>ha
inoremap <C-f> <ESC>la
inoremap <C-e> <ESC>$a
inoremap <C-a> <ESC>0a
inoremap <C-d> <ESC>lxi
nnoremap S :w<CR>
nnoremap AS :wa<CR>
nnoremap ∆ 3j
nnoremap ˚ 3k                    
"close buffer in window without closing window itself
nnoremap <C-w>b :bp<bar>sp<bar>bn<bar>bd<CR> "show time
nnoremap time :echo 'Current time is ' . strftime('%c')<CR> " toggle lines
nnoremap <leader>l :set invnumber<CR> " toggle search highlight
nnoremap <leader>h :set hls!<CR> " search visually selected text 
vnoremap // y/<C-R>"<CR> 

"color
if (empty($TMUX))
  if (has("termguicolors"))
      set termguicolors
  endif
endif
syntax on
colorscheme onedark
hi Normal guibg=NONE ctermbg=NONE

"python 
set expandtab
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
nmap <leader>sh   :leftabove  vnew <CR>
nmap <leader>sl  :rightbelow vnew <CR>
nmap <leader>sk     :leftabove  new <CR>
nmap <leader>sj   :rightbelow new<CR>


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

"show open buffers
nnoremap <leader>b :ls<CR>:b

"statusline
set statusline=%t       "tail of the filename
set statusline+=%h      "help file flag
set statusline+=%m      "modified flag
set statusline+=%r      "read only flag
set statusline+=%=      "left/right separator
set statusline+=%c,     "cursor column
set statusline+=%l/%L   "cursor line/total lines
hi StatusLine ctermfg=234 ctermbg=107
hi StatusLineNC ctermfg=234 ctermbg=245

"netrw
let g:netrw_list_hide='.*\.pyc'
"other
set nobackup
set nowb
set noswapfile
if $TERM_PROGRAM =~ "iTerm"
   let &t_SI = "\<Esc>]50;CursorShape=1\x7"
   let &t_EI = "\<Esc>]50;CursorShape=0\x7"
endif
set clipboard=unnamed
filetype plugin indent on
set timeoutlen=1000 ttimeoutlen=0
let g:netrw_banner = 0


"todo
"- make command execution behave like bash, and have line cursor
"- figure out left option iterm mapping
"- make vim know to open a file in running instance of vim when using the vim command from bash
"- figure navigating by words w/o punctuation
