"general
set nobackup
set nowb
set noswapfile
let mapleader=" "
filetype plugin indent on
set backspace=indent,eol,start
set ic
set expandtab
set autoindent
set ruler

"vundle
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'scrooloose/nerdcommenter'
Plugin 'mustache/vim-mustache-handlebars'
Plugin 'pangloss/vim-javascript'
Plugin 'mxw/vim-jsx'
Plugin 'alvan/vim-closetag'
Plugin 'jiangmiao/auto-pairs'
Plugin 'kien/ctrlp.vim'
Plugin 'henrik/vim-indexed-search'
Plugin 'tpope/vim-surround'
Bundle 'djoshea/vim-autoread'
call vundle#end()

"enables code coloring
syntax on

"jsx
let g:jsx_ext_required = 0
let g:closetag_filenames = "*.html,*.js,*.jsx"

"ctrlp
let g:ctrlp_custom_ignore = '\v[\/](node_modules|venv|target|dist)|(\.(swp|pyc|git|svn))$'

"mappings
inoremap jk <ESC>
inoremap kj <ESC>
inoremap KJ <ESC>
inoremap JK <ESC>
inoremap kJ <ESC>
inoremap Kj <ESC>
nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk
nnoremap 0 g0
nnoremap $ g$
nnoremap <M-y> v$hy
nnoremap <M-p> v$hp
nnoremap <M-P> v$hP
inoremap <C-b> <ESC>ha
inoremap <C-f> <ESC>la
inoremap <C-e> <ESC>$a
inoremap <C-a> <ESC>0a
inoremap <C-d> <ESC>lxi
nnoremap <leader>c :call NERDComment('n',"toggle")<CR>
vnoremap <leader>c :call NERDComment('n',"toggle")<CR>
let g:NERDCreateDefaultMappings = 0
nnoremap S :w<CR>
nnoremap <leader>S :wa<CR>
"close buffer in window without closing window itself
nnoremap <C-w>b :bp<bar>sp<bar>bn<bar>bd<CR>
nnoremap <leader>t :set noexpandtab<CR>
nnoremap <leader>l :set invnumber<CR> " toggle lines
nnoremap <leader>h :set hls!<CR> " toggle search highlight
nnoremap <leader>m :call cursor(0, len(getline('.'))/2)<CR>
nnoremap <leader>e :e .<CR>
nnoremap <leader>b :b#<CR>
vnoremap // y/<C-R>"<CR>  " search visually selected text 
nnoremap <leader>y v$hy
nnoremap <leader>p v$hp

"java
autocmd FileType java set tabstop=4
autocmd FileType java set shiftwidth=4
autocmd FileType java inoremap psvm public static void main(String[] args) {}<ESC>i<CR><CR><ESC>kcc
autocmd FileType java inoremap sop System.out.println();<ESC>hi
autocmd FileType java inoremap forr for (int i = 0; i < array.length; i++) {}<ESC>BBBce

"python
autocmd FileType python set tabstop=4
autocmd FileType python set shiftwidth=4

"go
"log fmt.Println()<ESC>i
autocmd FileType go set tabstop=4
autocmd FileType go set shiftwidth=4

".txt files
"setlocal spell spelllang=en_us


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


"statusline
set statusline=%f       "tail of the filename
set statusline+=%h      "help file flag
set statusline+=%m      "modified flag
set statusline+=%r      "read only flag
set statusline+=%=      "left/right separator
set statusline+=Line:%-2l/%-6L "cursor line/total lines
set statusline+=Column:\ %-2c "cursor column

hi StatusLine ctermfg=234 guifg=#1c1c1c ctermbg=107 guibg=#87af5f
hi StatusLineNC ctermfg=234 guifg=#1c1c1c ctermbg=245 guibg=#8a8a8a

"netrw
let g:netrw_list_hide='.*\.pyc'
set timeoutlen=1000 ttimeoutlen=0
let g:netrw_banner = 0
let g:netrw_keepdir=0 "so I can move files in netrw

" silver searcher
if executable('ag')
  set grepprg=ag\ --nogroup\ --nocolor
endif
command -nargs=+ -complete=file -bar Ag silent! grep! <args>|cwindow|redraw!
nnoremap <leader>g :Ag<SPACE>
