"general
set nobackup
set nowb
set noswapfile
let mapleader=" "
if $TERM_PROGRAM =~ "iTerm"
    let &t_SI = "\<Esc>]50;CursorShape=1\x7"
    let &t_EI = "\<Esc>]50;CursorShape=0\x7"
endif
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
Plugin 'Vimjas/vim-python-pep8-indent'
Plugin 'joshdick/onedark.vim'
Plugin 'mustache/vim-mustache-handlebars'
Plugin 'pangloss/vim-javascript'
Plugin 'mxw/vim-jsx'
Plugin 'alvan/vim-closetag'
Plugin 'jiangmiao/auto-pairs'
Plugin 'kien/ctrlp.vim'
call vundle#end()

"jsx
let g:jsx_ext_required = 0
let g:closetag_filenames = "*.html,*.js,*.jsx"

"ctrlp
let g:ctrlp_custom_ignore = '\v[\/](node_modules|venv|target|dist)|(\.(swp|pyc|git|svn))$'

"mappings
inoremap jk <ESC>
inoremap kj <ESC>
nnoremap j gj
nnoremap k gk
nnoremap 0 g0
nnoremap $ g$
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
nnoremap ∆ 3j
nnoremap ˚ 3k                    
"close buffer in window without closing window itself
nnoremap <C-w>b :bp<bar>sp<bar>bn<bar>bd<CR>
nnoremap time :echo '=== ' . strftime('%c') . ' ==='<CR> "show time
nnoremap <leader>l :set invnumber<CR> " toggle lines
nnoremap <leader>h :set hls!<CR> " toggle search highlight
nnoremap <leader>m :call cursor(0, len(getline('.'))/2)<CR>
nnoremap <leader>j ddp
nnoremap <leader>k ddkP
nnoremap <leader>e :e .<CR>
nnoremap <leader>b :b#<CR>
vnoremap // y/<C-R>"<CR>  " search visually selected text 

"color
if (empty($TMUX))
  if (has("termguicolors"))
      set termguicolors
  endif
endif
syntax on
colorscheme onedark
"hi Normal guibg=NONE ctermbg=NONE
hi Search ctermfg=234 guifg=#1c1c1c ctermbg=105 guibg=#8787ff


"python 
autocmd BufRead,BufNewFile *.py call SetPyOptions()
function SetPyOptions()
  set tabstop=4
  set softtabstop=4
  set shiftwidth=4
endfunction

"go
autocmd BufRead,BufNewFile *.go inoremap call SetGoOptions()
function SetGoOptions()
  log fmt.Println()<ESC>i
  set tabstop=4
  set softtabstop=4
  set shiftwidth=4
endfunction

"js
autocmd BufRead,BufNewFile *.js call SetJsOptions()
function SetJsOptions()
  setlocal tabstop=2
  set softtabstop=2
  set shiftwidth=2
  inoremap log console.log();<ESC>hi
endfunction

"html
autocmd BufRead,BufNewFile *.html call SetHtmlOptions()
function SetHtmlOptions()
  setlocal tabstop=2
  set softtabstop=2
  set shiftwidth=2
endfunction

"txt
autocmd BufRead,BufNewFile *.txt call SetTxtOptions()
function SetTxtOptions()
  setlocal tabstop=4
  set softtabstop=4
  set shiftwidth=4
  setlocal spell spelllang=en_us
endfunction

"bash
autocmd BufRead,BufNewFile *.sh call SetBashOptions()
function SetBashOptions()
  setlocal tabstop=4
  set softtabstop=4
  set shiftwidth=4
endfunction

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
set statusline+=%c,     "cursor column
set statusline+=%l/%L   "cursor line/total lines
hi StatusLine ctermfg=234 guifg=#1c1c1c ctermbg=107 guibg=#87af5f
hi StatusLineNC ctermfg=234 guifg=#1c1c1c ctermbg=245 guibg=#8a8a8a

"netrw
let g:netrw_list_hide='.*\.pyc'
set timeoutlen=1000 ttimeoutlen=0
let g:netrw_banner = 0

" silver searcher
if executable('ag')
  set grepprg=ag\ --nogroup\ --nocolor
endif
command -nargs=+ -complete=file -bar Ag silent! grep! <args>|cwindow|redraw!
nnoremap <leader>g :Ag<SPACE>

