"GENERAL
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
set foldmethod=syntax
set foldopen-=hor
set nofoldenable
syntax on "enables code coloring
set ignorecase "Case-insensitive searching.
set smartcase "But case-sensitive if expression contains a capital letter.
"setlocal spell spelllang=en_us
"set dictionary+=/usr/share/dict/american-english
set complete+=k " make complete use dictionaries as well
set wildmode=longest,list,full
set wildmenu
map <leader>2 mqgg=G'qzz



"VANILLA MAPPINGS
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
inoremap <C-b> <ESC>ha
inoremap <C-f> <ESC>la
inoremap <C-e> <ESC>$a
inoremap <C-a> <ESC>0i
inoremap <C-d> <ESC>lxi
nnoremap S :w<CR>
nnoremap <leader>S :wa<CR>

nnoremap <leader>e :e .<CR>
nnoremap <leader>b :b#<CR>
vnoremap // y/<C-R>"<CR>  " search visually selected text 
nnoremap <leader>y "+y
nnoremap <leader>p "+p
vnoremap <leader>y "+y
vnoremap <leader>p "+p
nnoremap <leader>Y "+Y
nnoremap <leader>P "+P
nnoremap <leader>t :tabnew<CR>
nnoremap <leader>] :tabnext<CR>
nnoremap <leader>[ :tabprevious<CR>
inoremap <expr> <C-j> ((pumvisible())?("\<C-n>"):("\<C-j>")) "allow C-j and C-k to scroll in autocomplete windows
inoremap <expr> <C-k> ((pumvisible())?("\<C-p>"):("\<C-k>"))

" "OPTIONS"
nnoremap <leader>ol :set invnumber<CR> " toggle lines
nnoremap <leader>oh :set hls!<CR> " toggle search highlight

" STATUSLINE
set laststatus=2        "have statusline always show (even with single window)
set statusline=%f       "tail of the filename
set statusline+=%h      "help file flag
set statusline+=%m      "modified flag
set statusline+=%r      "read only flag
set statusline+=%=      "left/right separator
set statusline+=Line:%-2l/%-6L "cursor line/total lines
"hi StatusLine ctermfg=234 guifg=#1c1c1c ctermbg=107 guibg=#87af5f
"hi StatusLineNC ctermfg=234 guifg=#1c1c1c ctermbg=245 guibg=#8a8a8a


"NETRW
let g:netrw_list_hide='.*\.pyc'
set timeoutlen=1000 ttimeoutlen=0
let g:netrw_banner = 0


"HIGHLIGHTING
hi Search cterm=NONE ctermfg=Black ctermbg=DarkGreen
hi DiffChange cterm=NONE ctermfg=DarkBlue ctermbg=LightMagenta
set updatetime=5

function! HighlightWordUnderCursor()
	if getline(".")[col(".")-1] !~# '[[:punct:][:blank:]]' 
		exec 'match' 'DiffChange' '/\V\<'.expand('<cword>').'\>/' 
	else 
		match none 
	endif
endfunction

let blacklist = ['txt', 'md']
autocmd BufWritePre * if index(blacklist, &ft) < 0 | autocmd! CursorHold,CursorHoldI * call HighlightWordUnderCursor()

"SPLIT CURRENT WINDOW
nmap <leader>sh   :leftabove  vnew <CR>
nmap <leader>sl  :rightbelow vnew <CR>
nmap <leader>sk     :leftabove  new <CR>
nmap <leader>sj   :rightbelow new<CR>


"RESIZE WINDOWS
nnoremap <S-Left> :vertical resize -2<CR>
nnoremap <S-Right> :vertical resize +2<CR>
nnoremap <S-Up> : resize +2<CR>
nnoremap <S-Down> : resize -2<CR>


"NAVIGATE WINDOWS
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

"FILETYPES
augroup filetype_python
	autocmd!
	autocmd FileType python set noexpandtab tabstop=4 shiftwidth=4
	autocmd FileType python set foldmethod=indent
	autocmd FileType python iabbrev <buffer>sop print()<ESC>i
augroup END


augroup filetype_vim
	autocmd!
	autocmd FileType vim set noexpandtab tabstop=4 shiftwidth=4
	autocmd FileType vim set foldmethod=indent
augroup END

".jsxPLUGINS
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin("~/.vim/plugged")
Plug 'ctrlpvim/ctrlp.vim'
Plug 'scrooloose/nerdcommenter'
Plug 'vim-scripts/auto-pairs-gentle'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'dhruvasagar/vim-table-mode'
call plug#end()
