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
"
"https://webpack.js.org/guides/development/#adjusting-your-text-editor
set backupcopy=yes
" make n/N always go in the same direction
nnoremap <expr> n 'Nn'[v:searchforward] . "zv"
nnoremap <expr> N 'nN'[v:searchforward] . "zv"


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
nnoremap <leader>w :e %:h<CR>
nnoremap <leader>b :b#<CR>
vnoremap // y/<C-R>"<CR>  " search visually selected text 
nnoremap <leader>y "+y
nnoremap <leader>p "+p
vnoremap <leader>y "+y
vnoremap <leader>p "+p
nnoremap <leader>Y "+Y
nnoremap <leader>P "+P
vnoremap $ $h
nnoremap <leader>t :tabnew%<CR><C-o>
nnoremap <leader>] :tabnext<CR>
nnoremap <leader>[ :tabprevious<CR>
inoremap <expr> <Tab> pumvisible() ? coc#_select_confirm() : "<Tab>"
nnoremap <leader>vs :source ~/.vimrc<CR>
inoremap <Left>a á
inoremap <Left>e é
inoremap <Left>o ó
inoremap <Left>u ú
inoremap <Left>i í
inoremap <Left>n ñ
" "THINGS TO APPEND"
nnoremap <leader>ac A <C-k>OK<ESC>
nnoremap <leader>ax A <C-k>XX<ESC>
"INSERT TIME
nnoremap <leader>k A<C-r>=strftime('%m.%d.%Y')<CR><ESC>


" "OPTIONS"
nnoremap <leader>ol :set invnumber<CR> " toggle lines
nnoremap <leader>oh :set hls!<CR> " toggle search highlight
nnoremap <leader>os :set spell!<CR> " toggle search highlight

" STATUSLINE
set laststatus=2        "have statusline always show (even with single window)
set statusline=%f       "tail of the filename
set statusline+=%h      "help file flag
set statusline+=%m      "modified flag
set statusline+=%r      "read only flag
set statusline+=%=      "left/right separator
set statusline+=Line:%-2l/%-6L "cursor line/total lines
"set statusline+=column:\ %c, " to see column


"NETRW
let g:netrw_list_hide='.*\.pyc'
set timeoutlen=1000 ttimeoutlen=0
let g:netrw_banner = 0


"HIGHLIGHTING
hi Search cterm=NONE ctermfg=Black ctermbg=DarkGreen
hi DiffChange cterm=NONE ctermfg=DarkBlue ctermbg=LightMagenta
highlight Folded ctermbg=None ctermfg=grey
set updatetime=5

"SPLIT CURRENT WINDOW
nmap <leader>sh   :leftabove  vsplit<CR>
nmap <leader>sl  :rightbelow vsplit<CR>
nmap <leader>sk     :leftabove  split<CR>
nmap <leader>sj   :rightbelow split<CR>


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

