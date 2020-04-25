"""VANILLA OPTIONS
set nobackup
set nowb
set noswapfile
let mapleader=" "
filetype plugin indent on
set backspace=indent,eol,start
set ic
set expandtab
set autoindent
set nocompatible
set ruler
"set foldmethod=syntax
"set foldopen-=hor
set nofoldenable
syntax on "enables code coloring
set ignorecase "Case-insensitive searching.
set smartcase "But case-sensitive if expression contains a capital letter.
"setlocal spell spelllang=en_us
set wildmode=longest,list,full
set wildmenu
map <leader>2 mqgg=G'qzz



"""VANILLA MAPPINGS
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
inoremap <C-a> <ESC>0a
inoremap <C-d> <ESC>lxi
nnoremap S :w<CR>
nnoremap <leader>S :wa<CR>
nnoremap <leader>l :set invnumber<CR> " toggle lines
nnoremap <leader>h :set hls!<CR> " toggle search highlight
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
nnoremap <leader>ev :rightbelow vnew $MYVIMRC<cr>




"""LANGUAGE-SPECIFIC MAPPINGS/SETTINGS
"https://www.reddit.com/r/vim/comments/99ylz8/confused_about_the_difference_between_tabstop_and/
autocmd FileType java inoremap <buffer>psvm public static void main(String[] args) {}<ESC>i<CR><CR><ESC>kcc
autocmd FileType java inoremap <buffer>sop System.out.println();<ESC>hi
autocmd FileType java set tabstop=4
autocmd FileType java set shiftwidth=4

autocmd FileType python set noexpandtab tabstop=4 shiftwidth=4
autocmd FileType python set foldmethod=indent
autocmd FileType python inoremap <buffer>sop print()<ESC>i
autocmd FileType python inoremap <buffer>stw st.write()<ESC>i

autocmd FileType go set tabstop=4
autocmd FileType go set shiftwidth=4
autocmd FileType go set foldmethod=syntax
autocmd FileType go inoremap <buffer>sop fmt.Printf()<ESC>i
autocmd FileType go inoremap <buffer>sff fmt.Sprintf()<ESC>i"%s", 
autocmd FileType go inoremap <buffer>sleep time.Sleep(*time.Millisecond)<ESC>bbbba 

autocmd FileType html set foldmethod=indent
autocmd FileType html set tabstop=4
autocmd FileType html set shiftwidth=4
autocmd FileType html set noexpandtab
autocmd FileType html inoremap <buffer>sop console.log();<ESC>hi

autocmd FileType css set foldmethod=syntax
autocmd FileType css set tabstop=4
autocmd FileType css set shiftwidth=4
autocmd FileType css set noexpandtab

autocmd FileType javascript set foldmethod=indent
autocmd FileType javascript inoremap <buffer>sop console.log();<ESC>hi
autocmd FileType javascript set noexpandtab tabstop=2 shiftwidth=2
autocmd FileType javascript inoremap <buffer>ffor for (let i = 0; i < .length; i++){<CR>}<ESC>kwwwwwwwwwi

autocmd FileType text set foldmethod=indent
autocmd FileType text set tabstop=2
autocmd FileType text set shiftwidth=2
autocmd FileType text set complete+=k
autocmd FileType text set dictionary=/home/leo/.10k.txt
autocmd FileType text set completeopt+=menuone,noselect
let g:mucomplete#enable_auto_at_startup = 1

autocmd FileType yaml set expandtab
autocmd FileType yaml set shiftwidth=2


"""FUNCTIONS AND THEIR MAPPINGS
function! DisplayPUM()
  echom "in this func"
endfunction
autocmd InsertCharPre echom call DisplayPUM()

"highlighting
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

autocmd! CursorHold,CursorHoldI * call HighlightWordUnderCursor()

function Meow()
  return "Meow string!"
endfunction



"""STATUSLINE
set laststatus=2        "have statusline always show (even with single window)
set statusline=%f       "tail of the filename
set statusline+=%h      "help file flag
set statusline+=%m      "modified flag
set statusline+=%r      "read only flag
set statusline+=%=      "left/right separator
set statusline+=Line:%-2l/%-6L "cursor line/total lines
hi StatusLine ctermfg=234 guifg=#1c1c1c ctermbg=107 guibg=#87af5f
hi StatusLineNC ctermfg=234 guifg=#1c1c1c ctermbg=245 guibg=#8a8a8a


"""NETRW
let g:netrw_list_hide='.*\.pyc'
set timeoutlen=1000 ttimeoutlen=0
let g:netrw_banner = 0



"""PLUGINS
call plug#begin("~/.vim/plugged")
Plug 'maxmellon/vim-jsx-pretty'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'scrooloose/nerdcommenter'
Plug 'vim-scripts/auto-pairs-gentle'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'dhruvasagar/vim-table-mode'
Plug 'lifepillar/vim-mucomplete'
call plug#end()

""""VIM-TABLE-MODE
nnoremap <leader>1 :TableModeToggle<CR>
nnoremap <leader>sv :source ~/.vimrc<CR> :e<CR>


""""CTRL-P
let g:ctrlp_custom_ignore = '\v[\/](node_modules|venv|target|dist|.git|build)|(\.(swp|pyc|git|svn|class|csv|tsv|txt|american-english))$'
nnoremap <C-b> :CtrlPMRU<CR>



""""NERDCOMMENT
nnoremap <leader>c :call NERDComment('n',"toggle")<CR>
vnoremap <leader>c :call NERDComment('n',"toggle")<CR>

let g:NERDAltDelims_javascript = 1
let g:NERDCreateDefaultMappings = 0
let g:NERDCustomDelimiters={
	\ 'javascript': { 'left': '//', 'right': '', 'leftAlt': '{/*', 'rightAlt': '*/}' },
\}
map <leader>3 <plug>NERDCommenterAltDelims


"""OTHER
"https://medium.com/@crashybang/supercharge-vim-with-fzf-and-ripgrep-d4661fc853d2
nnoremap <leader>f :Find 
command! -bang -nargs=* Find call fzf#vim#grep('rg --column --line-number --no-heading --fixed-strings --ignore-case --no-ignore --hidden --follow --glob "!.git/*" --color "always" '.shellescape(<q-args>), 1, <bang>0)
nnoremap <C-m> :BufferTags<CR>
command! -bang -nargs=* BufferTags call fzf#vim#buffer_tags("")


"""NOTES
"https://github.com/codegangsta/dotfiles/blob/master/vim/.vimrc#L108
"https://github.com/kien/ctrlp.vim/issues/51
"https://realpython.com/vim-and-python-a-match-made-in-heaven/#lets-make-an-ide
"MANUALLY
"- install coc correctly
"- :CocInstall coc-word


"folding in this file
"" vim:fdm=expr:fdl=0
"" vim:fde=getline(v\:lnum)=~'^""'?'>'.(matchend(getline(v\:lnum),'""*')-2)\:'='
