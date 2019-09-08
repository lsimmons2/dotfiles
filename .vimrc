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
syntax on "enables code coloring
set ignorecase "Case-insensitive searching.
set smartcase "But case-sensitive if expression contains a capital letter.
"setlocal spell spelllang=en_us


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
nnoremap <C-t> :tabnew<CR>
nnoremap <C-]> :tabnext<CR>
nnoremap <C-[> :tabprevious<CR>
inoremap <expr> <C-j> ((pumvisible())?("\<C-n>"):("j")) "allow C-j and C-k to scroll in autocomplete windows
inoremap <expr> <C-k> ((pumvisible())?("\<C-p>"):("k"))
nnoremap <leader>m :GoImports<CR>


"LANGUAGE-SPECIFIC MAPPINGS
autocmd FileType java inoremap psvm public static void main(String[] args) {}<ESC>i<CR><CR><ESC>kcc
autocmd FileType java inoremap psvm public static void main(String[] args) {}<ESC>i<CR><CR><ESC>kcc
autocmd FileType go inoremap sop fmt.Println()<ESC>i


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


"STATUSLINE
set statusline=%f       "tail of the filename
set statusline+=%h      "help file flag
set statusline+=%m      "modified flag
set statusline+=%r      "read only flag
set statusline+=%=      "left/right separator
set statusline+=Line:%-2l/%-6L "cursor line/total lines
hi StatusLine ctermfg=234 guifg=#1c1c1c ctermbg=107 guibg=#87af5f
hi StatusLineNC ctermfg=234 guifg=#1c1c1c ctermbg=245 guibg=#8a8a8a


"NETRW
let g:netrw_list_hide='.*\.pyc'
set timeoutlen=1000 ttimeoutlen=0
let g:netrw_banner = 0


"PLUGINS
call plug#begin("~/.vim/plugged")
Plug 'fatih/vim-go'
Plug 'neoclide/coc.nvim', {'do': 'yarn install --frozen-lockfile'}
Plug 'ctrlpvim/ctrlp.vim'
Plug 'scrooloose/nerdcommenter'
Plug 'vim-scripts/auto-pairs-gentle'
call plug#end()


"COC STUFF
set hidden " if hidden is not set, TextEdit might fail. - "Vim's windowing is basically unusable without hidden."
set cmdheight=2 " Better display for messages
set updatetime=300 " Smaller updatetime for CursorHold & CursorHoldI
set shortmess+=c " don't give |ins-completion-menu| messages.
set signcolumn=yes " always show signcolumns

" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
                        \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

nmap <silent> [e <Plug>(coc-diagnostic-prev)
nmap <silent> ]e <Plug>(coc-diagnostic-next)
nmap <silent> <leader>d <Plug>(coc-definition)
nmap <silent> <leader>t <Plug>(coc-type-definition)
nmap <silent> <leader>r <Plug>(coc-references)


""VIM-GO
let g:go_fmt_experimental = 1 " since vim-go default formatting clears fold state
let g:go_info_mode='gopls'
let g:go_def_mapping_enabled=0 "disable default mapping for GoDef
let g:go_code_completion_enabled = 0
nnoremap <leader>i :GoImports<CR>
nnoremap <C-i> :GoInfo<CR>
nnoremap <leader>d :GoDef<CR>
nnoremap Q :GoDocBrowser<CR>


"CTRL-P
let g:ctrlp_custom_ignore = '\v[\/](node_modules|venv|target|dist|.git|build)|(\.(swp|pyc|git|svn|class))$'
nnoremap <C-b> :CtrlPMRU<CR>


"NERDCOMMENT
nnoremap <leader>c :call NERDComment('n',"toggle")<CR>
vnoremap <leader>c :call NERDComment('n',"toggle")<CR>
let g:NERDCreateDefaultMappings = 0


"https://github.com/codegangsta/dotfiles/blob/master/vim/.vimrc#L108
