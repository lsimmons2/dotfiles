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
nnoremap <leader>b :b#<CR>
vnoremap // y/<C-R>"<CR>  " search visually selected text 
nnoremap <leader>y "+y
nnoremap <leader>p "+p
vnoremap <leader>y "+y
vnoremap <leader>p "+p
nnoremap <leader>Y "+Y
nnoremap <leader>P "+P
vnoremap $ $h
nnoremap <leader>t :tabnew<CR>
nnoremap <leader>] :tabnext<CR>
nnoremap <leader>[ :tabprevious<CR>
inoremap <expr> <C-j> ((pumvisible())?("\<C-n>"):("\<C-j>")) "allow C-j and C-k to scroll in autocomplete windows
inoremap <expr> <C-k> ((pumvisible())?("\<C-p>"):("\<C-k>"))
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

let blacklist = ['text', 'markdown']
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
	autocmd FileType python inoremap <buffer>sop print()<ESC>i
augroup END

augroup filetype_vim
	autocmd!
	autocmd FileType vim set noexpandtab tabstop=4 shiftwidth=4
	autocmd FileType vim set foldmethod=indent
augroup END

augroup filetype_text
	autocmd!
	autocmd FileType sh set noexpandtab tabstop=4 shiftwidth=4
	autocmd FileType sh set foldmethod=indent
	autocmd FileType sh inoremap <buffer>sop echo 
augroup END

augroup filetype_javascript
	autocmd!
	autocmd FileType javascript set noexpandtab tabstop=4 shiftwidth=4
	autocmd FileType javascript set foldmethod=indent
	"autocmd FileType javascript inoremap <buffer>sop logger.Debug();<ESC>hi
	autocmd FileType javascript inoremap <buffer>sop console.log();<ESC>hi
augroup END

augroup filetype_typescriptjavascript
	autocmd!
	autocmd FileType typescriptreact set noexpandtab tabstop=4 shiftwidth=4
	autocmd FileType typescriptreact set foldmethod=indent
	"autocmd FileType typescriptreact inoremap <buffer>sop console.log();<ESC>hi
	autocmd FileType typescriptreact inoremap <buffer>sop logger.Debug();<ESC>hi
	autocmd FileType typescriptreact inoremap <buffer>ffor for (let i = 0; i < .length; i++) {}<ESC>i<CR><ESC>kwwwwwwwwwi
augroup END

augroup filetype_shell
	autocmd!
	autocmd FileType text set noexpandtab tabstop=4 shiftwidth=4
	autocmd FileType text set foldmethod=indent
augroup END

augroup filetype_makefile
	autocmd!
	autocmd FileType make set noexpandtab tabstop=4 shiftwidth=4
	autocmd FileType make set foldmethod=indent
augroup END

augroup filetype_dockerfile
	autocmd BufEnter Dockerfile* :setlocal filetype=dockerfile
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
Plug 'leafgarland/typescript-vim'
Plug 'alvan/vim-closetag'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'ludovicchabant/vim-gutentags'
Plug 'preservim/tagbar'
Plug 'airblade/vim-gitgutter'
call plug#end()

"COC.NVIM
nmap <silent> [e <Plug>(coc-diagnostic-prev)
nmap <silent> ]e <Plug>(coc-diagnostic-next)

"GUTENTAGS
set statusline+=%{gutentags#statusline()}
let g:gutentags_ctags_exclude = [
	  \ '*.git', '*.svg', '*.hg',
	  \ '*/tests/*',
	  \ 'build',
	  \ 'dist',
	  \ '*sites/*/files/*',
	  \ 'bin',
	  \ 'node_modules',
	  \ 'bower_components',
	  \ 'cache',
	  \ 'compiled',
	  \ 'docs',
	  \ 'example',
	  \ 'bundle',
	  \ 'vendor',
	  \ '*.md',
	  \ '*-lock.json',
	  \ '*.lock',
	  \ '*bundle*.js',
	  \ '*build*.js',
	  \ '.*rc*',
	  \ '*.json',
	  \ '*.min.*',
	  \ '*.map',
	  \ '*.bak',
	  \ '*.zip',
	  \ '*.pyc',
	  \ '*.class',
	  \ '*.sln',
	  \ '*.Master',
	  \ '*.csproj',
	  \ '*.tmp',
	  \ '*.csproj.user',
	  \ '*.cache',
	  \ '*.pdb',
	  \ 'tags*',
	  \ 'cscope.*',
	  \ '*.css',
	  \ '*.less',
	  \ '*.scss',
	  \ '*.exe', '*.dll',
	  \ '*.mp3', '*.ogg', '*.flac',
	  \ '*.swp', '*.swo',
	  \ '*.bmp', '*.gif', '*.ico', '*.jpg', '*.png',
	  \ '*.rar', '*.zip', '*.tar', '*.tar.gz', '*.tar.xz', '*.tar.bz2',
	  \ '*.pdf', '*.doc', '*.docx', '*.ppt', '*.pptx',
	  \ ]


"TAGBAR
nnoremap <leader>ob :TagbarToggle<CR>

"VIM-CLOSETAG
let g:closetag_filenames = '*.html,*.xhtml,*.phtml,*.tsx,*.jsx'

" FZF
"mapping to OOB fzf.vim functions
nnoremap <C-p> :Files<CR>
nnoremap <C-m> :BTags<CR>
nnoremap <leader>m :Tags<CR>

"these examples taken/modified from https://github.com/junegunn/fzf.vim
command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always --ignore-file=/home/leo/.gitignore_global -F --smart-case -- '.shellescape(<q-args>), 1,
  \   fzf#vim#with_preview({ 'options': '-e' }), <bang>0)
nnoremap <leader>/ :Rg<CR>

command! -bang -nargs=* RgGlobal
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always --ignore-file=/home/leo/.gitignore_global -F --smart-case --glob=*.py --glob=*.go --glob=*.js --glob=*.jsx --glob=*.ts --glob=*.tsx --glob=*.sh /home/leo -e '.shellescape(<q-args>), 1,
  \   fzf#vim#with_preview({ 'options': '-e' }), <bang>0)
nnoremap <leader>? :RgGlobal<CR>

"defining own function with fzf (not fzf.vim) package
nnoremap <leader>l :call fzf#run({'source': 'fasd -d -l', 'sink': 'lcd'})<CR>

"overwriting fzf.vim BLines function to have -e/"exact match" option
command! -bang -nargs=? -complete=dir BLines
	\ call fzf#vim#buffer_lines(<q-args>, {'options': ['-e']}, <bang>0)
nnoremap / :BLines<CR>


"let g:fzf_tags_command = 'ctags -R'

"CTRL-P
nnoremap <C-b> :CtrlPMRU<CR>
let g:ctrlp_switch_buffer = 0


" NERDCOMMENTER
let g:NERDCreateDefaultMappings = 0
nnoremap <leader>c :call NERDComment(0,"toggle")<CR>
vnoremap <leader>c :call NERDComment(0,"toggle")<CR>

" TABLE MODE
let g:table_mode_disable_mappings = 1
let g:table_mode_disable_tableize_mappings = 1
let g:table_mode_map_prefix = "<Leader>xxxxxxxxxxxxxxxxxx"

"VIM GUTTER
nnoremap <leader>oq :GitGutterToggle<CR>
nmap ]q <Plug>(GitGutterNextHunk)
nmap [q <Plug>(GitGutterPrevHunk)
nmap q] <Plug>(GitGutterNextHunk)
nmap q[ <Plug>(GitGutterPrevHunk)
nmap qs <Plug>(GitGutterStageHunk)
nmap qu <Plug>(GitGutterUndoHunk)
nmap qp <Plug>(GitGutterPreviewHunk)
let g:gitgutter_preview_win_floating = 1
