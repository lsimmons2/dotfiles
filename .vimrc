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

" Show tabs as '▸' followed by spaces
"set listchars=tab:▸\ ,trail:·
" Enable showing these characters
"set list


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
"inoremap <expr> <C-j> ((pumvisible())?("\<C-n>"):("\<C-j>")) "allow C-j and C-k to scroll in autocomplete windows
"inoremap <expr> <C-k> ((pumvisible())?("\<C-p>"):("\<C-k>"))
"inoremap <expr> <Tab> pumvisible() ? "\<C-n>\<C-p>\<Esc>" : "\<Tab>"
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
nnoremap <leader>og :GitGutterToggle<CR>

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

function! HighlightWordUnderCursor()
	if getline(".")[col(".")-1] !~# '[[:punct:][:blank:]]' 
		exec 'match' 'DiffChange' '/\V\<'.expand('<cword>').'\>/' 
	else 
		match none 
	endif
endfunction

let blacklist = ['text', 'markdown', 'lisp']
autocmd BufWritePre * if index(blacklist, &ft) < 0 | autocmd! CursorHold,CursorHoldI * call HighlightWordUnderCursor()

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

"FILETYPES
augroup filetype_python
	autocmd!
	autocmd FileType python set tabstop=4 shiftwidth=4
	autocmd FileType python set foldmethod=indent
	autocmd FileType python inoremap <buffer>sop print()<ESC>i
augroup END

augroup filetype_go
	autocmd!
	autocmd FileType go set tabstop=4 shiftwidth=4
	autocmd FileType go set foldmethod=indent
	autocmd FileType go inoremap <buffer>sop print()<ESC>i
augroup END

augroup filetype_vim
	autocmd!
	autocmd FileType vim set noexpandtab tabstop=4 shiftwidth=4
	autocmd FileType vim set foldmethod=indent
augroup END

augroup filetype_shell
	autocmd!
	autocmd FileType sh set noexpandtab tabstop=4 shiftwidth=4
	autocmd FileType sh set foldmethod=indent
	autocmd FileType sh inoremap <buffer>sop echo 
augroup END

function FoldText()
  return "fold"
endfunction

augroup filetype_text
	autocmd!
	autocmd FileType text setlocal noexpandtab tabstop=4 shiftwidth=4
	autocmd FileType text setlocal foldmethod=indent
	autocmd FileType text setlocal foldtext=FoldText()
	autocmd FileType text setlocal foldminlines=0
	autocmd FileType text nnoremap <leader>j VU<ESC>A><ESC>
	"autocmd FileType text setlocal wrap breakindent linebreak
	autocmd FileType text setlocal wrap breakindent linebreak breakindentopt=shift:2,min:2
augroup END


augroup filetype_javascript
	autocmd!
	autocmd FileType javascript set noexpandtab tabstop=4 shiftwidth=4
	autocmd FileType javascript set foldmethod=indent
	"autocmd FileType javascript inoremap <buffer>sop logger.Debug();<ESC>hi
	autocmd FileType javascript inoremap <buffer>sop console.log();<ESC>hi
augroup END

augroup filetype_typescript
	autocmd!
	autocmd FileType typescript set noexpandtab tabstop=4 shiftwidth=4
	autocmd FileType typescript set foldmethod=indent
	"autocmd FileType typescript inoremap <buffer>sop logger.Debug();<ESC>hi
	autocmd FileType typescript inoremap <buffer>sop console.log();<ESC>hi
	autocmd FileType typescript inoremap <buffer>ffor for (let i = 0; i < .length; i++) {}<ESC>i<CR><ESC>kwwwwwwwwwi
augroup END

augroup filetype_typescriptjavascript
	autocmd!
	autocmd FileType typescriptreact set noexpandtab tabstop=4 shiftwidth=4
	autocmd FileType typescriptreact set foldmethod=indent
	autocmd FileType typescriptreact inoremap <buffer>sop console.log();<ESC>hi
	"autocmd FileType typescriptreact inoremap <buffer>sop logger.Debug();<ESC>hi
	autocmd FileType typescriptreact inoremap <buffer>ffor for (let i = 0; i < .length; i++) {}<ESC>i<CR><ESC>kwwwwwwwwwi
augroup END

augroup filetype_makefile
	autocmd!
	autocmd FileType make set noexpandtab tabstop=4 shiftwidth=4
	autocmd FileType make set foldmethod=indent
augroup END

augroup filetype_dockerfile
	autocmd BufEnter Dockerfile* :setlocal filetype=dockerfile
augroup END

augroup filetype_json
    autocmd!
    autocmd FileType json setlocal conceallevel=0
augroup END

".jsxPLUGINS
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin("~/.vim/plugged")
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

"Plug 'SirVer/ultisnips'
"Plug 'honza/vim-snippets'

Plug 'Yggdroot/indentLine'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'scrooloose/nerdcommenter'
Plug 'vim-scripts/auto-pairs-gentle'
Plug 'dhruvasagar/vim-table-mode'
Plug 'alvan/vim-closetag' "to autoclose html tags
Plug 'airblade/vim-gitgutter'

"Plug 'ludovicchabant/vim-gutentags'
"Plug 'preservim/tagbar'

Plug 'integralist/vim-mypy'
Plug 'pappasam/coc-jedi', { 'do': 'yarn install --frozen-lockfile && yarn build', 'branch': 'main' }

"Plug 'leafgarland/typescript-vim'
"Plug 'tell-k/vim-autopep8'
"Plug 'github/copilot.vim'
"Plug 'jparise/vim-graphql'
call plug#end()

"SNIPPETS
"let g:UltiSnipsExpandTrigger="<C-space>"
"let g:UltiSnipsListSnippets="<C-4>"
let g:UltiSnipsJumpBackwardTrigger="<C-z>"
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"

"COC.NVIM
nmap <silent> [e <Plug>(coc-diagnostic-prev)
nmap <silent> ]e <Plug>(coc-diagnostic-next)
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gt <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
nmap <silent> <leader>r <Plug>(coc-rename)
nmap <silent> <leader>i :CocCommand editor.action.organizeImport<CR>


autocmd FileType python let b:coc_root_patterns = ['.git', '.env', 'venv', '.venv', 'setup.cfg', 'setup.py', 'pyproject.toml', 'pyrightconfig.json']

"GUTENTAGS
"set statusline+=%{gutentags#statusline()}
let g:gutentags_project_root = [".ROOT_NOTES_MARKER"]
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
command! -bang -nargs=* MyTags call fzf#vim#tags(<q-args>, {'options':'--nth 1'}, <bang>0)
nnoremap <leader>m :MyTags<CR>

"these examples taken/modified from https://github.com/junegunn/fzf.vim
"https://github.com/junegunn/fzf.vim/issues/346#issuecomment-288483704
command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always --ignore-file=/home/leo/.gitignore_global -F --smart-case -- '.shellescape(<q-args>), 1,
  \   fzf#vim#with_preview({'options': '--delimiter : --nth 4..'}), <bang>0)
nnoremap <leader>/ :Rg<CR>

command! -bang -nargs=* RgExact
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always --smart-case -- '.shellescape(<q-args>), 1,
  \   fzf#vim#with_preview(), <bang>0)
nnoremap <leader>? :RgExact<CR>

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
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_mruf_relative = 1
let g:ctrlp_custom_ignore = 'node_modules\|DS_Store\|git'


" NERDCOMMENTER
let g:NERDCreateDefaultMappings = 0
nnoremap <leader>c :call nerdcommenter#Comment(0,"toggle")<CR>
vnoremap <leader>c :call nerdcommenter#Comment(0,"toggle")<CR>

" TABLE MODE
"let g:table_mode_disable_mappings = 1
"let g:table_mode_disable_tableize_mappings = 1
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


"COPILOT
"nnoremap <leader>ocp :Copilot enable<CR>
"nnoremap <leader>ocn :Copilot disable<CR>
"autocmd FileType text :Copilot disable

"python notes:
"atow (06.02.2024), using coc-pyright for type-checking and formatting for python, and coc-jedi for go-to-def and go-to-references
"


"SSH/SCP

" Disable reloading and unnecessary SCP actions after saving when editing via scp://
"autocmd BufWritePost scp://* silent! !scp -q %:p leo@10.0.0.216:%:p:h/
"autocmd BufWritePost scp://* silent! !scp -q %:p leo@10.0.0.216:%:p:h/
"autocmd BufReadPost scp://* setlocal noautoread
" ## added by OPAM user-setup for vim / base ## d611dd144a5764d46fdea4c0c2e0ba07 ## you can edit, but keep this line
let s:opam_share_dir = system("opam var share")
let s:opam_share_dir = substitute(s:opam_share_dir, '[\r\n]*$', '', '')

let s:opam_configuration = {}

function! OpamConfOcpIndent()
  execute "set rtp^=" . s:opam_share_dir . "/ocp-indent/vim"
endfunction
let s:opam_configuration['ocp-indent'] = function('OpamConfOcpIndent')

function! OpamConfOcpIndex()
  execute "set rtp+=" . s:opam_share_dir . "/ocp-index/vim"
endfunction
let s:opam_configuration['ocp-index'] = function('OpamConfOcpIndex')

function! OpamConfMerlin()
  let l:dir = s:opam_share_dir . "/merlin/vim"
  execute "set rtp+=" . l:dir
endfunction
let s:opam_configuration['merlin'] = function('OpamConfMerlin')

let s:opam_packages = ["ocp-indent", "ocp-index", "merlin"]
let s:opam_available_tools = []
for tool in s:opam_packages
  " Respect package order (merlin should be after ocp-index)
  if isdirectory(s:opam_share_dir . "/" . tool)
    call add(s:opam_available_tools, tool)
    call s:opam_configuration[tool]()
  endif
endfor
" ## end of OPAM user-setup addition for vim / base ## keep this line
" ## added by OPAM user-setup for vim / ocp-indent ## b66982e68cd87de093aa6b46e456c54a ## you can edit, but keep this line
if count(s:opam_available_tools,"ocp-indent") == 0
  source "/Users/leo/.opam/default/share/ocp-indent/vim/indent/ocaml.vim"
endif
" ## end of OPAM user-setup addition for vim / ocp-indent ## keep this line
