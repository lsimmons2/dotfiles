"""VANILLA OPTIONS/COMMANDS
set nobackup
set nowritebackup
set noswapfile
set backspace=indent,eol,start "non-vim behavior of backspace in insert mode
set autoindent "copy indent from currenlt line when using o,O, cr in insert mode
set nocompatible "don't behave like vi
set ruler "show line number of cursor
set foldopen-=hor "don't open folds with h,l
set nofoldenable "open all folds by default
set ignorecase "Case-insensitive searching.
set smartcase "But case-sensitive if expression contains a capital letter.
set wildmode=longest,list,full
set wildmenu
set nospell "disable spell-check by default
syntax on "enables code coloring
filetype plugin indent on
set number
set relativenumber
set incsearch
set cursorline
set hlsearch
set modelineexpr

"""VANILLA MAPPINGS
let mapleader=" "
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
nnoremap S :w<CR>
vnoremap // y/<C-R>"<CR>  " search visually selected text 
nnoremap <leader>S :wa<CR>
nnoremap <leader>e :e .<CR>
nnoremap <leader>b :b#<CR>
nnoremap <leader>y "+y
nnoremap <leader>p "+p
vnoremap <leader>y "+y
vnoremap <leader>p "+p
nnoremap <leader>Y "+Y
nnoremap <leader>P "+P
nnoremap <leader>t :tabnew<CR>
nnoremap <leader>] :tabnext<CR>
nnoremap <leader>[ :tabprevious<CR>
nnoremap <leader>ve :rightbelow vnew $MYVIMRC<cr> " split to the right with vimrc open
nnoremap <leader>vs :source ~/.vimrc<CR> :e<CR> " source vimrc and trigger filetype event for autocmds
"allow C-j and C-k to scroll in autocomplete windows
inoremap <expr> <C-j> ((pumvisible())?("\<C-n>"):("\<C-j>"))
inoremap <expr> <C-k> ((pumvisible())?("\<C-p>"):("\<C-k>"))
inoremap <TAB> <C-n>
inoremap <S-TAB> <C-p>
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
nnoremap <leader>2 mqgg=G'qzz
nnoremap <leader>ol :set invnumber<CR> " toggle lines
nnoremap <leader>or :set invrelativenumber<CR> " toggle relative lines
nnoremap <leader>os :set invspell<CR> " toggle spellcheck
nnoremap <leader>oh :set hls!<CR> " toggle search highlight
"nnoremap <leader>ac A <C-v>u2713<ESC>
"nnoremap <leader>ac A <C-v>u10102<ESC>
nnoremap <leader>ac A <C-k>OK<ESC>
nnoremap <leader>ax A <C-k>XX<ESC>

"""LANGUAGE-SPECIFIC MAPPINGS/SETTINGS
"https://www.reddit.com/r/vim/comments/99ylz8/confused_about_the_difference_between_tabstop_and/
augroup filetype_java
    autocmd!
    autocmd FileType java inoremap <buffer>psvm public static void main(String[] args) {}<ESC>i<CR><CR><ESC>kcc
    autocmd FileType java inoremap <buffer>sop System.out.println();<ESC>hi
    autocmd FileType java set tabstop=4
    autocmd FileType java set shiftwidth=4
augroup END

augroup filetype_python
    autocmd!
    "autocmd FileType python set noexpandtab tabstop=4 shiftwidth=4
    autocmd FileType python set expandtab
    autocmd FileType python set tabstop=4 shiftwidth=4
    autocmd FileType python set foldmethod=indent
    autocmd FileType python inoremap <buffer>sop print()<ESC>i
    autocmd FileType python inoremap <buffer>stw st.write()<ESC>i
    "autocmd FileType python setlocal foldmethod=expr foldexpr=getline(v:lnum)=~'^\\s*#'
    "autocmd FileType python let b:coc_suggest_disable = 1
    "autocmd FileType python let g:pymode_rope = 1
    "autocmd FileType python let g:pymode_rope_autoimport=1
    "autocmd FileType python let g:pymode_motion = 1
"augroup END

augroup filetype_go
    autocmd!
    autocmd FileType go set tabstop=4
    autocmd FileType go set shiftwidth=4
    autocmd FileType go set foldmethod=syntax
    autocmd FileType go inoremap <buffer>sopl log.Info("")<ESC>hi
    autocmd FileType go inoremap <buffer>sopf log.Infof("",)<ESC>hhi
    autocmd FileType go inoremap <buffer>sff fmt.Sprintf()<ESC>i"%s", 
augroup END

augroup filetype_html
    autocmd!
    autocmd FileType html set foldmethod=indent
    autocmd FileType html set tabstop=4
    autocmd FileType html set shiftwidth=4
    autocmd FileType html set noexpandtab
    autocmd FileType html inoremap <buffer>sop console.log();<ESC>hi
augroup END

augroup filetype_css
    autocmd!
    autocmd FileType css set foldmethod=syntax
    autocmd FileType css set tabstop=4
    autocmd FileType css set shiftwidth=4
    autocmd FileType css set noexpandtab
augroup END

augroup filetype_javascript
    autocmd!
    autocmd FileType javascript set foldmethod=indent
    autocmd FileType javascript inoremap <buffer>sop console.log();<ESC>hi
    autocmd FileType javascript set noexpandtab tabstop=2 shiftwidth=2
    autocmd FileType javascript inoremap <buffer>ffor for (let i = 0; i < .length; i++){<CR>}<ESC>kwwwwwwwwwi
augroup END

augroup filetype_text
    autocmd!
    autocmd FileType text set foldmethod=indent
    autocmd FileType text set tabstop=2
    autocmd FileType text set shiftwidth=2
    autocmd FileType text set complete+=k
    "autocmd FileType text set dictionary=/home/leo/.10k.txt
    autocmd FileType text setlocal spelllang=en_us
    "https://vi.stackexchange.com/a/16944/12658
    autocmd FileType text set norelativenumber
    autocmd FileType text set nonumber
    autocmd FileType text nnoremap <expr> j v:count == 0 ? 'gj' : "\<Esc>".v:count.'j'
    autocmd FileType text nnoremap <expr> k v:count == 0 ? 'gk' : "\<Esc>".v:count.'k'
    autocmd FileType text GitGutterDisable
augroup END

augroup filetype_yaml
    autocmd!
    autocmd FileType yaml set expandtab
    autocmd FileType yaml set shiftwidth=2
augroup END

augroup filetype_json
    autocmd!
    autocmd FileType json set expandtab
    autocmd FileType json set shiftwidth=4
augroup END

augroup filetype_help
    autocmd!
    autocmd FileType help wincmd L " open help windows in vertical split to the right
augroup END

augroup filetype_vim
    autocmd!
    autocmd FileType vim wincmd L " open help windows in vertical split to the right
    autocmd FileType vim set expandtab
    autocmd FileType vim set shiftwidth=4
    autocmd FileType vim GitGutterDisable
augroup END

"""HIGHLIGHTING
hi Search cterm=NONE ctermfg=Black ctermbg=DarkGreen
hi DiffChange cterm=NONE ctermfg=DarkBlue ctermbg=LightMagenta

"""FUNCTIONS AND THEIR MAPPINGS
"nnoremap ; :<C-P><CR>

"function! MessageWindow()
  "echom "in this func bro"
  "new
  "redir => messages_output
  "silent messages
  "redir END
  "silent put=messages_output
"endfunction
"autocmd FileType text nnoremap <leader>l call MessageWindow()

set updatetime=300
function! HighlightWordUnderCursor()
  if getline(".")[col(".")-1] !~# '[[:punct:][:blank:]]' 
    exec 'match' 'DiffChange' '/\V\<'.expand('<cword>').'\>/' 
  else 
    match none 
  endif
endfunction
autocmd! CursorHold,CursorHoldI * call HighlightWordUnderCursor()



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
Plug 'junegunn/seoul256.vim'
Plug 'vim-scripts/auto-pairs-gentle'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'dhruvasagar/vim-table-mode'
"Plug 'autozimu/LanguageClient-neovim', {
    "\ 'branch': 'next',
    "\ 'do': 'bash install.sh',
    "\ }
"Plug 'fatih/vim-go', {'do': ':GoUpdateBinaries'}
"folding in this file
Plug 'neoclide/coc.nvim', {'branch': 'release'}
"Plug 'prabirshrestha/async.vim'
"Plug 'prabirshrestha/vim-lsp'
"Plug 'prabirshrestha/asyncomplete.vim'
Plug 'airblade/vim-gitgutter'

"Plug 'dense-analysis/ale'

"Plug 'python-mode/python-mode', { 'for': 'python', 'branch': 'develop' }
call plug#end()


"let g:ale_fix_on_save = 1
"" Check Python files with flake8 and pylint.
"let b:ale_linters = ['flake8', 'pylint']
"" Fix Python files with autopep8 and yapf.
""let b:ale_fixers = ['yapf']
""let b:ale_fixers = {'python': ['yapf']}
"let g:ale_fixers = {
"\   'python': ['yapf'],
"\}

"if executable('pyls')
    "" pip install python-language-server
    "au User lsp_setup call lsp#register_server({
        "\ 'name': 'pyls',
        "\ 'cmd': {server_info->['pyls']},
        "\ 'whitelist': ['python'],
        "\ })
"endif

"function! s:on_lsp_buffer_enabled() abort
    "setlocal omnifunc=lsp#complete
    "setlocal signcolumn=yes
    "if exists('+tagfunc') | setlocal tagfunc=lsp#tagfunc | endif
    "nmap <buffer> gd <plug>(lsp-definition)
    "nmap <buffer> <f2> <plug>(lsp-rename)
    "" refer to doc to add more commands
"endfunction

"augroup lsp_install
    "au!
    "" call s:on_lsp_buffer_enabled only for languages that has the server registered.
    "autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
"augroup END


"inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
"inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
"inoremap <expr> <cr>    pumvisible() ? "\<C-y>" : "\<cr>"
"imap <c-space> <Plug>(asyncomplete_force_refresh)

"let g:LanguageClient_serverCommands = {
    "\ 'python': ['/home/leo/.local/bin/pyls'],
    "\ }
"set completefunc=LanguageClient#complete

"nnoremap <silent> <leader>d :call LanguageClient#textDocument_definition()<CR>
"nnoremap <silent> <leader>r :call LanguageClient#textDocument_references()<CR>
"nnoremap <silent> K :call LanguageClient#textDocument_hover()<CR>

""""GITGUTTER
"autocmd VimEnter * GitGutterDisable
nnoremap <leader>oq :GitGutterTo<CR>
nnoremap q] :GitGutterNextHunk<CR>:GitGutterPreviewHunk<CR>
nnoremap q[ :GitGutterPrevHunk<CR>:GitGutterPreviewHunk<CR>
nnoremap ]q :GitGutterNextHunk<CR>:GitGutterPreviewHunk<CR>
nnoremap [q :GitGutterPrevHunk<CR>:GitGutterPreviewHunk<CR>
nnoremap ql :GitGutterQuickFix<CR>
nnoremap qs :GitGutterStageHunk<CR>
nnoremap qp :GitGutterPreviewHunk<CR>
nnoremap qz :GitGutterFold<CR>
nnoremap qr :GitGutterUndoHunk<CR>


""""SEOUL256
color seoul256
set background=light
nnoremap <expr> <leader>oa &background == "light" ? ':set background=dark<cr>' : ':set background=light<cr>'

""""AUTOCOMPLETE
"autocmd FileType text set completeopt+=menuone,noselect
"autocmd FileType text set completeopt=menu,menuone,noinsert,longest
"inoremap <expr> <CR> pumvisible() ? "\<C-Y>" : "\<CR>"
"fun! AutoComplete()
"
"function! AutoComplete()
    "echom v:char
    "echom '\K'
    ""if v:char =~ '\K'
	""\ && getline('.')[col('.') - 4] !~ '\K'
	""\ && getline('.')[col('.') - 3] =~ '\K'
	""\ && getline('.')[col('.') - 2] =~ '\K' " last char
	""\ && getline('.')[col('.') - 1] !~ '\K'

	""call feedkeys("\<C-P>", 'n')
    ""end
"endfunction

"augroup autocompletion
    "autocmd!
    "autocmd InsertCharPre * call AutoComplete()
"augroup END


""""COC.NVIM
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)
set updatetime=300 " Smaller updatetime for CursorHold & CursorHoldI
set shortmess+=c " don't give |ins-completion-menu| messages.
nmap ]e <Plug>(coc-diagnostic-next)
nmap [e <Plug>(coc-diagnostic-prev)
nmap <leader>d <Plug>(coc-definition)
nmap <leader>q <Plug>(coc-type-definition)
"nnoremap <leader>r :echo "hola"<CR>
nmap <leader>r <Plug>(coc-references)
nmap <leader>f <Plug>(coc-format)
"let whitelist = ['python']
"autocmd BufWritePre * if index(whitelist, &ft) >= 0 | silent call CocActionAsync('organizeImport')
"autocmd CursorHold * silent call CocActionAsync('highlight')
"
"let blacklist = ['python']
"autocmd BufWritePre * if index(blacklist, &ft) < 0 | inoremap <silent><expr> <TAB>
     "\ pumvisible() ? "\<C-n>" :
     "\ <SID>check_back_space() ? "\<TAB>" :
     "\ coc#refresh()

"autocmd BufWritePre * if index(blacklist, &ft) < 0 | inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
 let col = col('.') - 1
 return !col || getline('.')[col - 1]  =~# '\s'
endfunction

"autocmd CursorHold * silent call CocActionAsync('highlight')
""""VIM-GO
let g:go_fmt_experimental = 1 " since vim-go default formatting clears fold state
let g:go_def_mapping_enabled = 0
let g:go_code_completion_enabled = 0
let g:go_doc_keywordprg_enabled = 0
au FileType go nmap <leader>l <Plug>(go-imports)
au FileType go nmap <leader>i <Plug>(go-info)
au FileType go nmap <C-p> :GoDeclsDir<CR>
"au FileType go nmap <leader>m :GoDeclsDir<CR>

""""MUCOMPLETE
"set completeopt+=menuone,noinsert
"set completeopt+=noselect
"set completeopt+=noinsert
"let g:mucomplete#enable_auto_at_startup = 1

""""JEDI-VIM
"let g:jedi#completions_command = "<TAB>"
"set noshowmode
"let g:jedi#show_call_signatures_delay = "0"
"let g:jedi#show_call_signatures = "0"
"let g:jedi#goto_command = "gd"

""""VIM-TABLE-MODE
"let g:table_mode_disable_mappings = 1
map <Plug>(table-mode-tableize) <Nop>
map <Plug>(table-mode-tableize-delimiter) <Nop>
nnoremap <leader>ot :TableModeToggle<CR>
" because there isn't an option to remove default table-mode mappings
let g:table_mode_map_prefix='<Leader><Up>'

""""FZF
nnoremap <C-m> :call fzf#vim#buffer_tags("")<CR>
nnoremap / :BLines<CR>
nnoremap <C-a> :Tags<CR>
nnoremap <Backspace>c :Commands<CR>
nnoremap <Backspace>h :History:<CR>
nnoremap <Backspace>m :Tags<CR>
let g:fzf_colors =
\ { 'fg':      ['fg', 'Normal'],
  \ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'Comment'],
  \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
  \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
  \ 'hl+':     ['fg', 'Statement'],
  \ 'info':    ['fg', 'PreProc'],
  \ 'border':  ['fg', 'Ignore'],
  \ 'prompt':  ['fg', 'Conditional'],
  \ 'pointer': ['fg', 'Exception'],
  \ 'marker':  ['fg', 'Keyword'],
  \ 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment'] }
"function! s:format_mark(line)
    "return a:line
  ""return substitute(a:line, '\S', '\=s:yellow(submatch(0), "Number")', '')
"endfunction

"function! Change_sink(lines)
  "if len(a:lines) < 2
    "return
  "endif
  "let cmd = s:action_for(a:lines[0])
  "if !empty(cmd)
    "execute 'silent' cmd
  "endif
  "execute 'normal! `'.matchstr(a:lines[1], '\S').'zz'
"endfunction

"function! Changes(...)
  "redir => cout
  "silent changes
  "redir END
  "let list = split(cout, "\n")
  "return s:fzf('changes', {
  "\ 'source':  extend(list[0:0], map(list[1:], 's:format_mark(v:val)')),
  "\ 'sink*':   Change_sink,
  "\ 'options': '+m -x --ansi --tiebreak=index --header-lines 1 --tiebreak=begin --prompt "Changes> "'}, a:000)
"endfunction

"nnoremap <C-n> :call Changes("")<CR>
""nnoremap <C-n> :call fzf#run({'source':'','sink': 'edit'})<CR>
"nnoremap <C-n> :call fzf#run({'source':'','sink': 'edit'})<CR>

"nnoremap <C-n> :BLines<CR>
"nnoremap <C-\<> :BLines<CR>
"nnoremap <C-m> :BufferTags<CR>
"command! -bang -nargs=* BufferTags call fzf#vim#buffer_tags("")

""""CTRL-P
let g:ctrlp_custom_ignore = '\v[\/](node_modules|venv|target|dist|.git|build)|(\.(swp|pyc|git|svn|class|csv|tsv|txt|american-english))$'
let g:ctrlp_switch_buffer = 0
nnoremap <C-b> :CtrlPMRU<CR>
"nnoremap <C-m> :echo "hola from C-m"<CR>

""""NERDCOMMENT
nnoremap <leader>c :call NERDComment('n',"toggle")<CR>
vnoremap <leader>c :call NERDComment('n',"toggle")<CR>
let g:NERDCreateDefaultMappings = 0
let g:NERDCustomDelimiters={
			\ 'javascript': { 'left': '//', 'right': '', 'leftAlt': '{/*', 'rightAlt': '*/}' },
			\}
map <leader>oc <Plug>NERDCommenterAltDelims




"""OTHER
"https://medium.com/@crashybang/supercharge-vim-with-fzf-and-ripgrep-d4661fc853d2
nnoremap <C-f> :Find 
command! -bang -nargs=* Find call fzf#vim#grep('rg --column --line-number --no-heading --fixed-strings --ignore-case --no-ignore --hidden --follow --glob "!.git/*" --glob "!*tags" --glob "!*venv*" --color "always" '.shellescape(<q-args>), 1, <bang>0)

"""NOTES
"https://github.com/codegangsta/dotfiles/blob/master/vim/.vimrc#L108
"https://github.com/kien/ctrlp.vim/issues/51
"https://realpython.com/vim-and-python-a-match-made-in-heaven/#lets-make-an-ide
"MANUALLY
"- install coc correctly
"- :CocInstall coc-word
" denite - pip uninstall msgpack-python; pip install -U msgpack


"folding in this file
"" vim:fdm=expr:fdl=0
"" vim:fde=getline(v\:lnum)=~'^""'?'>'.(matchend(getline(v\:lnum),'""*')-2)\:'='
