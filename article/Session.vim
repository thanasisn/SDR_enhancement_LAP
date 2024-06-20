let SessionLoad = 1
if &cp | set nocp | endif
let s:cpo_save=&cpo
set cpo&vim
imap <Nul> <C-Space>
inoremap <expr> <Up> pumvisible() ? "\" : "\<Up>"
inoremap <expr> <Down> pumvisible() ? "\" : "\<Down>"
map! <S-Insert> <MiddleMouse>
inoremap <silent> <Plug>(fzf-maps-i) :call fzf#vim#maps('i', 0)
inoremap <expr> <Plug>(fzf-complete-buffer-line) fzf#vim#complete#buffer_line()
inoremap <expr> <Plug>(fzf-complete-line) fzf#vim#complete#line()
inoremap <expr> <Plug>(fzf-complete-file-ag) fzf#vim#complete#path('ag -l -g ""')
inoremap <expr> <Plug>(fzf-complete-file) fzf#vim#complete#path("find . -path '*/\.*' -prune -o -type f -print -o -type l -print | sed 's:^..::'")
inoremap <expr> <Plug>(fzf-complete-path) fzf#vim#complete#path("find . -path '*/\.*' -prune -o -print | sed '1d;s:^..::'")
inoremap <expr> <Plug>(fzf-complete-word) fzf#vim#complete#word()
inoremap <silent> <Plug>(vimcomplete-skip) =vimcomplete#completor#SkipCompleteSet()
inoremap <silent> <Plug>(vimcomplete-do-complete) =vimcomplete#completor#DoComplete()
inoremap <expr> <S-Tab> pumvisible() ? "\" : "\<S-Tab>"
inoremap <C-Tab> 	
inoremap <silent> <Plug>(ale_complete) :ALEComplete
inoremap <silent> <Plug>(peekaboo) :call peekaboo#aboo()
inoremap <C-F> 
nmap v <Plug>SlimeConfig
nmap  <Plug>SlimeParagraphSend
xmap  <Plug>SlimeRegionSend
nnoremap  h
nnoremap <NL> j
nnoremap  k
nnoremap  l " terminal redraw conflict
xmap <nowait>  <Plug>(VM-Find-Subword-Under)
nmap <nowait>  <Plug>(VM-Find-Under)
nnoremap <silent>   :WhichKey '  '
vmap   <Plug>RDSendSelection
nnoremap <silent> ' :WhichKey ''
nnoremap <silent> [B :bfirst
nnoremap <silent> [b :bprevious
nmap \w\m <Plug>VimwikiMakeTomorrowDiaryNote
nmap \w\y <Plug>VimwikiMakeYesterdayDiaryNote
nmap \w\t <Plug>VimwikiTabMakeDiaryNote
nmap \w\w <Plug>VimwikiMakeDiaryNote
nmap \w\i <Plug>VimwikiDiaryGenerateLinks
nmap \wi <Plug>VimwikiDiaryIndex
nmap \ws <Plug>VimwikiUISelect
nmap \wt <Plug>VimwikiTabIndex
nmap \ww <Plug>VimwikiIndex
xmap <nowait> \\c <Plug>(VM-Visual-Cursors)
nmap <nowait> \\gS <Plug>(VM-Reselect-Last)
nmap <nowait> \\/ <Plug>(VM-Start-Regex-Search)
nmap <nowait> \\\ <Plug>(VM-Add-Cursor-At-Pos)
xmap <nowait> \\a <Plug>(VM-Visual-Add)
xmap <nowait> \\f <Plug>(VM-Visual-Find)
xmap <nowait> \\/ <Plug>(VM-Visual-Regex)
xmap <nowait> \\A <Plug>(VM-Visual-All)
nmap <nowait> \\A <Plug>(VM-Select-All)
vnoremap <silent> \| :call RightPadSelection(+3)
nnoremap <silent> \ :WhichKey '\'
nnoremap <silent> \d :diffupdate
nmap \wx :call VimwikiFindIncompleteTasks()
nmap \wa :call VimwikiFindAllIncompleteTasks()
nnoremap <silent> ]B :blast
nnoremap <silent> ]b :bnext
xmap gx <Plug>NetrwBrowseXVis
nmap gx <Plug>NetrwBrowseX
nmap gcu <Plug>Commentary<Plug>Commentary
nmap gcc <Plug>CommentaryLine
omap gc <Plug>Commentary
nmap gc <Plug>Commentary
xmap gc <Plug>Commentary
nnoremap <silent> <Plug>(YCMFindSymbolInDocument) :call youcompleteme#finder#FindSymbol( 'document' )
nnoremap <silent> <Plug>(YCMFindSymbolInWorkspace) :call youcompleteme#finder#FindSymbol( 'workspace' )
map <S-Insert> <MiddleMouse>
xnoremap <silent> <Plug>NetrwBrowseXVis :call netrw#BrowseXVis()
nnoremap <silent> <Plug>NetrwBrowseX :call netrw#BrowseX(netrw#GX(),netrw#CheckIfRemote(netrw#GX()))
tnoremap <silent> <Plug>(fzf-normal) 
tnoremap <silent> <Plug>(fzf-insert) i
nnoremap <silent> <Plug>(fzf-normal) <Nop>
nnoremap <silent> <Plug>(fzf-insert) i
onoremap <silent> <Plug>(fzf-maps-o) :call fzf#vim#maps('o', 0)
xnoremap <silent> <Plug>(fzf-maps-x) :call fzf#vim#maps('x', 0)
nnoremap <silent> <Plug>(fzf-maps-n) :call fzf#vim#maps('n', 0)
nnoremap <silent> <Plug>(ale_info_preview) :ALEInfo -preview
nnoremap <silent> <Plug>(ale_info_clipboard) :ALEInfo -clipboard
nnoremap <silent> <Plug>(ale_info_echo) :ALEInfo -echo
nnoremap <silent> <Plug>(ale_info) :ALEInfo
nnoremap <silent> <Plug>(ale_repeat_selection) :ALERepeatSelection
nnoremap <silent> <Plug>(ale_code_action) :ALECodeAction
nnoremap <silent> <Plug>(ale_filerename) :ALEFileRename
nnoremap <silent> <Plug>(ale_rename) :ALERename
nnoremap <silent> <Plug>(ale_import) :ALEImport
nnoremap <silent> <Plug>(ale_documentation) :ALEDocumentation
nnoremap <silent> <Plug>(ale_hover) :ALEHover
nnoremap <silent> <Plug>(ale_find_references) :ALEFindReferences
nnoremap <silent> <Plug>(ale_go_to_implementation_in_vsplit) :ALEGoToImplementation -vsplit
nnoremap <silent> <Plug>(ale_go_to_implementation_in_split) :ALEGoToImplementation -split
nnoremap <silent> <Plug>(ale_go_to_implementation_in_tab) :ALEGoToImplementation -tab
nnoremap <silent> <Plug>(ale_go_to_implementation) :ALEGoToImplementation
nnoremap <silent> <Plug>(ale_go_to_type_definition_in_vsplit) :ALEGoToTypeDefinition -vsplit
nnoremap <silent> <Plug>(ale_go_to_type_definition_in_split) :ALEGoToTypeDefinition -split
nnoremap <silent> <Plug>(ale_go_to_type_definition_in_tab) :ALEGoToTypeDefinition -tab
nnoremap <silent> <Plug>(ale_go_to_type_definition) :ALEGoToTypeDefinition
nnoremap <silent> <Plug>(ale_go_to_definition_in_vsplit) :ALEGoToDefinition -vsplit
nnoremap <silent> <Plug>(ale_go_to_definition_in_split) :ALEGoToDefinition -split
nnoremap <silent> <Plug>(ale_go_to_definition_in_tab) :ALEGoToDefinition -tab
nnoremap <silent> <Plug>(ale_go_to_definition) :ALEGoToDefinition
nnoremap <silent> <Plug>(ale_fix) :ALEFix
nnoremap <silent> <Plug>(ale_detail) :ALEDetail
nnoremap <silent> <Plug>(ale_lint) :ALELint
nnoremap <silent> <Plug>(ale_reset_buffer) :ALEResetBuffer
nnoremap <silent> <Plug>(ale_disable_buffer) :ALEDisableBuffer
nnoremap <silent> <Plug>(ale_enable_buffer) :ALEEnableBuffer
nnoremap <silent> <Plug>(ale_toggle_buffer) :ALEToggleBuffer
nnoremap <silent> <Plug>(ale_reset) :ALEReset
nnoremap <silent> <Plug>(ale_disable) :ALEDisable
nnoremap <silent> <Plug>(ale_enable) :ALEEnable
nnoremap <silent> <Plug>(ale_toggle) :ALEToggle
nnoremap <silent> <Plug>(ale_last) :ALELast
nnoremap <silent> <Plug>(ale_first) :ALEFirst
nnoremap <silent> <Plug>(ale_next_wrap_warning) :ALENext -wrap -warning
nnoremap <silent> <Plug>(ale_next_warning) :ALENext -warning
nnoremap <silent> <Plug>(ale_next_wrap_error) :ALENext -wrap -error
nnoremap <silent> <Plug>(ale_next_error) :ALENext -error
nnoremap <silent> <Plug>(ale_next_wrap) :ALENextWrap
nnoremap <silent> <Plug>(ale_next) :ALENext
nnoremap <silent> <Plug>(ale_previous_wrap_warning) :ALEPrevious -wrap -warning
nnoremap <silent> <Plug>(ale_previous_warning) :ALEPrevious -warning
nnoremap <silent> <Plug>(ale_previous_wrap_error) :ALEPrevious -wrap -error
nnoremap <silent> <Plug>(ale_previous_error) :ALEPrevious -error
nnoremap <silent> <Plug>(ale_previous_wrap) :ALEPreviousWrap
nnoremap <silent> <Plug>(ale_previous) :ALEPrevious
nmap <C-C>v <Plug>SlimeConfig
nmap <C-C><C-C> <Plug>SlimeParagraphSend
xmap <C-C><C-C> <Plug>SlimeRegionSend
noremap <SNR>105_Operator :call slime#store_curpos():set opfunc=slime#send_opg@
xnoremap <Plug>ColorFgBg :ColorSwapFgBg
nnoremap <Plug>ColorFgBg :ColorSwapFgBg
xnoremap <Plug>ColorContrast :ColorContrast
nnoremap <Plug>ColorContrast :ColorContrast
xnoremap <Plug>Colorizer :ColorHighlight
nnoremap <Plug>Colorizer :ColorToggle
nmap <silent> <Plug>CommentaryUndo :echoerr "Change your <Plug>CommentaryUndo map to <Plug>Commentary<Plug>Commentary"
nmap <nowait> <C-Down> <Plug>(VM-Add-Cursor-Down)
xmap <nowait> <C-N> <Plug>(VM-Find-Subword-Under)
nmap <nowait> <C-Up> <Plug>(VM-Add-Cursor-Up)
nmap <nowait> <S-Right> <Plug>(VM-Select-l)
nmap <nowait> <S-Left> <Plug>(VM-Select-h)
nmap <nowait> <C-N> <Plug>(VM-Find-Under)
nnoremap <silent> <Plug>(VM-Select-BBW) :call vm#commands#motion('BBW', v:count1, 1, 0)
nnoremap <silent> <Plug>(VM-Select-gE) :call vm#commands#motion('gE', v:count1, 1, 0)
nnoremap <silent> <Plug>(VM-Select-ge) :call vm#commands#motion('ge', v:count1, 1, 0)
nnoremap <silent> <Plug>(VM-Select-E) :call vm#commands#motion('E', v:count1, 1, 0)
nnoremap <silent> <Plug>(VM-Select-e) :call vm#commands#motion('e', v:count1, 1, 0)
nnoremap <silent> <Plug>(VM-Select-B) :call vm#commands#motion('B', v:count1, 1, 0)
nnoremap <silent> <Plug>(VM-Select-b) :call vm#commands#motion('b', v:count1, 1, 0)
nnoremap <silent> <Plug>(VM-Select-W) :call vm#commands#motion('W', v:count1, 1, 0)
nnoremap <silent> <Plug>(VM-Select-w) :call vm#commands#motion('w', v:count1, 1, 0)
nnoremap <silent> <Plug>(VM-Select-l) :call vm#commands#motion('l', v:count1, 1, 0)
nnoremap <silent> <Plug>(VM-Select-k) :call vm#commands#motion('k', v:count1, 1, 0)
nnoremap <silent> <Plug>(VM-Select-j) :call vm#commands#motion('j', v:count1, 1, 0)
nnoremap <silent> <Plug>(VM-Select-h) :call vm#commands#motion('h', v:count1, 1, 0)
nnoremap <silent> <Plug>(VM-Mouse-Column) :call vm#commands#mouse_column()
nmap <silent> <Plug>(VM-Mouse-Word) <Plug>(VM-Left-Mouse)<Plug>(VM-Find-Under)
nmap <silent> <Plug>(VM-Mouse-Cursor) <Plug>(VM-Left-Mouse)<Plug>(VM-Add-Cursor-At-Pos)
nnoremap <silent> <Plug>(VM-Left-Mouse) <LeftMouse>
xnoremap <silent> <Plug>(VM-Visual-Regex) :call vm#commands#find_by_regex(2):call feedkeys('/', 'n')
nnoremap <silent> <Plug>(VM-Slash-Search) @=vm#commands#find_by_regex(3)
nnoremap <silent> <Plug>(VM-Start-Regex-Search) @=vm#commands#find_by_regex(1)
nnoremap <silent> <Plug>(VM-Find-Under) :call vm#commands#ctrln(v:count1)
xnoremap <silent> <Plug>(VM-Visual-Reduce) :call vm#visual#reduce()
xnoremap <silent> <Plug>(VM-Visual-Add) :call vm#commands#visual_add()
xnoremap <silent> <Plug>(VM-Visual-Cursors) :call vm#commands#visual_cursors()
nnoremap <silent> <Plug>(VM-Select-All) :call vm#commands#find_all(0, 1)
nnoremap <silent> <Plug>(VM-Reselect-Last) :call vm#commands#reselect_last()
nnoremap <silent> <Plug>(VM-Select-Cursor-Up) :call vm#commands#add_cursor_up(1, v:count1)
nnoremap <silent> <Plug>(VM-Select-Cursor-Down) :call vm#commands#add_cursor_down(1, v:count1)
nnoremap <silent> <Plug>(VM-Add-Cursor-Up) :call vm#commands#add_cursor_up(0, v:count1)
nnoremap <silent> <Plug>(VM-Add-Cursor-Down) :call vm#commands#add_cursor_down(0, v:count1)
nnoremap <silent> <Plug>(VM-Add-Cursor-At-Word) :call vm#commands#add_cursor_at_word(1, 1)
nnoremap <silent> <Plug>(VM-Add-Cursor-At-Pos) :call vm#commands#add_cursor_at_pos(0)
xmap <silent> <expr> <Plug>(VM-Visual-Find) vm#operators#find(1, 1)
nnoremap <silent> <Plug>(grammarous-move-to-previous-error) :call grammarous#move_to_previous_error(getpos('.')[1 : 2], b:grammarous_result)
nnoremap <silent> <Plug>(grammarous-move-to-next-error) :call grammarous#move_to_next_error(getpos('.')[1 : 2], b:grammarous_result)
nnoremap <silent> <Plug>(grammarous-disable-category) :call grammarous#disable_category_at(getpos('.')[1 : 2], b:grammarous_result)
nnoremap <silent> <Plug>(grammarous-disable-rule) :call grammarous#disable_rule_at(getpos('.')[1 : 2], b:grammarous_result)
nnoremap <silent> <Plug>(grammarous-remove-error) :call grammarous#remove_error_at(getpos('.')[1 : 2], b:grammarous_result)
nnoremap <silent> <Plug>(grammarous-close-info-window) :call grammarous#info_win#close()
nnoremap <silent> <Plug>(grammarous-fixall) :call grammarous#fixall(b:grammarous_result)
nnoremap <silent> <Plug>(grammarous-fixit) :call grammarous#fixit(grammarous#get_error_at(getpos('.')[1 : 2], b:grammarous_result))
nnoremap <silent> <Plug>(grammarous-reset) :call grammarous#reset()
nnoremap <silent> <Plug>(grammarous-open-info-window) :call grammarous#create_update_info_window_of(b:grammarous_result)
nnoremap <silent> <Plug>(grammarous-move-to-info-window) :call grammarous#create_and_jump_to_info_window_of(b:grammarous_result)
xnoremap <silent> <Plug>(peekaboo) :call peekaboo#aboo()
nnoremap <silent> <Plug>(peekaboo) :call peekaboo#aboo()
vnoremap <silent> <C-Down> :resize -1
onoremap <silent> <C-Down> :resize -1
vnoremap <silent> <C-Up> :resize +1
onoremap <silent> <C-Up> :resize +1
noremap <silent> <C-Right> :vertical resize -1
noremap <silent> <C-Left> :vertical resize +1
nnoremap <C-L> l " terminal redraw conflict
nnoremap <C-K> k
nnoremap <C-J> j
nnoremap <C-H> h
nnoremap <F6> :UndotreeToggle
map <F4> :Voom
nmap <F3> :TagbarToggle
map <F2> :NERDTreeToggle
noremap <Right> <Nop>
noremap <Left> <Nop>
noremap <Down> <Nop>
noremap <Up> <Nop>
inoremap  
inoremap <expr> 	 pumvisible() ? "\" : "\	"
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'
let &cpo=s:cpo_save
unlet s:cpo_save
set keymap=greek_utf-8
set autoindent
set autoread
set background=dark
set backspace=indent,eol,start
set balloonexpr=ale#balloon#Expr()
set clipboard=unnamedplus
set commentstring=<!--\ %s\ -->
set completeopt=noinsert,menuone,noselect
set cpoptions=aAceFsB
set expandtab
set fileencodings=ucs-bom,utf-8,default,latin1
set guifont=Fira\ Code\ 9
set guioptions=aegimLt
set history=1000
set hlsearch
set ignorecase
set incsearch
set iskeyword=@,48-57,192-255
set laststatus=2
set mouse=nicr
set printoptions=paper:a4
set ruler
set runtimepath=~/.vim,~/.vim/bundle/Vundle.vim,~/.vim/bundle/gruvbox,~/.vim/bundle/vim-peekaboo,~/.vim/bundle/vim-which-key,~/.vim/bundle/vim-grammarous,~/.vim/bundle/vim-LanguageTool,~/.vim/bundle/vim-autoread,~/.vim/bundle/linediff.vim,~/.vim/bundle/csv.vim,~/.vim/bundle/nerdtree,~/.vim/bundle/tagbar,~/.vim/bundle/vim-visual-multi,~/.vim/bundle/ZFVimDirDiff,~/.vim/bundle/ZFVimJob,~/.vim/bundle/ZFVimIgnore,~/.vim/bundle/vim-airline,~/.vim/bundle/vim-commentary,~/.vim/bundle/Colorizer,~/.vim/bundle/vim-slime,~/.vim/bundle/julia-vim,~/.vim/bundle/Nvim-R,~/.vim/bundle/otter.nvim,~/.vim/bundle/vim-rmarkdown,~/.vim/bundle/vim-pandoc-syntax,~/.vim/bundle/quarto-nvim,~/.vim/bundle/ale,~/.vim/bundle/nvim-lspconfig,~/.vim/bundle/ncm2,~/.vim/bundle/nvim-yarp,~/.vim/bundle/ncm-R,~/.vim/bundle/supertab,~/.vim/bundle/vimcomplete,~/.vim/bundle/conoline.vim,~/.vim/bundle/Apprentice,~/.vim/bundle/vimwiki,~/.vim/bundle/undotree,~/.vim/bundle/VOoM,~/.vim/bundle/fzf.vim,~/.vim/bundle/fzf,~/.vim/bundle/vim-hug-neovim-rpc,/var/lib/vim/addons,/etc/vim,/usr/share/vim/vimfiles,/usr/share/vim/vimfiles/pack/dist-bundle/start/syntastic,/usr/share/vim/vim90,/usr/share/vim/vimfiles/after,/etc/vim/after,/var/lib/vim/addons/after,~/.vim/after,~/.vim/bundle/Vundle.vim,~/.vim/bundle/Vundle.vim/after,~/.vim/bundle/gruvbox/after,~/.vim/bundle/vim-peekaboo/after,~/.vim/bundle/vim-which-key/after,~/.vim/bundle/vim-grammarous/after,~/.vim/bundle/vim-LanguageTool/after,~/.vim/bundle/vim-autoread/after,~/.vim/bundle/linediff.vim/after,~/.vim/bundle/csv.vim/after,~/.vim/bundle/nerdtree/after,~/.vim/bundle/tagbar/after,~/.vim/bundle/vim-visual-multi/after,~/.vim/bundle/ZFVimDirDiff/after,~/.vim/bundle/ZFVimJob/after,~/.vim/bundle/ZFVimIgnore/after,~/.vim/bundle/vim-airline/after,~/.vim/bundle/vim-commentary/after,~/.vim/bundle/Colorizer/after,~/.vim/bundle/vim-slime/after,~/.vim/bundle/julia-vim/after,~/.vim/bundle/Nvim-R/after,~/.vim/bundle/otter.nvim/after,~/.vim/bundle/vim-rmarkdown/after,~/.vim/bundle/vim-pandoc-syntax/after,~/.vim/bundle/quarto-nvim/after,~/.vim/bundle/ale/after,~/.vim/bundle/nvim-lspconfig/after,~/.vim/bundle/ncm2/after,~/.vim/bundle/nvim-yarp/after,~/.vim/bundle/ncm-R/after,~/.vim/bundle/supertab/after,~/.vim/bundle/vimcomplete/after,~/.vim/bundle/conoline.vim/after,~/.vim/bundle/Apprentice/after,~/.vim/bundle/vimwiki/after,~/.vim/bundle/undotree/after,~/.vim/bundle/VOoM/after,~/.vim/bundle/fzf.vim/after,~/.vim/bundle/fzf/after,~/.vim/bundle/vim-hug-neovim-rpc/after
set scrolloff=10
set shiftwidth=2
set shortmess=filnxtToOSc
set showmatch
set smarttab
set softtabstop=2
set spelllang=en_us,el
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc
set tabstop=2
set termencoding=utf-8
set timeoutlen=700
set undodir=~/.vim/undo_dir
set undofile
set wildignore=.Rproj.user,.Ruserdata,.project,*.depend*,.Rhistory,*.pyc,*.dex,*.meta,*.swp,*.user,local.properties,.vim_tags,*.iml,tags,.RData,.DS_Store,.Rproj.user,*/.Rproj.user/*,.Ruserdata,*/.Ruserdata/*,.settings,*/.settings/*,_tmp,*/_tmp/*,.RData,*/.RData/*,.cache,*/.cache/*,build-*,*/build-*/*,bin-*,*/bin-*/*,_cache,*/_cache/*,Pods,*/Pods/*,.wing,*/.wing/*,.vs,*/.vs/*,_build,*/_build/*,.hg,*/.hg/*,.release,*/.release/*,.Rhistory,*/.Rhistory/*,.gradle,*/.gradle/*,.git,*/.git/*,node_modules,*/node_modules/*,.cxx,*/.cxx/*,.svn,*/.svn/*,.idea,*/.idea/*,.externalNativeBuild,*/.externalNativeBuild/*,.vscode,*/.vscode/*,_release,*/_release/*,__pycache__,*/__pycache__/*,vendor,*/vendor/*,.tmp,*/.tmp/*,.build,*/.build/*,_repo,*/_repo/*
set window=42
let s:so_save = &g:so | let s:siso_save = &g:siso | setg so=0 siso=0 | setl so=-1 siso=-1
let v:this_session=expand("<sfile>:p")
silent only
silent tabonly
cd ~/MANUSCRIPTS/02_enhancement/article
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
if &shortmess =~ 'A'
  set shortmess=aoOA
else
  set shortmess=aoO
endif
badd +0 article.Rmd
argglobal
%argdel
$argadd article.Rmd
edit article.Rmd
argglobal
let s:cpo_save=&cpo
set cpo&vim
imap <buffer> <expr> <C-R> peekaboo#peek(1, "\",  0)
xmap <buffer> <expr> " peekaboo#peek(v:count1, '"',  1)
nmap <buffer> <expr> " peekaboo#peek(v:count1, '"',  0)
lnoremap <buffer> � ”
lnoremap <buffer> � “
nmap <buffer> <expr> @ peekaboo#peek(v:count1, '@', 0)
xnoremap <buffer> <silent> [# :keeppatterns '<,'>g/^#/keeppatterns s/^#//
nnoremap <buffer> <silent> [# :keeppatterns s/^#//e
noremap <buffer> <silent> \gN :call b:PreviousRChunk()
noremap <buffer> <silent> \gn :call b:NextRChunk()
noremap <buffer> <silent> \ca :call b:SendChunkToR("echo", "down")
noremap <buffer> <silent> \cd :call b:SendChunkToR("silent", "down")
noremap <buffer> <silent> \ce :call b:SendChunkToR("echo", "stay")
noremap <buffer> <silent> \cc :call b:SendChunkToR("silent", "stay")
vnoremap <buffer> <silent> \kn :call RKnit()
nnoremap <buffer> <silent> \kn :call RKnit()
onoremap <buffer> <silent> \kn :call RKnit()
vnoremap <buffer> <silent> \rd :call RSetWD()
nnoremap <buffer> <silent> \rd :call RSetWD()
onoremap <buffer> <silent> \rd :call RSetWD()
vnoremap <buffer> <silent> \ko :call RMakeRmd("odt_document")
nnoremap <buffer> <silent> \ko :call RMakeRmd("odt_document")
onoremap <buffer> <silent> \ko :call RMakeRmd("odt_document")
vnoremap <buffer> <silent> \kh :call RMakeRmd("html_document")
nnoremap <buffer> <silent> \kh :call RMakeRmd("html_document")
onoremap <buffer> <silent> \kh :call RMakeRmd("html_document")
vnoremap <buffer> <silent> \kw :call RMakeRmd("word_document")
nnoremap <buffer> <silent> \kw :call RMakeRmd("word_document")
onoremap <buffer> <silent> \kw :call RMakeRmd("word_document")
vnoremap <buffer> <silent> \kl :call RMakeRmd("beamer_presentation")
nnoremap <buffer> <silent> \kl :call RMakeRmd("beamer_presentation")
onoremap <buffer> <silent> \kl :call RMakeRmd("beamer_presentation")
vnoremap <buffer> <silent> \kp :call RMakeRmd("pdf_document")
nnoremap <buffer> <silent> \kp :call RMakeRmd("pdf_document")
onoremap <buffer> <silent> \kp :call RMakeRmd("pdf_document")
vnoremap <buffer> <silent> \ka :call RMakeRmd("all")
nnoremap <buffer> <silent> \ka :call RMakeRmd("all")
onoremap <buffer> <silent> \ka :call RMakeRmd("all")
vnoremap <buffer> <silent> \kr :call RMakeRmd("default")
nnoremap <buffer> <silent> \kr :call RMakeRmd("default")
onoremap <buffer> <silent> \kr :call RMakeRmd("default")
vnoremap <buffer> <silent> \r- :call RBrOpenCloseLs("C")
nnoremap <buffer> <silent> \r- :call RBrOpenCloseLs("C")
onoremap <buffer> <silent> \r- :call RBrOpenCloseLs("C")
vnoremap <buffer> <silent> \r= :call RBrOpenCloseLs("O")
nnoremap <buffer> <silent> \r= :call RBrOpenCloseLs("O")
onoremap <buffer> <silent> \r= :call RBrOpenCloseLs("O")
vnoremap <buffer> <silent> \ro :call RObjBrowser()
nnoremap <buffer> <silent> \ro :call RObjBrowser()
onoremap <buffer> <silent> \ro :call RObjBrowser()
vnoremap <buffer> <silent> \rb :call RAction("plotsumm", "v")
vnoremap <buffer> <silent> \rg :call RAction("plot", "v")
vnoremap <buffer> <silent> \rs :call RAction("summary", "v")
nnoremap <buffer> <silent> \rb :call RAction("plotsumm")
onoremap <buffer> <silent> \rb :call RAction("plotsumm")
nnoremap <buffer> <silent> \rg :call RAction("plot")
onoremap <buffer> <silent> \rg :call RAction("plot")
nnoremap <buffer> <silent> \rs :call RAction("summary")
onoremap <buffer> <silent> \rs :call RAction("summary")
vnoremap <buffer> <silent> \rh :call RAction("help")
nnoremap <buffer> <silent> \rh :call RAction("help")
onoremap <buffer> <silent> \rh :call RAction("help")
vnoremap <buffer> <silent> \re :call RAction("example")
nnoremap <buffer> <silent> \re :call RAction("example")
onoremap <buffer> <silent> \re :call RAction("example")
vnoremap <buffer> <silent> \ra :call RAction("args")
nnoremap <buffer> <silent> \ra :call RAction("args")
onoremap <buffer> <silent> \ra :call RAction("args")
vnoremap <buffer> <silent> \td :call RAction("dputtab", "v")
vnoremap <buffer> <silent> \vh :call RAction("viewobj", "v", ", howto='above 7split', nrows=6")
vnoremap <buffer> <silent> \vv :call RAction("viewobj", "v", ", howto='vsplit'")
vnoremap <buffer> <silent> \vs :call RAction("viewobj", "v", ", howto='split'")
vnoremap <buffer> <silent> \rv :call RAction("viewobj", "v")
vnoremap <buffer> <silent> \rt :call RAction("str", "v")
vnoremap <buffer> <silent> \rn :call RAction("nvim.names", "v")
vnoremap <buffer> <silent> \rp :call RAction("print", "v")
nnoremap <buffer> <silent> \td :call RAction("dputtab")
onoremap <buffer> <silent> \td :call RAction("dputtab")
nnoremap <buffer> <silent> \vh :call RAction("viewobj", ", howto='above 7split', nrows=6")
onoremap <buffer> <silent> \vh :call RAction("viewobj", ", howto='above 7split', nrows=6")
nnoremap <buffer> <silent> \vv :call RAction("viewobj", ", howto='vsplit'")
onoremap <buffer> <silent> \vv :call RAction("viewobj", ", howto='vsplit'")
nnoremap <buffer> <silent> \vs :call RAction("viewobj", ", howto='split'")
onoremap <buffer> <silent> \vs :call RAction("viewobj", ", howto='split'")
nnoremap <buffer> <silent> \rv :call RAction("viewobj")
onoremap <buffer> <silent> \rv :call RAction("viewobj")
nnoremap <buffer> <silent> \rt :call RAction("str")
onoremap <buffer> <silent> \rt :call RAction("str")
nnoremap <buffer> <silent> \rn :call RAction("nvim.names")
onoremap <buffer> <silent> \rn :call RAction("nvim.names")
nnoremap <buffer> <silent> \rp :call RAction("print")
onoremap <buffer> <silent> \rp :call RAction("print")
vnoremap <buffer> <silent> \rm :call RClearAll()
nnoremap <buffer> <silent> \rm :call RClearAll()
onoremap <buffer> <silent> \rm :call RClearAll()
vnoremap <buffer> <silent> \rr :call RClearConsole()
nnoremap <buffer> <silent> \rr :call RClearConsole()
onoremap <buffer> <silent> \rr :call RClearConsole()
vnoremap <buffer> <silent> \rl :call g:SendCmdToR("ls()")
nnoremap <buffer> <silent> \rl :call g:SendCmdToR("ls()")
onoremap <buffer> <silent> \rl :call g:SendCmdToR("ls()")
noremap <buffer> <silent> \ud :call RAction("undebug")
noremap <buffer> <silent> \bg :call RAction("debug")
noremap <buffer> <silent> \r<Right> :call RSendPartOfLine("right", 0)
noremap <buffer> <silent> \r<Left> :call RSendPartOfLine("left", 0)
noremap <buffer> <silent> \m :set opfunc=SendMotionToRg@
vnoremap <buffer> <silent> \o :call RWarningMsg("This command does not work over a selection of lines.")
nnoremap <buffer> <silent> \o :call SendLineToRAndInsertOutput()0
onoremap <buffer> <silent> \o :call SendLineToRAndInsertOutput()0
noremap <buffer> <silent> \d :call SendLineToR("down")0
noremap <buffer> <silent> \l :call SendLineToR("stay")
noremap <buffer> <silent> \ch :call SendFHChunkToR()
noremap <buffer> <silent> \pa :call SendParagraphToR("echo", "down")
noremap <buffer> <silent> \pd :call SendParagraphToR("silent", "down")
noremap <buffer> <silent> \pe :call SendParagraphToR("echo", "stay")
noremap <buffer> <silent> \pp :call SendParagraphToR("silent", "stay")
vnoremap <buffer> <silent> \so :call SendSelectionToR("echo", "stay", "NewtabInsert")
vnoremap <buffer> <silent> \sa :call SendSelectionToR("echo", "down")
vnoremap <buffer> <silent> \se :call SendSelectionToR("echo", "stay")
vnoremap <buffer> <silent> \ss :call SendSelectionToR("silent", "stay")
nnoremap <buffer> <silent> \sa :call SendSelectionToR("echo", "down", "normal")
onoremap <buffer> <silent> \sa :call SendSelectionToR("echo", "down", "normal")
noremap <buffer> <silent> \sd :call SendSelectionToR("silent", "down", "normal")
nnoremap <buffer> <silent> \se :call SendSelectionToR("echo", "stay", "normal")
onoremap <buffer> <silent> \se :call SendSelectionToR("echo", "stay", "normal")
nnoremap <buffer> <silent> \ss :call SendSelectionToR("silent", "stay", "normal")
onoremap <buffer> <silent> \ss :call SendSelectionToR("silent", "stay", "normal")
vnoremap <buffer> <silent> \fa :call SendFunctionToR("echo", "down")
nnoremap <buffer> <silent> \fa :call SendFunctionToR("echo", "down")
onoremap <buffer> <silent> \fa :call SendFunctionToR("echo", "down")
vnoremap <buffer> <silent> \fd :call SendFunctionToR("silent", "down")
nnoremap <buffer> <silent> \fd :call SendFunctionToR("silent", "down")
onoremap <buffer> <silent> \fd :call SendFunctionToR("silent", "down")
vnoremap <buffer> <silent> \fe :call SendFunctionToR("echo", "stay")
nnoremap <buffer> <silent> \fe :call SendFunctionToR("echo", "stay")
onoremap <buffer> <silent> \fe :call SendFunctionToR("echo", "stay")
vnoremap <buffer> <silent> \ff :call SendFunctionToR("silent", "stay")
nnoremap <buffer> <silent> \ff :call SendFunctionToR("silent", "stay")
onoremap <buffer> <silent> \ff :call SendFunctionToR("silent", "stay")
noremap <buffer> <silent> \ba :call SendMBlockToR("echo", "down")
noremap <buffer> <silent> \bd :call SendMBlockToR("silent", "down")
noremap <buffer> <silent> \be :call SendMBlockToR("echo", "stay")
noremap <buffer> <silent> \bb :call SendMBlockToR("silent", "stay")
vnoremap <buffer> <silent> \rw :call RQuit('save')
nnoremap <buffer> <silent> \rw :call RQuit('save')
onoremap <buffer> <silent> \rw :call RQuit('save')
vnoremap <buffer> <silent> \rq :call RQuit('nosave')
nnoremap <buffer> <silent> \rq :call RQuit('nosave')
onoremap <buffer> <silent> \rq :call RQuit('nosave')
vnoremap <buffer> <silent> \rc :call StartR("custom")
nnoremap <buffer> <silent> \rc :call StartR("custom")
onoremap <buffer> <silent> \rc :call StartR("custom")
vnoremap <buffer> <silent> \rf :call StartR("R")
nnoremap <buffer> <silent> \rf :call StartR("R")
onoremap <buffer> <silent> \rf :call StartR("R")
xnoremap <buffer> <silent> ]# :keeppatterns '<,'>g/^#/keeppatterns s/^#/##/
nnoremap <buffer> <silent> ]# :keeppatterns s/^/#/e
noremap <buffer> <silent> <Plug>RPreviousRChunk :call b:PreviousRChunk()
noremap <buffer> <silent> <Plug>RNextRChunk :call b:NextRChunk()
noremap <buffer> <silent> <Plug>REDSendChunk :call b:SendChunkToR("echo", "down")
noremap <buffer> <silent> <Plug>RDSendChunk :call b:SendChunkToR("silent", "down")
noremap <buffer> <silent> <Plug>RESendChunk :call b:SendChunkToR("echo", "stay")
noremap <buffer> <silent> <Plug>RSendChunk :call b:SendChunkToR("silent", "stay")
vnoremap <buffer> <silent> <Plug>RKnit :call RKnit()
nnoremap <buffer> <silent> <Plug>RKnit :call RKnit()
onoremap <buffer> <silent> <Plug>RKnit :call RKnit()
vnoremap <buffer> <silent> <Plug>RSetwd :call RSetWD()
nnoremap <buffer> <silent> <Plug>RSetwd :call RSetWD()
onoremap <buffer> <silent> <Plug>RSetwd :call RSetWD()
vnoremap <buffer> <silent> <Plug>RMakeODT :call RMakeRmd("odt_document")
nnoremap <buffer> <silent> <Plug>RMakeODT :call RMakeRmd("odt_document")
onoremap <buffer> <silent> <Plug>RMakeODT :call RMakeRmd("odt_document")
vnoremap <buffer> <silent> <Plug>RMakeHTML :call RMakeRmd("html_document")
nnoremap <buffer> <silent> <Plug>RMakeHTML :call RMakeRmd("html_document")
onoremap <buffer> <silent> <Plug>RMakeHTML :call RMakeRmd("html_document")
vnoremap <buffer> <silent> <Plug>RMakeWord :call RMakeRmd("word_document")
nnoremap <buffer> <silent> <Plug>RMakeWord :call RMakeRmd("word_document")
onoremap <buffer> <silent> <Plug>RMakeWord :call RMakeRmd("word_document")
vnoremap <buffer> <silent> <Plug>RMakePDFKb :call RMakeRmd("beamer_presentation")
nnoremap <buffer> <silent> <Plug>RMakePDFKb :call RMakeRmd("beamer_presentation")
onoremap <buffer> <silent> <Plug>RMakePDFKb :call RMakeRmd("beamer_presentation")
vnoremap <buffer> <silent> <Plug>RMakePDFK :call RMakeRmd("pdf_document")
nnoremap <buffer> <silent> <Plug>RMakePDFK :call RMakeRmd("pdf_document")
onoremap <buffer> <silent> <Plug>RMakePDFK :call RMakeRmd("pdf_document")
vnoremap <buffer> <silent> <Plug>RMakeAll :call RMakeRmd("all")
nnoremap <buffer> <silent> <Plug>RMakeAll :call RMakeRmd("all")
onoremap <buffer> <silent> <Plug>RMakeAll :call RMakeRmd("all")
vnoremap <buffer> <silent> <Plug>RMakeRmd :call RMakeRmd("default")
nnoremap <buffer> <silent> <Plug>RMakeRmd :call RMakeRmd("default")
onoremap <buffer> <silent> <Plug>RMakeRmd :call RMakeRmd("default")
vnoremap <buffer> <silent> <Plug>RCloseLists :call RBrOpenCloseLs("C")
nnoremap <buffer> <silent> <Plug>RCloseLists :call RBrOpenCloseLs("C")
onoremap <buffer> <silent> <Plug>RCloseLists :call RBrOpenCloseLs("C")
vnoremap <buffer> <silent> <Plug>ROpenLists :call RBrOpenCloseLs("O")
nnoremap <buffer> <silent> <Plug>ROpenLists :call RBrOpenCloseLs("O")
onoremap <buffer> <silent> <Plug>ROpenLists :call RBrOpenCloseLs("O")
vnoremap <buffer> <silent> <Plug>RUpdateObjBrowser :call RObjBrowser()
nnoremap <buffer> <silent> <Plug>RUpdateObjBrowser :call RObjBrowser()
onoremap <buffer> <silent> <Plug>RUpdateObjBrowser :call RObjBrowser()
vnoremap <buffer> <silent> <Plug>RSPlot :call RAction("plotsumm", "v")
vnoremap <buffer> <silent> <Plug>RPlot :call RAction("plot", "v")
vnoremap <buffer> <silent> <Plug>RSummary :call RAction("summary", "v")
nnoremap <buffer> <silent> <Plug>RSPlot :call RAction("plotsumm")
onoremap <buffer> <silent> <Plug>RSPlot :call RAction("plotsumm")
nnoremap <buffer> <silent> <Plug>RPlot :call RAction("plot")
onoremap <buffer> <silent> <Plug>RPlot :call RAction("plot")
nnoremap <buffer> <silent> <Plug>RSummary :call RAction("summary")
onoremap <buffer> <silent> <Plug>RSummary :call RAction("summary")
vnoremap <buffer> <silent> <Plug>RHelp :call RAction("help")
nnoremap <buffer> <silent> <Plug>RHelp :call RAction("help")
onoremap <buffer> <silent> <Plug>RHelp :call RAction("help")
vnoremap <buffer> <silent> <Plug>RShowEx :call RAction("example")
nnoremap <buffer> <silent> <Plug>RShowEx :call RAction("example")
onoremap <buffer> <silent> <Plug>RShowEx :call RAction("example")
vnoremap <buffer> <silent> <Plug>RShowArgs :call RAction("args")
nnoremap <buffer> <silent> <Plug>RShowArgs :call RAction("args")
onoremap <buffer> <silent> <Plug>RShowArgs :call RAction("args")
vnoremap <buffer> <silent> <Plug>RDputObj :call RAction("dputtab", "v")
vnoremap <buffer> <silent> <Plug>RViewDFa :call RAction("viewobj", "v", ", howto='above 7split', nrows=6")
vnoremap <buffer> <silent> <Plug>RViewDFv :call RAction("viewobj", "v", ", howto='vsplit'")
vnoremap <buffer> <silent> <Plug>RViewDFs :call RAction("viewobj", "v", ", howto='split'")
vnoremap <buffer> <silent> <Plug>RViewDF :call RAction("viewobj", "v")
vnoremap <buffer> <silent> <Plug>RObjectStr :call RAction("str", "v")
vnoremap <buffer> <silent> <Plug>RObjectNames :call RAction("nvim.names", "v")
vnoremap <buffer> <silent> <Plug>RObjectPr :call RAction("print", "v")
nnoremap <buffer> <silent> <Plug>RDputObj :call RAction("dputtab")
onoremap <buffer> <silent> <Plug>RDputObj :call RAction("dputtab")
nnoremap <buffer> <silent> <Plug>RViewDFa :call RAction("viewobj", ", howto='above 7split', nrows=6")
onoremap <buffer> <silent> <Plug>RViewDFa :call RAction("viewobj", ", howto='above 7split', nrows=6")
nnoremap <buffer> <silent> <Plug>RViewDFv :call RAction("viewobj", ", howto='vsplit'")
onoremap <buffer> <silent> <Plug>RViewDFv :call RAction("viewobj", ", howto='vsplit'")
nnoremap <buffer> <silent> <Plug>RViewDFs :call RAction("viewobj", ", howto='split'")
onoremap <buffer> <silent> <Plug>RViewDFs :call RAction("viewobj", ", howto='split'")
nnoremap <buffer> <silent> <Plug>RViewDF :call RAction("viewobj")
onoremap <buffer> <silent> <Plug>RViewDF :call RAction("viewobj")
nnoremap <buffer> <silent> <Plug>RObjectStr :call RAction("str")
onoremap <buffer> <silent> <Plug>RObjectStr :call RAction("str")
nnoremap <buffer> <silent> <Plug>RObjectNames :call RAction("nvim.names")
onoremap <buffer> <silent> <Plug>RObjectNames :call RAction("nvim.names")
nnoremap <buffer> <silent> <Plug>RObjectPr :call RAction("print")
onoremap <buffer> <silent> <Plug>RObjectPr :call RAction("print")
vnoremap <buffer> <silent> <Plug>RClearAll :call RClearAll()
nnoremap <buffer> <silent> <Plug>RClearAll :call RClearAll()
onoremap <buffer> <silent> <Plug>RClearAll :call RClearAll()
vnoremap <buffer> <silent> <Plug>RClearConsole :call RClearConsole()
nnoremap <buffer> <silent> <Plug>RClearConsole :call RClearConsole()
onoremap <buffer> <silent> <Plug>RClearConsole :call RClearConsole()
vnoremap <buffer> <silent> <Plug>RListSpace :call g:SendCmdToR("ls()")
nnoremap <buffer> <silent> <Plug>RListSpace :call g:SendCmdToR("ls()")
onoremap <buffer> <silent> <Plug>RListSpace :call g:SendCmdToR("ls()")
noremap <buffer> <silent> <Plug>RUndebug :call RAction("undebug")
noremap <buffer> <silent> <Plug>RDebug :call RAction("debug")
noremap <buffer> <silent> <Plug>RNRightPart :call RSendPartOfLine("right", 0)
noremap <buffer> <silent> <Plug>RNLeftPart :call RSendPartOfLine("left", 0)
noremap <buffer> <silent> <Plug>RSendMotion :set opfunc=SendMotionToRg@
vnoremap <buffer> <silent> <Plug>(RDSendLineAndInsertOutput) :call RWarningMsg("This command does not work over a selection of lines.")
nnoremap <buffer> <silent> <Plug>(RDSendLineAndInsertOutput) :call SendLineToRAndInsertOutput()0
onoremap <buffer> <silent> <Plug>(RDSendLineAndInsertOutput) :call SendLineToRAndInsertOutput()0
noremap <buffer> <silent> <Plug>RDSendLine :call SendLineToR("down")0
noremap <buffer> <silent> <Plug>RSendLine :call SendLineToR("stay")
noremap <buffer> <silent> <Plug>RSendChunkFH :call SendFHChunkToR()
noremap <buffer> <silent> <Plug>REDSendParagraph :call SendParagraphToR("echo", "down")
noremap <buffer> <silent> <Plug>RDSendParagraph :call SendParagraphToR("silent", "down")
noremap <buffer> <silent> <Plug>RESendParagraph :call SendParagraphToR("echo", "stay")
noremap <buffer> <silent> <Plug>RSendParagraph :call SendParagraphToR("silent", "stay")
vnoremap <buffer> <silent> <Plug>RSendSelAndInsertOutput :call SendSelectionToR("echo", "stay", "NewtabInsert")
vnoremap <buffer> <silent> <Plug>REDSendSelection :call SendSelectionToR("echo", "down")
vnoremap <buffer> <silent> <Plug>RDSendSelection :call SendSelectionToR("silent", "down")
vnoremap <buffer> <silent> <Plug>RESendSelection :call SendSelectionToR("echo", "stay")
vnoremap <buffer> <silent> <Plug>RSendSelection :call SendSelectionToR("silent", "stay")
nnoremap <buffer> <silent> <Plug>REDSendSelection :call SendSelectionToR("echo", "down", "normal")
onoremap <buffer> <silent> <Plug>REDSendSelection :call SendSelectionToR("echo", "down", "normal")
nnoremap <buffer> <silent> <Plug>RDSendSelection :call SendSelectionToR("silent", "down", "normal")
onoremap <buffer> <silent> <Plug>RDSendSelection :call SendSelectionToR("silent", "down", "normal")
nnoremap <buffer> <silent> <Plug>RESendSelection :call SendSelectionToR("echo", "stay", "normal")
onoremap <buffer> <silent> <Plug>RESendSelection :call SendSelectionToR("echo", "stay", "normal")
nnoremap <buffer> <silent> <Plug>RSendSelection :call SendSelectionToR("silent", "stay", "normal")
onoremap <buffer> <silent> <Plug>RSendSelection :call SendSelectionToR("silent", "stay", "normal")
vnoremap <buffer> <silent> <Plug>RDSendFunction :call SendFunctionToR("echo", "down")
nnoremap <buffer> <silent> <Plug>RDSendFunction :call SendFunctionToR("echo", "down")
onoremap <buffer> <silent> <Plug>RDSendFunction :call SendFunctionToR("echo", "down")
vnoremap <buffer> <silent> <Plug>RSendFunction :call SendFunctionToR("silent", "stay")
nnoremap <buffer> <silent> <Plug>RSendFunction :call SendFunctionToR("silent", "stay")
onoremap <buffer> <silent> <Plug>RSendFunction :call SendFunctionToR("silent", "stay")
noremap <buffer> <silent> <Plug>REDSendMBlock :call SendMBlockToR("echo", "down")
noremap <buffer> <silent> <Plug>RDSendMBlock :call SendMBlockToR("silent", "down")
noremap <buffer> <silent> <Plug>RESendMBlock :call SendMBlockToR("echo", "stay")
noremap <buffer> <silent> <Plug>RSendMBlock :call SendMBlockToR("silent", "stay")
vnoremap <buffer> <silent> <Plug>RSaveClose :call RQuit('save')
nnoremap <buffer> <silent> <Plug>RSaveClose :call RQuit('save')
onoremap <buffer> <silent> <Plug>RSaveClose :call RQuit('save')
vnoremap <buffer> <silent> <Plug>RClose :call RQuit('nosave')
nnoremap <buffer> <silent> <Plug>RClose :call RQuit('nosave')
onoremap <buffer> <silent> <Plug>RClose :call RQuit('nosave')
vnoremap <buffer> <silent> <Plug>RCustomStart :call StartR("custom")
nnoremap <buffer> <silent> <Plug>RCustomStart :call StartR("custom")
onoremap <buffer> <silent> <Plug>RCustomStart :call StartR("custom")
vnoremap <buffer> <silent> <Plug>RStart :call StartR("R")
nnoremap <buffer> <silent> <Plug>RStart :call StartR("R")
onoremap <buffer> <silent> <Plug>RStart :call StartR("R")
imap <buffer> <expr>  peekaboo#peek(1, "\",  0)
lnoremap <buffer> #q ϙ
lnoremap <buffer> #SP Ϡ
lnoremap <buffer> #Q Ϙ
lnoremap <buffer> #G Ϝ
lnoremap <buffer> #ST Ϛ
lnoremap <buffer> && ϗ
lnoremap <buffer> ' ᾽
lnoremap <buffer> -Y Ῡ
lnoremap <buffer> -I Ῑ
lnoremap <buffer> -A Ᾱ
lnoremap <buffer> -y ῡ
lnoremap <buffer> -i ῑ
lnoremap <buffer> -a ᾱ
lnoremap <buffer> --- —
lnoremap <buffer> -- –
lnoremap <buffer> /v ώ
lnoremap <buffer> /y ύ
lnoremap <buffer> /o ό
lnoremap <buffer> /i ί
lnoremap <buffer> /h ή
lnoremap <buffer> /e έ
lnoremap <buffer> /a ά
lnoremap <buffer> /V Ώ
lnoremap <buffer> /Y Ύ
lnoremap <buffer> /O Ό
lnoremap <buffer> /I Ί
lnoremap <buffer> /H Ή
lnoremap <buffer> /E Έ
lnoremap <buffer> /A Ά
lnoremap <buffer> / ´
lnoremap <buffer> :~y ῧ
lnoremap <buffer> :~i ῗ
lnoremap <buffer> :~ ῁
lnoremap <buffer> :y ϋ
lnoremap <buffer> :i ϊ
lnoremap <buffer> :`y ῢ
lnoremap <buffer> :;y ΰ
lnoremap <buffer> :Y Ϋ
lnoremap <buffer> :I Ϊ
lnoremap <buffer> :`i ῒ
lnoremap <buffer> :;i ΐ
lnoremap <buffer> :` ῭
lnoremap <buffer> :; ΅
lnoremap <buffer> : ¨
lnoremap <buffer> ;U ϴ
lnoremap <buffer> ;r ϱ
lnoremap <buffer> ;k ϰ
lnoremap <buffer> ;p ϖ
lnoremap <buffer> ;f ϕ
lnoremap <buffer> ;u ϑ
lnoremap <buffer> ;b ϐ
lnoremap <buffer> ;## ͵
lnoremap <buffer> ;# ʹ
lnoremap <buffer> ;v| ῴ
lnoremap <buffer> ;h| ῄ
lnoremap <buffer> ;a| ᾴ
lnoremap <buffer> ;v ώ
lnoremap <buffer> ;y ύ
lnoremap <buffer> ;o ό
lnoremap <buffer> ;:y ΰ
lnoremap <buffer> ;i ί
lnoremap <buffer> ;h ή
lnoremap <buffer> ;e έ
lnoremap <buffer> ;a ά
lnoremap <buffer> ;:i ΐ
lnoremap <buffer> ;V Ώ
lnoremap <buffer> ;Y Ύ
lnoremap <buffer> ;O Ό
lnoremap <buffer> ;I Ί
lnoremap <buffer> ;H Ή
lnoremap <buffer> ;E Έ
lnoremap <buffer> ;A Ά
lnoremap <buffer> ;' ῾
lnoremap <buffer> ;: ΅
lnoremap <buffer> ; ΄
lnoremap <buffer> ;. ·
lnoremap <buffer> ;< «
lnoremap <buffer> ;> »
lnoremap <buffer> <R Ῥ
lnoremap <buffer> <r ῥ
lnoremap <buffer> <, ̔
lnoremap <buffer> <~v| ᾧ
lnoremap <buffer> <~h| ᾗ
lnoremap <buffer> <~a| ᾇ
lnoremap <buffer> <~V| ᾯ
lnoremap <buffer> <~H| ᾟ
lnoremap <buffer> <~A| ᾏ
lnoremap <buffer> <;v| ᾥ
lnoremap <buffer> <;h| ᾕ
lnoremap <buffer> <;a| ᾅ
lnoremap <buffer> <;V| ᾭ
lnoremap <buffer> <;H| ᾝ
lnoremap <buffer> <;A| ᾍ
lnoremap <buffer> <`v| ᾣ
lnoremap <buffer> <`h| ᾓ
lnoremap <buffer> <`a| ᾃ
lnoremap <buffer> <`V| ᾫ
lnoremap <buffer> <`H| ᾛ
lnoremap <buffer> <`A| ᾋ
lnoremap <buffer> <v| ᾡ
lnoremap <buffer> <h| ᾑ
lnoremap <buffer> <a| ᾁ
lnoremap <buffer> <V| ᾩ
lnoremap <buffer> <H| ᾙ
lnoremap <buffer> <A| ᾉ
lnoremap <buffer> <~v ὧ
lnoremap <buffer> <~y ὗ
lnoremap <buffer> <~o ὇
lnoremap <buffer> <~i ἷ
lnoremap <buffer> <~h ἧ
lnoremap <buffer> <~e ἗
lnoremap <buffer> <~a ἇ
lnoremap <buffer> <~V Ὧ
lnoremap <buffer> <~Y Ὗ
lnoremap <buffer> <~O ὏
lnoremap <buffer> <~I Ἷ
lnoremap <buffer> <~H Ἧ
lnoremap <buffer> <~E ἟
lnoremap <buffer> <~A Ἇ
lnoremap <buffer> <;v ὥ
lnoremap <buffer> <;y ὕ
lnoremap <buffer> <;o ὅ
lnoremap <buffer> <;i ἵ
lnoremap <buffer> <;h ἥ
lnoremap <buffer> <;e ἕ
lnoremap <buffer> <;a ἅ
lnoremap <buffer> <;V Ὥ
lnoremap <buffer> <;Y Ὕ
lnoremap <buffer> <;O Ὅ
lnoremap <buffer> <;I Ἵ
lnoremap <buffer> <;H Ἥ
lnoremap <buffer> <;E Ἕ
lnoremap <buffer> <;A Ἅ
lnoremap <buffer> <`v ὣ
lnoremap <buffer> <`y ὓ
lnoremap <buffer> <`o ὃ
lnoremap <buffer> <`i ἳ
lnoremap <buffer> <`h ἣ
lnoremap <buffer> <`e ἓ
lnoremap <buffer> <`a ἃ
lnoremap <buffer> <`V Ὣ
lnoremap <buffer> <`Y Ὓ
lnoremap <buffer> <`O Ὃ
lnoremap <buffer> <`I Ἳ
lnoremap <buffer> <`H Ἣ
lnoremap <buffer> <`E Ἓ
lnoremap <buffer> <`A Ἃ
lnoremap <buffer> <v ὡ
lnoremap <buffer> <y ὑ
lnoremap <buffer> <o ὁ
lnoremap <buffer> <i ἱ
lnoremap <buffer> <h ἡ
lnoremap <buffer> <e ἑ
lnoremap <buffer> <a ἁ
lnoremap <buffer> <V Ὡ
lnoremap <buffer> <Y Ὑ
lnoremap <buffer> <O Ὁ
lnoremap <buffer> <I Ἱ
lnoremap <buffer> <H Ἡ
lnoremap <buffer> <E Ἑ
lnoremap <buffer> <A Ἁ
lnoremap <buffer> <~ ῟
lnoremap <buffer> <; ῞
lnoremap <buffer> <` ῝
lnoremap <buffer> << «
lnoremap <buffer> >r ῤ
lnoremap <buffer> >, ̓
lnoremap <buffer> >~v| ᾦ
lnoremap <buffer> >~h| ᾖ
lnoremap <buffer> >~a| ᾆ
lnoremap <buffer> >~V| ᾮ
lnoremap <buffer> >~H| ᾞ
lnoremap <buffer> >~A| ᾎ
lnoremap <buffer> >;v| ᾤ
lnoremap <buffer> >;h| ᾔ
lnoremap <buffer> >;a| ᾄ
lnoremap <buffer> >;V| ᾬ
lnoremap <buffer> >;H| ᾜ
lnoremap <buffer> >;A| ᾌ
lnoremap <buffer> >`v| ᾢ
lnoremap <buffer> >`h| ᾒ
lnoremap <buffer> >`a| ᾂ
lnoremap <buffer> >`V| ᾪ
lnoremap <buffer> >`H| ᾚ
lnoremap <buffer> >`A| ᾊ
lnoremap <buffer> >v| ᾠ
lnoremap <buffer> >h| ᾐ
lnoremap <buffer> >a| ᾀ
lnoremap <buffer> >V| ᾨ
lnoremap <buffer> >H| ᾘ
lnoremap <buffer> >A| ᾈ
lnoremap <buffer> >~v ὦ
lnoremap <buffer> >~y ὖ
lnoremap <buffer> >~o ὆
lnoremap <buffer> >~i ἶ
lnoremap <buffer> >~h ἦ
lnoremap <buffer> >~e ἖
lnoremap <buffer> >~a ἆ
lnoremap <buffer> >~V Ὦ
lnoremap <buffer> >~Y ὞
lnoremap <buffer> >~O ὎
lnoremap <buffer> >~I Ἶ
lnoremap <buffer> >~H Ἦ
lnoremap <buffer> >~E ἞
lnoremap <buffer> >~A Ἆ
lnoremap <buffer> >;v ὤ
lnoremap <buffer> >;y ὔ
lnoremap <buffer> >;o ὄ
lnoremap <buffer> >;i ἴ
lnoremap <buffer> >;h ἤ
lnoremap <buffer> >;e ἔ
lnoremap <buffer> >;a ἄ
lnoremap <buffer> >;V Ὤ
lnoremap <buffer> >;Y ὜
lnoremap <buffer> >;O Ὄ
lnoremap <buffer> >;I Ἴ
lnoremap <buffer> >;H Ἤ
lnoremap <buffer> >;E Ἔ
lnoremap <buffer> >;A Ἄ
lnoremap <buffer> >`v ὢ
lnoremap <buffer> >`y ὒ
lnoremap <buffer> >`o ὂ
lnoremap <buffer> >`i ἲ
lnoremap <buffer> >`h ἢ
lnoremap <buffer> >`e ἒ
lnoremap <buffer> >`a ἂ
lnoremap <buffer> >`V Ὢ
lnoremap <buffer> >`Y ὚
lnoremap <buffer> >`O Ὂ
lnoremap <buffer> >`I Ἲ
lnoremap <buffer> >`H Ἢ
lnoremap <buffer> >`E Ἒ
lnoremap <buffer> >`A Ἂ
lnoremap <buffer> >v ὠ
lnoremap <buffer> >y ὐ
lnoremap <buffer> >o ὀ
lnoremap <buffer> >i ἰ
lnoremap <buffer> >h ἠ
lnoremap <buffer> >e ἐ
lnoremap <buffer> >a ἀ
lnoremap <buffer> >V Ὠ
lnoremap <buffer> >Y ὘
lnoremap <buffer> >O Ὀ
lnoremap <buffer> >I Ἰ
lnoremap <buffer> >H Ἠ
lnoremap <buffer> >E Ἐ
lnoremap <buffer> >A Ἀ
lnoremap <buffer> >~ ῏
lnoremap <buffer> >; ῎
lnoremap <buffer> >` ῍
lnoremap <buffer> >> »
lnoremap <buffer> A| ᾼ
lnoremap <buffer> A Α
lnoremap <buffer> B Β
lnoremap <buffer> C Ψ
lnoremap <buffer> D$ ₯
lnoremap <buffer> D Δ
lnoremap <buffer> E Ε
lnoremap <buffer> E$ €
lnoremap <buffer> F Φ
lnoremap <buffer> G Γ
lnoremap <buffer> H| ῌ
lnoremap <buffer> H Η
lnoremap <buffer> I Ι
lnoremap <buffer> J Ξ
lnoremap <buffer> K Κ
lnoremap <buffer> L Λ
lnoremap <buffer> M Μ
lnoremap <buffer> N Ν
lnoremap <buffer> O Ο
lnoremap <buffer> P Π
lnoremap <buffer> Q :
lnoremap <buffer> R Ρ
lnoremap <buffer> S Σ
lnoremap <buffer> T Τ
lnoremap <buffer> U Θ
lnoremap <buffer> V| ῼ
lnoremap <buffer> V Ω
lnoremap <buffer> W ·
lnoremap <buffer> X Χ
lnoremap <buffer> Y Υ
lnoremap <buffer> Z Ζ
lnoremap <buffer> ^Y Ῠ
lnoremap <buffer> ^I Ῐ
lnoremap <buffer> ^A Ᾰ
lnoremap <buffer> ^y ῠ
lnoremap <buffer> ^i ῐ
lnoremap <buffer> ^a ᾰ
inoremap <buffer> <silent> _ :call ReplaceUnderS()a
inoremap <buffer> <silent> ` :call RWriteRmdChunk()a
lnoremap <buffer> `v| ῲ
lnoremap <buffer> `h| ῂ
lnoremap <buffer> `a| ᾲ
lnoremap <buffer> `v ὼ
lnoremap <buffer> `y ὺ
lnoremap <buffer> `o ὸ
lnoremap <buffer> `i ὶ
lnoremap <buffer> `h ὴ
lnoremap <buffer> `e ὲ
lnoremap <buffer> `a ὰ
lnoremap <buffer> `V Ὼ
lnoremap <buffer> `Y Ὺ
lnoremap <buffer> `O Ὸ
lnoremap <buffer> `I Ὶ
lnoremap <buffer> `H Ὴ
lnoremap <buffer> `E Ὲ
lnoremap <buffer> `A Ὰ
lnoremap <buffer> `:y ῢ
lnoremap <buffer> `:i ῒ
lnoremap <buffer> ` `
lnoremap <buffer> `: ῭
lnoremap <buffer> a| ᾳ
lnoremap <buffer> a α
lnoremap <buffer> b β
lnoremap <buffer> c ψ
lnoremap <buffer> d δ
lnoremap <buffer> e ε
lnoremap <buffer> f φ
lnoremap <buffer> g γ
lnoremap <buffer> h| ῃ
lnoremap <buffer> h η
lnoremap <buffer> i ι
lnoremap <buffer> j ξ
lnoremap <buffer> k κ
lnoremap <buffer> l λ
lnoremap <buffer> m μ
lnoremap <buffer> n ν
lnoremap <buffer> o ο
lnoremap <buffer> p π
lnoremap <buffer> q ;
lnoremap <buffer> r ρ
lnoremap <buffer> s σ
lnoremap <buffer> t τ
lnoremap <buffer> u θ
lnoremap <buffer> v| ῳ
lnoremap <buffer> v ω
lnoremap <buffer> w ς
lnoremap <buffer> x χ
lnoremap <buffer> y υ
lnoremap <buffer> z ζ
lnoremap <buffer> || ͺ
lnoremap <buffer> ~:y ῧ
lnoremap <buffer> ~:i ῗ
lnoremap <buffer> ~: ῁
lnoremap <buffer> ~v| ῷ
lnoremap <buffer> ~h| ῇ
lnoremap <buffer> ~a| ᾷ
lnoremap <buffer> ~v ῶ
lnoremap <buffer> ~y ῦ
lnoremap <buffer> ~i ῖ
lnoremap <buffer> ~h ῆ
lnoremap <buffer> ~a ᾶ
lnoremap <buffer> ~ ῀
let &cpo=s:cpo_save
unlet s:cpo_save
setlocal keymap=greek_utf-8
setlocal noarabic
setlocal autoindent
setlocal backupcopy=
setlocal balloonexpr=
setlocal nobinary
setlocal nobreakindent
setlocal breakindentopt=
setlocal bufhidden=
setlocal buflisted
setlocal buftype=
setlocal nocindent
setlocal cinkeys=0{,0},0),0],:,0#,!^F,o,O,e
setlocal cinoptions=
setlocal cinscopedecls=public,protected,private
setlocal cinwords=if,else,while,do,for,switch
setlocal colorcolumn=86
setlocal comments=fb:*,fb:-,fb:+,n:>
setlocal commentstring=<!--\ %s\ -->
setlocal complete=.,w,b,u,t,i
setlocal concealcursor=
setlocal conceallevel=2
setlocal completefunc=
setlocal nocopyindent
setlocal cryptmethod=
setlocal nocursorbind
setlocal nocursorcolumn
set cursorline
setlocal cursorline
setlocal cursorlineopt=both
setlocal define=
setlocal dictionary=
setlocal nodiff
setlocal equalprg=
setlocal errorformat=
setlocal expandtab
if &filetype != 'rmd'
setlocal filetype=rmd
endif
setlocal fillchars=
setlocal fixendofline
setlocal foldcolumn=0
set nofoldenable
setlocal nofoldenable
setlocal foldexpr=0
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldmarker={{{,}}}
setlocal foldmethod=manual
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldtext=foldtext()
setlocal formatexpr=FormatRmd()
setlocal formatoptions=cqlnt
setlocal formatlistpat=^\\s*\\d\\+\\.\\s\\+\\|^\\s*[-*+]\\s\\+
setlocal formatprg=
setlocal grepprg=
setlocal iminsert=0
setlocal imsearch=-1
setlocal include=
setlocal includeexpr=
setlocal indentexpr=GetRmdIndent()
setlocal indentkeys=0{,0},<:>,!^F,o,O,e
setlocal noinfercase
setlocal iskeyword=@,48-57,_,.
setlocal keywordprg=
setlocal linebreak
setlocal nolisp
setlocal lispoptions=
setlocal lispwords=
setlocal nolist
setlocal listchars=
setlocal makeencoding=
setlocal makeprg=
setlocal matchpairs=(:),{:},[:]
setlocal modeline
setlocal modifiable
setlocal nrformats=bin,octal,hex
set number
setlocal number
setlocal numberwidth=4
setlocal omnifunc=CompleteR
setlocal path=
setlocal nopreserveindent
setlocal nopreviewwindow
setlocal quoteescape=\\
setlocal noreadonly
set relativenumber
setlocal relativenumber
setlocal norightleft
setlocal rightleftcmd=search
setlocal noscrollbind
setlocal scrolloff=-1
setlocal shiftwidth=2
setlocal noshortname
setlocal showbreak=
setlocal sidescrolloff=-1
setlocal signcolumn=auto
setlocal nosmartindent
setlocal nosmoothscroll
setlocal softtabstop=2
setlocal spell
setlocal spellcapcheck=[.?!]\\_[\\])'\"\	\ ]\\+
setlocal spellfile=
setlocal spelllang=en_us,el
setlocal spelloptions=
setlocal statusline=%!airline#statusline(1)
setlocal suffixesadd=
setlocal swapfile
setlocal synmaxcol=3000
if &syntax != 'rmd'
setlocal syntax=rmd
endif
setlocal tabstop=2
setlocal tagcase=
setlocal tagfunc=
setlocal tags=
setlocal termwinkey=
setlocal termwinscroll=10000
setlocal termwinsize=
setlocal textwidth=85
setlocal thesaurus=
setlocal thesaurusfunc=
setlocal undofile
setlocal undolevels=-123456
setlocal varsofttabstop=
setlocal vartabstop=
setlocal virtualedit=
setlocal wincolor=
setlocal nowinfixheight
setlocal nowinfixwidth
setlocal wrap
setlocal wrapmargin=0
silent! normal! zE
let &fdl = &fdl
let s:l = 193 - ((18 * winheight(0) + 20) / 41)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 193
normal! 0
tabnext 1
if exists('s:wipebuf') && len(win_findbuf(s:wipebuf)) == 0
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20
set shortmess=filnxtToOSc
let s:sx = expand("<sfile>:p:r")."x.vim"
if filereadable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &g:so = s:so_save | let &g:siso = s:siso_save
nohlsearch
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
