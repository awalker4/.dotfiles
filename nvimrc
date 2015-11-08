""""""""""""""""""""""""""""""""""""""""""""""""""
" ~/.nvimrc
" nvim settings
"
" Inspired by Steve Losh's vimrc
" https://bitbucket.org/sjl/dotfiles/src/tip/vim/vimrc
"
" Sections:
"   -> General
"   -> Wildmenu
"   -> Tabs
"   -> Backups
"   -> Remappings 
"   -> Navigation
"   -> Searching
"   -> Leader shortcuts
"   -> Terminal
"   -> Plugins
    "   -> Plugin settings
"   -> Colors 
""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""
" => General
""""""""""""""""""""""""""""""""""""""""""""""""""
set encoding =utf-8
set undofile 
set undoreload=10000
set hidden
set number
set relativenumber
set ruler " line and column number
set cmdheight=2
set showmode
set showcmd
set laststatus=2 " always show status line
set lazyredraw " don't redraw when in the middle of a command
set confirm " ask about saving changes
set splitbelow
set splitright
set list
set listchars=tab:▸\ ,eol:¬,extends:❯,precedes:❮
set shiftround " round shifting to nearest indent

" Stop certain movements from returning to line start
set nostartofline

" Show matching brackets
set showmatch
set mat=2

" When resizing vim, make splits equal size
augroup splits
    au!
    au VimResized * :wincmd =
augroup END

" Open help files in a vertical split
au BufWinEnter *.txt* if &ft == 'help' | wincmd L | endif

" Jump to previous line when opening
augroup line_return
    au!
    au BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \     execute 'normal! g`"zvzz' |
        \ endif
augroup END

" Source nvimrc changes immediately
augroup sources
    au!
    au bufwritepost .nvimrc source $MYVIMRC
augroup END

" Autocompletion
set omnifunc=syntaxcomplete#Complete

""""""""""""""""""""""""""""""""""""""""""""""""""
" => Wildmenu
""""""""""""""""""""""""""""""""""""""""""""""""""
set wildmenu
set wildmode=list:longest

set wildignore+=*.o,*.obj,*.exe,*.out

""""""""""""""""""""""""""""""""""""""""""""""""""
" => Tabs
""""""""""""""""""""""""""""""""""""""""""""""""""
" 1 tab = 4 spaces
set shiftwidth=4
set softtabstop=4

" Use spaces over tabs
set expandtab

" Delete trailing white space
set ai "Auto indent
set si "Smart indent
set wrap "Wrap lines

""""""""""""""""""""""""""""""""""""""""""""""""""
" => Backups
""""""""""""""""""""""""""""""""""""""""""""""""""
set backup
set noswapfile

" Extra slash will add path to filenames
set undodir=~/.vim/tmp/undo//
set backupdir=~/.vim/tmp/backup//
set directory=~/.vim/tmp/swap//

" Create directories if needed
if !isdirectory(expand(&undodir))
    call mkdir(expand(&undodir), "p")
endif
if !isdirectory(expand(&backupdir))
    call mkdir(expand(&backupdir), "p")
endif
if !isdirectory(expand(&directory))
    call mkdir(expand(&directory), "p")
endif


""""""""""""""""""""""""""""""""""""""""""""""""""
" => Remappings
""""""""""""""""""""""""""""""""""""""""""""""""""
" Split line at cursor (opposite of J)
nnoremap S i<cr><esc>^mwgk:silent! s/\v +$//<cr>:noh<cr>'w

" Select (charwise) the contents of the current line, excluding indentation.
nnoremap vv ^vg_

" From insert mode, make the previous word uppercase
inoremap <C-u> <esc>mzgUiw`za

" Press jk to exit insert mode
inoremap jk <Esc>
inoremap kj <Esc>

" Better split panes

" Easier navigation between split windows
nnoremap <M-j> <C-W><C-J>
nnoremap <M-k> <C-W><C-K>
nnoremap <M-l> <C-W><C-L>
nnoremap <M-h> <C-W><C-H>

" Make Y behavior more consistent
nnoremap Y y$

""""""""""""""""""""""""""""""""""""""""""""""""""
" => Navigation
""""""""""""""""""""""""""""""""""""""""""""""""""
" Move through wrapped lines
nnoremap j gj
nnoremap k gk

noremap H ^
noremap L $
vnoremap L g_

""""""""""""""""""""""""""""""""""""""""""""""""""
" => Searching
""""""""""""""""""""""""""""""""""""""""""""""""""
" Case insensitive search, except for capital letters
set smartcase
set ignorecase

" Substitute globally by default
set gdefault

" Clear search highlighting
map <leader>h :silent! noh<Bar> call clearmatches()<Bar> echo<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""
" => Leader shortcuts
""""""""""""""""""""""""""""""""""""""""""""""""""
" Set the leader key
let mapleader=" "
let g:mapleader=" "

""""""""""""""""""""""""""""""
" => Buffer/window shortcuts
""""""""""""""""""""""""""""""
" New Window
map <leader>nw :vne<cr><C-l>

" Edit Makefile
map <leader>em :vsp makefile<cr>

" Edit Vimrc
map <leader>ei :e $MYVIMRC<cr>

" Splits
map <leader>sp :sp<cr>
map <leader>sv :vsp<cr>

""""""""""""""""""""""""""""""
" => Make shortcuts
""""""""""""""""""""""""""""""
" Make LaTeX
map <leader>ml :w<CR>:!pdflatex -synctex=1 -interaction=nonstopmode %<CR>

" Make Project
map <leader>mp :wa<CR>:make<CR>

" Make Run (custom target)
map <leader>mr :make run<CR>

" Make Script
map <leader>ms :w<CR>:!./%<CR>

" Make This
map <leader>mt :w<CR>:make %:r<CR>

" Run current line in bash (Bash This)
map <leader>bt :.w !bash<CR>

" Expand working directory for quickly opening nearby files
cnoremap %% <C-R>=fnameescape(expand('%:h')).'/'<cr>
map <leader>ew :e %%
map <leader>es :sp %%
"map <leader>ev :vsp %%
map <leader>et :tabe %%

""""""""""""""""""""""""""""""""""""""""""""""""""
" => Terminal
""""""""""""""""""""""""""""""""""""""""""""""""""
" Esc to get to normal mode
:tnoremap <Esc> <C-\><C-n>

""""""""""""""""""""""""""""""""""""""""""""""""""
" => Plugins
""""""""""""""""""""""""""""""""""""""""""""""""""
call plug#begin('~/.vim/plugged')

Plug 'vim-scripts/AutoComplPop'

" Visual
Plug 'bling/vim-airline' " Status line
"Plug 'altercation/vim-colors-solarized' " Solarized colorscheme

" Navigation
Plug 'kien/ctrlp.vim'
Plug 'tpope/vim-unimpaired' " Some nice bracket maps
Plug 'nelstrom/vim-visual-star-search' " Use visual selection for * and #

Plug 'scrooloose/nerdcommenter' " Quick commenting
Plug 'scrooloose/syntastic' " Show syntax errors
Plug 'tpope/vim-surround' " Quickly surround text

call plug#end()

""""""""""""""""""""""""""""""
" => Plugin settings
""""""""""""""""""""""""""""""

""""""""""""""""""""
" Ctrlp
""""""""""""""""""""
"let g:ctrlp_clear_cache_on_exit = 0
let g:ctrlp_use_caching = 500
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_open_multiple_files = '2vjr'

nnoremap <leader>bs :CtrlPBuffer<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""
" => Colors 
""""""""""""""""""""""""""""""""""""""""""""""""""
" Syntax highlighting
syntax on
"set background=dark

" Make popups readable
hi Pmenusel ctermbg=red

"colorscheme solarized

" Highlight Word (Thanks Steve Losh!)
"
" This mini-plugin provides a few mappings for highlighting words temporarily.
"
" Sometimes you're looking at a hairy piece of code and would like a certain
" word or two to stand out temporarily.  You can search for it, but that only
" gives you one color of highlighting.  Now you can use <leader>N where N is
" a number from 1-6 to highlight the current word in a specific color.

function! HiInterestingWord(n) " {{{
    " Save our location.
    normal! mz

    " Yank the current word into the z register.
    normal! "zyiw

    " Calculate an arbitrary match ID.  Hopefully nothing else is using it.
    let mid = 86750 + a:n

    " Clear existing matches, but don't worry if they don't exist.
    silent! call matchdelete(mid)

    " Construct a literal pattern that has to match at boundaries.
    let pat = '\V\<' . escape(@z, '\') . '\>'

    " Actually match the words.
    call matchadd("InterestingWord" . a:n, pat, 1, mid)

    " Move back to our original location.
    normal! `z
endfunction " }}}

hi def InterestingWord1 guifg=#000000 ctermfg=16 guibg=#ffa724 ctermbg=214
hi def InterestingWord2 guifg=#000000 ctermfg=16 guibg=#aeee00 ctermbg=154
hi def InterestingWord3 guifg=#000000 ctermfg=16 guibg=#8cffba ctermbg=121
hi def InterestingWord4 guifg=#000000 ctermfg=16 guibg=#b88853 ctermbg=137
hi def InterestingWord5 guifg=#000000 ctermfg=16 guibg=#ff9eb8 ctermbg=211
hi def InterestingWord6 guifg=#000000 ctermfg=16 guibg=#ff2c4b ctermbg=195

nnoremap <silent> <leader>1 :call HiInterestingWord(1)<cr>
nnoremap <silent> <leader>2 :call HiInterestingWord(2)<cr>
nnoremap <silent> <leader>3 :call HiInterestingWord(3)<cr>
nnoremap <silent> <leader>4 :call HiInterestingWord(4)<cr>
nnoremap <silent> <leader>5 :call HiInterestingWord(5)<cr>
nnoremap <silent> <leader>6 :call HiInterestingWord(6)<cr>
