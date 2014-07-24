""""""""""""""""""""""""""""""""""""""""""""""""""
" ~/.vimrc
" Vim settings
"
" Sections:
"   -> General
"   -> Colors and fonts
"   -> Vim interface
"   -> Text, indenting, pasting
"   -> Search
"   -> Leader shortcuts
"   -> Plugins
"   -> Autocommands
""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""
" => General
""""""""""""""""""""""""""""""""""""""""""""""""""
" Don't break anything
set nocompatible

" Remember up to 250 lines of history
set history=250

" Filetype plugins
filetype plugin on

" Watch for external changes
set autoread

" Ask about changes instead of throwing an error
set confirm

" Open help files in a vertical split
au BufWinEnter *.txt* if &ft == 'help' | wincmd L | endif

""""""""""""""""""""""""""""""""""""""""""""""""""
" => Colors and fonts
""""""""""""""""""""""""""""""""""""""""""""""""""
" Syntax highlighting
syntax enable

" Make popups readable
hi Pmenusel ctermbg=red

""""""""""""""""""""""""""""""""""""""""""""""""""
" => Vim interface
""""""""""""""""""""""""""""""""""""""""""""""""""
" Turn on the wild menu
set wildmenu

set wildmode=longest

" Hide abandoned buffers
set hid

" Show current position
set ruler
set number

" Higher command window
set cmdheight=2

" Always display last status
set laststatus=2

" Backspace over autoindent, line breaks, and start of insert action
set backspace=indent,eol,start

" Show matching brackets
set showmatch
set mat=2

" Stop certain movements from returning to line start
set nostartofline

" Press jj to exit insert mode
inoremap jj <Esc>

" Better split panes
set splitbelow
set splitright

" Easier navigation between split windows
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

""""""""""""""""""""""""""""""""""""""""""""""""""
" => Text, indenting, pasting
""""""""""""""""""""""""""""""""""""""""""""""""""
" Use spaces over tabs
set expandtab

" Smart tabbing
set smarttab

" 1 tab = 4 spaces
set shiftwidth=4
set softtabstop=4

set showmode

" Delete trailing white space
set ai "Auto indent
set si "Smart indent
set wrap "Wrap lines

" Autocompletion
set omnifunc=syntaxcomplete#Complete

" Make Y behavior more consistent
nnoremap Y y$

""""""""""""""""""""""""""""""""""""""""""""""""""
" => Search
""""""""""""""""""""""""""""""""""""""""""""""""""
" Press space to clear search highlighting
nnoremap <silent> <Space> :silent noh<Bar>echo<CR>

" Case insensitive search, except for capital letters
set ignorecase
set smartcase

" Jump to first match
set incsearch

"Always highlight matches
set hlsearch

" Use literal regex
"nnoremap / /\v
"vnoremap / /\v

" Substitute globally by default
set gdefault

""""""""""""""""""""""""""""""""""""""""""""""""""
" => Leader shortcuts
""""""""""""""""""""""""""""""""""""""""""""""""""
" Set the leader key
let mapleader=","
let g:mapleader=","

" Open and jump to new split window
map <leader>nw :vne<cr><C-l>

" Open vimrc
map <leader>ev :vsp ~/.vimrc<cr>

" Shortcuts to call :make
" save and Make Project
map <leader>mp :w<CR>:make<CR>
" save and Make This
map <leader>mt :w<CR>:make %:r<CR><CR>
" Make Run (custom make target)
map <leader>mr :make run<CR>

" Expand working directory for quickly opening nearby files
cnoremap %% <C-R>=fnameescape(expand('%:h')).'/'<cr>
map <leader>ew :e %%
map <leader>es :sp %%
"map <leader>ev :vsp %%
map <leader>et :tabe %%

""""""""""""""""""""""""""""""""""""""""""""""""""
" => Plugins
""""""""""""""""""""""""""""""""""""""""""""""""""
filetype off

" Use Vundle
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'

if v:version >= 704 || v:version == 703 && has("patch584")
    Plugin 'Valloric/YouCompleteMe' " Program autocompletion (Vim 7.3.584+)
else
    Plugin 'vim-scripts/AutoComplPop'
endif

" Visual
Plugin 'bling/vim-airline' " Status line
Plugin 'altercation/vim-colors-solarized' " Solarized colorscheme

" Navigation
Plugin 'kien/ctrlp.vim'
Plugin 'tpope/vim-unimpaired' " Some nice bracket maps
Plugin 'nelstrom/vim-visual-star-search' " Use visual selection for * and #
"Plugin 'justinmk/vim-sneak' " Easy forward motion

" Coding
"Plugin 'MarcWeber/vim-addon-mw-utils'
"Plugin 'tomtom/tlib_vim'
Plugin 'honza/vim-snippets'
"Plugin 'garbas/vim-snipmate' " Depends on above three
Plugin 'sirver/ultisnips'

Plugin 'scrooloose/nerdcommenter' " Quick commenting
Plugin 'scrooloose/syntastic' " Show syntax errors
Plugin 'tpope/vim-surround' " Quickly surround text

" Git/Github
Plugin 'tpope/vim-fugitive' " Git integration
Plugin 'mattn/webapi-vim'
Plugin 'mattn/gist-vim' " Quickly upload gists

call vundle#end()

filetype plugin indent on

set background=dark
colorscheme solarized

""""""""""""""""""""""""""""""
" => Plugin settings
""""""""""""""""""""""""""""""

""""""""""""""""""""
" YouCompleteMe
""""""""""""""""""""
let g:ycm_global_ycm_extra_conf = '~/.vim/bundle/YouCompleteMe/ycm_extra_conf.py'

function! g:UltiSnips_Complete()
  call UltiSnips#ExpandSnippetOrJump()
  if g:ulti_expand_or_jump_res == 0
    if pumvisible()
      return "\<C-N>"
    else
      return "\<TAB>"
    endif
  endif

  return ""
  endif
endfunction

au BufEnter * exec "inoremap <silent> " . g:UltiSnipsExpandTrigger . " <C-R>=g:UltiSnips_Complete()<cr>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsListSnippets="<c-e>"
" this mapping Enter key to <C-y> to chose the current highlight item
" and close the selection list, same as other IDEs.
" CONFLICT with some plugins like tpope/Endwise
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

""""""""""""""""""""
" Ultisnips
""""""""""""""""""""
"let g:UltiSnipsExpandTrigger="<c-e>"

""""""""""""""""""""
" Fugitive
""""""""""""""""""""
set diffopt=vertical

""""""""""""""""""""
" Ctrlp
""""""""""""""""""""
let g:ctrlp_clear_cache_on_exit = 0
"let g:ctrlp_use_caching = 100
let g:ctrlp_open_multiple_files = '2vjr'
set wildignore+=*/.o

""""""""""""""""""""""""""""""""""""""""""""""""""
" => Autocommands
""""""""""""""""""""""""""""""""""""""""""""""""""
" Make shell scripts executable based on shebang
au BufwritePost * if getline("1") == '#!/bin/bash' | exe '!chmod u+x %' | endif

" Delete trailing white space on save, useful for Python and CoffeeScript
func! DeleteTrailingWS()
  exe "normal mz"
  %s/\s\+$//ge
  exe "normal `z"
endfunc
"autocmd BufWrite *.py,*.coffee :call DeleteTrailingWS()

" Source vimrc changes immediately
au bufwritepost .vimrc source $MYVIMRC
