""""""""""""""""""""""""""""""""""""""""""""""""""
" ~/.vimrc
"
" Sections:
"   -> General
"   -> Vim interface
"   -> Colors and fonts
"   -> Text, indenting, pasting
"   -> Search
"   -> Files, backups, undo
"   -> Additional functions
""""""""""""""""""""""""""""""""""""""""""""""""""



""""""""""""""""""""""""""""""""""""""""""""""""""
" => General
""""""""""""""""""""""""""""""""""""""""""""""""""
" Don't break anything
set nocompatible

" Remember up to 1000 lines
set history=1000

" Filetype plugins
filetype plugin on
filetype indent on

" Watch for external changes
set autoread

" Set the map leader
let mapleader=","
let g:mapleader=","

" Ask about changes
set confirm



""""""""""""""""""""""""""""""""""""""""""""""""""
" => Vim interface
""""""""""""""""""""""""""""""""""""""""""""""""""
" Turn on the wild menu
set wildmenu

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
" => Colors and fonts
""""""""""""""""""""""""""""""""""""""""""""""""""
" Syntax highlighting
syntax enable



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

" Use F2 to toggle pasting from outside vim
nnoremap <F2> :set invpaste paste?<CR>
set pastetoggle=<F2>
set showmode

" Delete trailing white spset ai "Auto indent
set si "Smart indent
set wrap "Wrap lines



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



""""""""""""""""""""""""""""""""""""""""""""""""""
" => Files, baskups, undo
""""""""""""""""""""""""""""""""""""""""""""""""""
" Turn off backups
set nobackup
set nowb
set noswapfile



""""""""""""""""""""""""""""""""""""""""""""""""""
" => Additional functions
""""""""""""""""""""""""""""""""""""""""""""""""""
" Delete trailing white space on save, useful for Python and CoffeeScript
func! DeleteTrailingWS()
  exe "normal mz"
  %s/\s\+$//ge
  exe "normal `z"
endfunc
autocmd BufWrite *.py :call DeleteTrailingWS()
autocmd BufWrite *.coffee :call DeleteTrailingWS()
