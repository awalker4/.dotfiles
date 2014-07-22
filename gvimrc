" Hide menus
set guioptions-=m  "remove menu bar
set guioptions-=T  "remove toolbar
set guioptions-=r  "remove right-hand scroll bar
set guioptions-=L  "remove left-hand scroll bar

set guifont=Inconsolata\ Medium\ 12

" Source gvimrc changes immediately
au bufwritepost .gvimrc source $MYGVIMRC
