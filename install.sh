#/bin/bash

# Set up directories
mkdir ~/.dotfiles/local

# Link everything from here
ln -s ~/.dotfiles/.emacs.d ~/.emacs.d/

mv ~/.bashrc ~/.bashrc.old
ln -s ~/.dotfiles/bashrc ~/.bashrc
ln -s ~/.dotfiles/emacs.d ~/.emacs.d
ln -s ~/.dotfiles/gvimrc ~/.gvimrc
ln -s ~/.dotfiles/i3 ~/.i3
ln -s ~/.dotfiles/nvimrc ~/.nvimrc
ln -s ~/.dotfiles/screenrc ~/.screenrc
ln -s ~/.dotfiles/tmux.conf ~/.tmux.conf
ln -s ~/.dotfiles/vimrc ~/.vimrc
ln -s ~/.dotfiles/Xresources ~/.Xresources
ln -s ~/.dotfiles/yaourtrc ~/.yaourtrc
ln -s ~/.dotfiles/zshrc ~/.zshrc

echo '(org-babel-load-file "~/.emacs.d/config.org")' > ~/.dotfiles/emacs.d/init.el

# Install the tmux plugin manager
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
