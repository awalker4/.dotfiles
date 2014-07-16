#!/bin/bash
# install.sh
# When on a fresh system, symlink all the relevant files back to here

dir=~/.dotfiles
olddir=~/dotfiles_old
files=$(ls | grep 'rc\|conf')

mkdir $olddir

for file in $files; do
    mv ~/.$file $olddir
    echo "Linking $file to ~/.$file"
    ln -s $dir/$file ~/.$file
done
