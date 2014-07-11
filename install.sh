#!/bin/bash
# install.sh
# When on a fresh system, symlink all the relevant files back to here

dir=~/.dotfiles
olddir=~/dotfiles_old

cd $dir
files=$(ls *rc*)
mkdir $olddir

for file in $files; do
    echo "Linking $file to ~/.$file"
    mv ~/.$file $olddir
    ln -s $dir/$file ~/.$file
done
