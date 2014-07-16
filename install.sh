#!/bin/bash
# install.sh
# When on a fresh system, symlink all the relevant files back to here

dir=~/.dotfiles
olddir=~/dotfiles_old
files=$(ls | grep 'rc\|conf')

if [[ ! -d $olddir ]]; then
    mkdir $olddir
fi

for file in $files; do
    if [[ -f ~/.$file ]]; then
        mv ~/.$file $olddir
    fi

    echo "Linking $file to ~/.$file"
    ln -s $dir/$file ~/.$file
done
