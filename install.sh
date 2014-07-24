#!/bin/bash
# install.sh
# When on a fresh system, symlink all the relevant files back to here

dir=~/.dotfiles
olddir=~/dotfiles_old
rcfiles=$(ls | grep 'rc\|conf')
scripts=$(ls scripts)

if [[ ! -d $olddir ]]; then
    mkdir $olddir
fi

echo "** Linking dotfiles **"
for file in $rcfiles; do
    if [[ -f ~/.$file ]]; then
        mv ~/.$file $olddir
    fi

    echo "Linking $file to ~/.$file"
    ln -s $dir/$file ~/.$file
done

echo "** Linking scripts **"
for file in $scripts; do
    if [[ -f ~/bin/$file ]]; then
        mv ~/bin/$file $olddir
    fi

    echo "Linking $file to ~/bin/$file"
    ln -s $dir/scripts/$file ~/bin/$file
done
