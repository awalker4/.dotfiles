#!/bin/bash
# install.sh
# When on a fresh system, symlink all the relevant files back to here

confDir=~/.dotfiles/conf
scriptDir=~/.dotfiles/scripts
olddir=~/dotfiles_old

rcfiles=$(ls conf)
scripts=$(ls scripts)

if [[ ! -d $olddir ]]; then
    mkdir $olddir
fi

echo "** Linking dotfiles **"
for file in $rcfiles; do
    if [[ -f ~/.$file ]]; then
        mv ~/.$file $olddir/$file
    elif [[ -h ~/.$file ]]; then
        rm ~/.$file
    fi

    echo "Linking $file to ~/.$file"
    ln -s $confDir/$file ~/.$file
done

echo "** Linking scripts **"
for file in $scripts; do
    if [[ -f ~/bin/$file ]]; then
        mv ~/bin/$file $olddir
    elif [[ -h ~/bin/$file ]]; then
        rm ~/bin/$file
    fi

    echo "Linking $file to ~/bin/$file"
    ln -s $scriptDir/$file ~/bin/$file
done
