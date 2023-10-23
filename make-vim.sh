#!/usr/bin/env sh

cp -v .vimrc ~

mkdir -p ~/.vim && cp -rv UltiSnips ~/.vim

curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

vim +PlugInstall +q +q

