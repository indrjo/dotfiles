#PHONY: all emacs vim

all: emacs vim

emacs:
	@mkdir -p ~/.emacs.d/ 
	@cp init.el ~/.emacs.d/
	@emacs --batch -l ~/.emacs.d/init.el 

vim:
	@cp -v vimrc ~/.vimrc
	@mkdir -p ~/.vim
	@curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
  		https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	@vim +PlugInstall +q +q
