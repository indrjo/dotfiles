#!/usr/bin/env sh

set -o pipefail

# **********
# * SYSTEM *
# **********

# Make sure there exists the directory ~/.tmp in your home. It is a space
# where to cram stuff that will be removed in a shot time.
[ -d ~/.tmp ] || mkdir -p ~/.tmp

# Make sure there is the directory where local binaries are stored and push
# it to $PATH. Many GNU/Linux distros have that directory in their path and
# it would be nice to have here.
[ -d ~/.local/bin ] || mkdir -p ~/.local/bin
(echo "$PATH" | grep -q ~/.local/bin) || export PATH=~/.local/bin:$PATH

# Generate ten passwords of a given length [default = 20].
pw-gen () {
  pwgen -cnys1 ${1:-20} 10
}

# Shred files.
alias shred='shred -uvz'

# **********
# * PYTHON *
# **********

alias py='python3'
eval "$(register-python-argcomplete pipx)"

# ********
# * PERL *
# ********

alias pl='perl'

# Your local Perl.
PATH="$HOME/perl5/bin${PATH:+:${PATH}}"
export PATH
PERL5LIB="$HOME/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"
export PERL5LIB
PERL_LOCAL_LIB_ROOT="$HOME/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"
export PERL_LOCAL_LIB_ROOT
PERL_MB_OPT="--install_base \"$HOME/perl5\""
export PERL_MB_OPT
PERL_MM_OPT="INSTALL_BASE=$HOME/perl5"
export PERL_MM_OPT

# *******************
# * DOWNLOAD VIDEOS *
# *******************

alias yt='yt-dlp --restrict-filenames -o "%(title)s.%(ext)s"'
alias smallest-yt='yt -S "+size,+br"'

# *******
# * GIT *
# *******

# !!! Just avoid the function below. !!!
git-zero () {
  git checkout --orphan temp-branch
  git add --all
  now=$(date +"%H:%M | %d %b %Y")
  git commit -am "reborn $now"
  git branch -D main
  git branch -m main
}

# Within a git repo read .gitignore and delete all the corresponding stuff
# inside the same tree. Observe that you should run this command from the
# base of your repository.
clean-git-repo () {
  grep -vP '^\s*(#|$)' .gitignore \
    | xargs -I % find . -path './.git' -prune -o -iname '%' -delete
}

