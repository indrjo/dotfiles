#!/bin/sh

# **********
# * SYSTEM *
# **********

# Getting rid of useless garbage.
alias clean='pacman -Qtdq | sudo pacman -Rnsu -'

# The same as above, just in case yay is installed.
alias yayclean='yay -Qtdq | yay -Rnsu -'

# Regenerating the file grub.cfg. It is just a shorthand.
regen-grub-cfg () {
  sudo grub-mkconfig -o /boot/grub/grub.cfg
}

# Update the list of mirrors for Arch Linux updates. Servers from Germany,
# France and Italy are interrogated due to geographical conveniences. I may
# use also servers from Austria and Switzerland as well.
update-arch-mirrors () {
  sudo reflector \
    --latest 6 \
    --sort rate \
    --protocol https,http \
    --country Germany,France,Italy \
    --save /etc/pacman.d/mirrorlist
}

# Make sure there exists the directory .tmp in your home. This place has no
# particular reason to be. It is just a space where to cram stuff that will
# be removed in a shot time.
[ -d ~/.tmp ] || mkdir -p ~/.tmp

# Make sure there is the directory where local bins are stored and push it
# to $PATH. Many GNU/Linux distros have that directory in their path and it
# would be nice to have. For example, pip installs stuff there and if it is
# not in $PATH, it will recommend you to insert it.
[ -d ~/.local/bin ] || mkdir -p ~/.local/bin
export PATH=~/.local/bin:$PATH

# Generate passwords. If no argument is given, the length is by deafult 12.
# It seems to me a nice default which will do most of the times. Of course,
# longer passwords are safer, but some websites accept up to 8 characters.
pw-gen () {
  [ -z $1 ] && len=12 || len=$1
  # Reminder: -c capital letters
  #           -n numbers
  #           -y special symbols
  #           -s secure passwords
  pwgen -cnys $len 1
}

# ***********
# * HASKELL *
# ***********

# Set the environment for Haskell. We rely on ghcup here.
[ -f ~/.ghcup/env ] && source ~/.ghcup/env
alias ghci='ghci -Wall'

# All the temporary stuff downloaded by ghcup is crammed into ~/.tmp.
alias ghcup='TMPDIR=~/.tmp ghcup'

# **********
# * RACKET *
# **********

# Make visible the binaries of racket.
[ -d ~/racket/bin ] && export PATH=~/racket/bin:$PATH

# **********
# * PYTHON *
# **********

alias py='python3'

# ********
# * PERL *
# ********

# Add Perl directories to $PATH.
PATH="/home/indrjo/perl5/bin${PATH:+:${PATH}}"
export PATH
PERL5LIB="/home/indrjo/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"
export PERL5LIB
PERL_LOCAL_LIB_ROOT="/home/indrjo/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"
export PERL_LOCAL_LIB_ROOT
PERL_MB_OPT="--install_base \"/home/indrjo/perl5\""
export PERL_MB_OPT
PERL_MM_OPT="INSTALL_BASE=/home/indrjo/perl5"
export PERL_MM_OPT

alias pl='perl'

# ********
# * RUBY *
# ********

# Add Ruby paths.
export GEM_HOME=~/gems
export PATH=~/.gem/ruby/3.0.0/bin:$PATH
export PATH=~/.local/share/gem/ruby/3.0.0/bin:$PATH
export PATH=~/gems/bin:$PATH

# *******
# * TEX *
# *******

# $PATH is fixed in an autonomous script.
[ -f ~/.tlrc ] && source ~/.tlrc

# This function accepts a filename, the one of the warning
#
#  ! LaTeX Error: File `FILENAME' not found.
#
# Interrogate CTAN for packages containing that file and install them.
tlmgr-install-missing () {
  tlmgr search --global --file "/$1" | \
    #grep -P ':\s*$' | sed 's/\s*:$//' | \
    perl -lne '/^\s*([^:]+):$/ && print $1' | \
      xargs tlmgr install
}

# The function below takes a given file (in our case a *.tex file) and
# from the lines containing '\usepackage{PACKAGE}' extract PACKAGE and
# install it immediately.
install-tex-pkgs () {
  perl -lne 'm!^\s*\\usepackage[^\{]*\{([^\}]+)\}! && print "tlmgr install $1"' $1
}

# *******************
# * DOWNLOAD VIDEOS *
# *******************

alias yt='yt-dlp -N 2 -o "%(title)s.%(ext)s"'

# *******
# * GIT *
# *******

# Just avoid the function below.
#git-zero () {
#  git checkout --orphan temp-branch
#  git add --all
#  now=$(date +"%H:%M | %d %b %Y")
#  git commit -am "reborn: $now"
#  git branch -D main
#  git branch -m main
#  #wget -q --spider "https://github.com"
#  #[ $? == 0 ] && git push -uf origin main
#}

# Within a git repo read .gitignore and delete all the corresponding stuff
# inside the same tree. Observe that you should run this command from the
# base of your repository.
clean-git-repo () {
  grep -vP '^\s*(#|$)' .gitignore | \
    xargs -I % find . -not -path './.git/*' -type f -name % -delete
}

# *******
# * ADB *
# *******

# Disable a given app.
alias adb-disable='adb shell pm disable-user --user 0'

# Enable a disabled app
alias adb-enable='adb shell pm enable'

# Uninstall an app
alias adb-uninstall='adb shell pm uninstall -k --user 0'

# Reinstall an app.
alias adb-reinstall='adb shell pm install-existing'

# *********************
# * CREATING LIVE USB *
# *********************

# When you have to create a Live USB.
make-live-usb () {
  sudo mkfs.vfat /dev/sdb -I
  sudo dd if=$1 of=/dev/sdb bs=4M status=progress conv=fsync
}

