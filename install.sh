#!/usr/bin/env bash

install_app() {
  local pkg_apt=$1
  local pkg_brew=${2:-$1}
  echo "Installing $pkg ..."
  if [ -f "/etc/debian_version" ]; then
    sudo apt-get install -y -qq $pkg_apt
  elif [ "`uname`" = "Darwin" ]; then
    brew install $pkg_brew
  else
    echo "unsupported OS detected."
    echo "Please manually install $pkg_apt"
  fi
}

if [ -d "$HOME/.emacs.d" ]; then
  echo "The directory .emacs.d already exists."
  echo "Moving .emacs.d -> .emacs.d.bak ..."
  sudo mv -f "$HOME/.emacs.d" "$HOME/.emacs.d.bak"
fi

if [ "`which git`" = "" ]; then
  install_app git
fi

git clone https://github.com/furushchev/dotemacs.git $HOME/.emacs.d
(cd $HOME/.emacs.d && git submodule update --init)

if [ "`which emacs`" = "" ]; then
  install_app emacs24-nox emacs
fi

if [ "`which pip`" = "" ]; then
  install_app python-pip
fi

sudo pip install -U virtualenv

emacs -batch --eval '(setq debug-on-error t)' -l $HOME/.emacs.d/init.el

echo "Installation is done!"
