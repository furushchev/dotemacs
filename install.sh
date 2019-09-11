#!/usr/bin/env bash

set -e

setup_emacs() {
  if [ ! -e /etc/lsb-release ]; then
    echo "Only Ubuntu is supported"
    return
  fi

  . /etc/lsb-release
  if [ "$DISTRIB_ID" != "Ubuntu" ]; then
    echo "Only Ubuntu is supported"
    return
  fi

  if [ "$(which emacs)" = "" ]; then
    echo "Emacs is not installed"
    return
  fi

  EMACS_VERSION=${EMACS_VERSION:-"$(emacs --version | head -n1 | cut -d' ' -f3)"}

  echo "Installed emacs version: ${EMACS_VERSION}"

  if dpkg --compare-version ${EMACS_VERSION} "lt" 27; then
    EMACS_CONFIG_DIR=${EMACS_CONFIG_DIR:-"$HOME/.emacs.d"}
  else
    XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-"$HOME/.config"}
    EMACS_CONFIG_DIR=${EMACS_CONFIG_DIR:-"$XDG_CONFIG_HOME/emacs"}
  fi

  echo "Emacs configuration directory: ${EMACS_CONFIG_DIR}"

  if [ -d "${EMACS_CONFIG_DIR}" ]; then
    echo "The directory ${EMACS_CONFIG_DIR} already exists."
    echo "Moving ${EMACS_CONFIG_DIR} -> ${EMACS_CONFIG_DIR}.bak ..."
    sudo mv -f "${EMACS_CONFIG_DIR}" "${EMACS_CONFIG_DIR}.bak"
  fi

  sudo apt-get update -qq

  if [ "`which git`" = "" ]; then
    echo "Installing git..."
    sudo apt-get install -qq -y git
  fi

  git clone https://github.com/furushchev/dotemacs.git ${EMACS_CONFIG_DIR}
  (cd ${EMACS_CONFIG_DIR} && git submodule update --init)

  if [ "`which pip`" = "" ]; then
    sudo apt-get install -qq -y python-pip
  fi

  if [ "`which clang`" = "" ]; then
    sudo apt-get install -qq -y clang clang-dev
  fi

  sudo pip install -U virtualenv jedi

  emacs -batch --eval '(setq debug-on-error t)' -l ${EMACS_CONFIG_DIR}/init.el

  echo "Installation is done!"
}

setup_emacs
