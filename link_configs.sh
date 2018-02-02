#!/bin/bash
# unlink and link files.

THIS_DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd)
CONF_DIR=$THIS_DIR/ubuntu_configs

lnif() {
    if [ -e "$2" ];then
        unlink $2
    fi
    if [ -e "$1" ]; then
        ln -sf "$1" "$2"
    fi
}

# unlink and link .zshrc
echo ">>>> unlink and link ~/.zshrc"
SRC_ZSHRC=${CONF_DIR}/zshrc
DIST_ZSHRC=${HOME}/.zshrc
lnif $SRC_ZSHRC $DIST_ZSHRC

# unlink and link .emacs.d
echo ">>>> unlink and link ~/.emacs.d"
SRC_EMACS=${THIS_DIR}/emacs.d
DIST_EMACS=${HOME}/.emacs.d
lnif $SRC_EMACS $DIST_EMACS

# unlink and link terminator conf 
echo ">>>> unlink and link ~/.config/terminator/config" 
SRC_TERM=${CONF_DIR}/terminator.conf
DIST_TERM=${HOME}/.config/terminator/config
lnif $SRC_TERM $DIST_TERM
