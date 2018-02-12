#!/bin/bash
#
# File: uinstall.sh
# Author: Xuancong Lee[congleetea] <congleetea@gmail.com>
# Created: Monday, February 22 2016
#

# sudo apt-get update
THISDIR=$( cd "$( dirname "${BASH_SOURCE[0]}")" && pwd )
SWDIR=${THISDIR}/ubuntu_pkgs
CONFIGDIR=${THISDIR}/ubuntu_configs

EMACS_VER=24.5
ERLANG_VER=19.3
sws=(
    # # shell
    # vim \
    # trash-cli \
    # sougou 
    # terminator
    # zsh 
    shadowsocks 
    # tree 
    # ssh
    # openssl
    # openssl-client
    # openssl-server
    # silversearcher-ag \
    # emacs \
    # curl \
    # espeak \
    # shutter \
    # albert \
    # # basic
    # calibre \
    # graphviz \
    # # interactive
    # erlang \
    # golang \
    # inkscape \
    # # optional
    # ansible
)

function interactive_install() {
    echo "!!!!!!!!! interactive installation  !!!!!!!!!!!!"
}

function install(){
    for sw in ${sws[@]}
    do
        echo ""
        echo "===================================================="
        echo "================ install $sw " 
        echo "===================================================="
        if [ $sw == "git" ];then
            interactive_install
            sudo add-apt-repository ppa:git-core/ppa
            sudo apt-get update
            sudo apt-get install -y git
            sudo apt-get install -y meld
        elif [ $sw == "sougou" ];then
            interactive_install
            sudo apt-get install -y fcitx libssh2-1
            cd ${SWDIR} && wget "http://pinyin.sogou.com/linux/download.php?f=linux&bit=64" -O "sougou_64.deb"
            sudo dpkg -i sougou_64.deb
        elif [ $sw == "zsh" ];then
            cd /home/$USER 
            if [ ! -d "/home/$USER/.oh-my-zsh" ];then
                git clone git://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
            fi
            sudo apt-get install -y zsh
            chsh -s /bin/zsh
            git clone https://github.com/joelthelion/autojump.git
            cd autojump && ./install.py
            echo "[[ -s /home/${USER}/.autojump/etc/profile.d/autojump.sh ]] && source /home/${USER}/.autojump/etc/profile.d/autojump.sh" >> ~/.zshrc
            echo "autoload -U compinit && compinit -u"
            git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
            ~/.fzf/install
            source ~/.zshrc
        elif [ $sw == "emacs" ];then
            cd ${SWDIR}
            sudo apt-get install -y build-essential texinfo libx11-dev libxpm-dev \
                 libgif-dev libxaw7-dev libjpeg-dev libpng12-dev libtiff4-dev \
                 libncurses5-dev xsel magit
            tar xvf emacs-${EMACS_VER}.tar.gz
            cd emacs-${EMACS_VER} && ./configure && make && sudo make install
            cd ${SWDIR}
            tar xvf global-6.5.7.tar.gz
            sudo apt-get install exuberant-ctags
            cd global-6.5.7 && ./configure && make && sudo make install
            echo "export GTAGSCONF=/usr/local/share/gtags/gtags.conf" >> ~/.zshrc
            echo "export GTAGSLABEL=ctags gtags" >> ~/.zshrc
        elif [ $sw == "erlang" ]; then
            cd ${SWDIR}
            wget http://erlang.org/download/otp_src_$ERLANG_VER.tar.gz
            wget http://erlang.org/download/otp_doc_man_$ERLANG_VER.tar.gz
            tar xvf otp_src_$ERLANG_VER.tar.gz
            sudo apt-get install -y build-essential autoconf m4 libncurses5-dev libwxgtk2.8-dev \
                 libgl1-mesa-dev libglu1-mesa-dev libpng3 libssh-dev unixodbc-dev
            cd otp_src_$ERLANG_VER && ./configure && make && sudo make install
            cd ${SWDIR}
            tar xvf otp_doc_man_$ERLANG_VER.tar.gz
            sudo cp -r man /usr/local/lib/erlang/

            sudo apt-get install rlwrap  # 可以用以erlang的历史命令记录。
            # 在zshrc中添加：alias erl='/usr/bin/rlwrap -a erl'
        elif [ $sw == "golang" ]; then
            GOVERSION=1.7.5
            GOTO=/usr/local
            GOROOT=${GOTO}/go
            GOPATH=/home/${USER}/gitlab/go
            if [ ! -d ${GOROOT} ]; then
                sudo rm ${GOROOT} -rf
            fi
            if [ ! -d ${GOPATH} ]; then
                mkdir -p ${GOPATH}
            fi
            sudo tar -C /usr/local -xzf ${SWDIR}/go${GOVERSION}.linux-amd64.tar.gz
            echo "export GOROOT=${GOROOT}" >> ~/.zshrc
            echo "export GOPATH=${GOPATH}" >> ~/.zshrc
            echo "export PATH=\$PATH:${GOROOT}/bin:${GOPATH}/bin" >> ~/.zshrc
            echo "GOPATH: $GOPATH"
            source ~/.zshrc
        elif [ $sw == "shadowsocks" ]; then
            sudo apt-get install -y inkscape
            sudo apt-get install -y nautilus-open-terminal
            # instalation for shadowsocks in terminator
            # http://droidyue.com/blog/2016/04/04/set-shadowsocks-proxy-for-terminal/index.html
            sudo apt-get install python-pip
            sudo pip install --upgrade git+https://github.com/shadowsocks/shadowsocks.git@master
	    cd ${SWDIR} && tar xvf libsodium.tar.gz && cd libsodium-stable && ./configure --prefix=/usr && sudo make install
            sudo apt-get install -y polipo
            sudo cp ${CONFIGDIR}/polipo.conf /etc/polipo/config
            sudo service polipo stop
            sudo service polipo start
        elif [ $sw == "ansible" ]; then
            sudo apt-get install -y software-properties-common
            sudo apt-add-repository ppa:ansible/ansible
            sudo apt-get update
            sudo apt-get install -y ansible
        elif [ $sw == "albert" ]; then
            # https://github.com/albertlauncher/albert/issues/123
            interactive_install
            sudo add-apt-repository ppa:nilarimogard/webupd8
            sudo apt-get update
            sudo apt-get install albert
            ExnDir="~/.local/share/albert/external"
            if [ ! -d $ExnDir ];then
                mkdir -p $ExnDir 
            fi
            cp $CONFIGDIR/org.albert.extension.external.switchapp.py $ExnDir 
        else
            sudo apt-get install -y $sw
        fi
    done
}

install
