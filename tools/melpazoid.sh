#!/bin/bash -ex

EMACS="${EMACS:=emacs}"
BASENAME=$(basename "$1")
PYTHON=$(which python3.6)
PYTHON="${PYTHON:=python}"

if [[ -z $(du -s melpazoid-master 2>/dev/null | cut -f1) ]] || \
       [[ $(du -s melpazoid-master 2>/dev/null | cut -f1) -le "100" ]] ; then
    curl -sLk -O https://github.com/riscy/melpazoid/archive/master.zip
    unzip master.zip
    rm -f master.zip
fi

cd $(git rev-parse --show-toplevel)
PKG_PATH="$(pwd)/melpazoid-master/$(basename $(pwd))"
PKG_NAME=$(basename "$PKG_PATH")
mkdir -p ${PKG_PATH}
rsync -av --files-from=<(cask files) . ${PKG_PATH}
if [ -s "$(pwd)/LICENSE" ]; then
  cp -p "$(pwd)/LICENSE" ${PKG_PATH}
fi
cd melpazoid-master
${PYTHON} -m pip install --user -U .
sed -i -e 's/ -it / -i /' Makefile
sed -i -e 's/ -ti / -i /' Makefile
PKG_PATH=${PKG_PATH} PKG_NAME=${PKG_NAME} make run
