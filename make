#!/bin/bash

#source ~/.bash_profile

set -e

make ()
{
  if [ -e $1 ]; then
    mv $1 $1.native
  fi
  OCAMLBUILD_FLAGS="-use-ocamlfind"
  if [[ `uname` == "OS X" ]]; then
    OCAMLBUILD_FLAGS="$OCAMLBUILD_FLAGS:-lflag -cclib -lflag \"-framework Cocoa\""
  fi
  ocamlbuild $OCAMLBUILD_FLAGS $1.native
  mv $1.native $1
}

while [ $# -gt 0 ]; do
  case $1 in
    game)   make "game";;
    editor) make "editor";;
    clean)  ocamlbuild -clean;;
    *)      echo "Unknown action $1";;
  esac;
  shift
done