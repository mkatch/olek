#!/bin/bash

#source ~/.bash_profile

#set -e

make ()
{
  if [ -e $1 ]; then
    mv $1 $1.native
  fi
  if [[ `uname` == "Darwin" ]]; then
    ocamlbuild -use-ocamlfind -lflag -cclib -lflag "-framework Cocoa" $1.native
  else
    ocamlbuild -use-ocamlfind $1.native
  fi
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