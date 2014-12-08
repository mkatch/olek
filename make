#!/bin/sh

set -e

make_game ()
{
  if [ -e game ]; then
    mv game game.native
  fi
  ocamlbuild -use-ocamlfind -lflag -cclib -lflag "-framework Cocoa" game.native
  mv game.native game
}

if [ $# -eq 0 ]; then
  rule all
else
  while [ $# -gt 0 ]; do
    case $1 in
      game)  make_game;;
      clean) ocamlbuild -clean;;
      *)     echo "Unknown action $1";;
    esac;
    shift
  done
fi