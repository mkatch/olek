#!/bin/bash

source ~/.bash_profile

set -e

make_game ()
{
  if [ -e game ]; then
    mv game game.native
  fi
  ocamlbuild -use-ocamlfind -lflag -cclib -lflag "-framework Cocoa" game.native
  mv game.native game
}

make_editor ()
{
	if [ -e editor ]; then
		mv editor editor.native
	fi
	ocamlbuild -use-ocamlfind -lflag -cclib -lflag "-framework Cocoa" editor.native
	mv editor.native editor
}

if [ $# -eq 0 ]; then
  make_game
else
  while [ $# -gt 0 ]; do
    case $1 in
      game)   make_game;;
			editor) make_editor;;
      clean)  ocamlbuild -clean;;
      *)      echo "Unknown action $1";;
    esac;
    shift
  done
fi