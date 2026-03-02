@_default:
   just -l

alias b := build
alias c := compile
alias p := preview

# compile the site generator
compile:
   builtin cd site && cabal build

# build the website
build:
   builtin cd site && cabal run site -- ../src ../ luthenwald.github.io

# open the built website
preview:
   open ./index.html
