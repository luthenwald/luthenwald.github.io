@_default:
   just -l

alias b := build
alias c := compile
alias p := preview

compile:
   rm -rf ./.zig-cache
   zig build

build:
   './zig-out/bin/src->site' ./src ./ luthenwald.github.io

preview:
   open ./index.html
