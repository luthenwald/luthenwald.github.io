@_default:
   just -l

alias b := build
alias c := compile
alias p := preview

compile:
   zig build

build:
   ~/dev/lacuna/zig-out/bin/lacuna build src/ ./ luthenwald.github.io

preview:
   open ./index.html
