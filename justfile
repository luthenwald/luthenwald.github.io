@_default:
   just -l

alias b := build
alias p := preview

build:
   ~/dev/lacuna/zig-out/bin/lacuna build src/ ./ luthenwald.github.io

preview:
   open ./index.html
