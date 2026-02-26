@_default:
   just -l

alias b := build
alias c := compile
alias p := preview

# compile the src->site program
compile:
   rm -rf ./.zig-cache
   zig build

# build the website
build:
   './zig-out/bin/src->site' ./src ./ luthenwald.github.io

# open the built website
preview:
   open ./index.html
