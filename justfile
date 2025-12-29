@_default:
   just -l

b:
   ~/dev/lacuna/zig-out/bin/lacuna build src/ ./ luthenwald.github.io

preview:
   open ./index.html
