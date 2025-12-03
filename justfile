@_default:
   just -l

b:
   ~/dev/lumen/zig-out/bin/lumen build src/ ./ luthenwald.github.io

preview:
   open ./index.html
