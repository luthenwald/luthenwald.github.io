@_default:
   just -l

b:
   rm -rfd ./htmls
   ~/dev/lumen/zig-out/bin/lumen build src/ htmls/ luthenwald.github.io

preview:
   open ./index.html
