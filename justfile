@_default:
   just -l

build:
   ~/dev/lumen/zig-out/bin/lumen build src/ htmls/ --base-url luthenwald.github.io

fmt:
   prettier -uw --print-width 79 --tab-width 3 ./htmls
   xq --indent 3 < ./htmls/feed.xml > tmp.xml && mv tmp.xml ./htmls/feed.xml

preview:
   open ./index.html
