_build/default/lib/gpb/bin/protoc-erl \
    -I ..proto \
    -o-erl apps/wt/src/ \
    -o-hrl apps/wt/include/ \
    ../proto/*.proto
