#!/bin/sh
rebar clean
rebar eunit || exit -1
rebar compile || exit -1
mkdir -p build
mkdir -p dist

MODULES="common leveldb yaws csv eunit io"

for m in $MODULES
do
    MODOUT=build/strikead_$m
    mkdir -p $MODOUT

    cp -v -r $m/ebin $MODOUT
    [ -d $m/include ] && cp -v -r $m/include $MODOUT
done

tar -czf dist/strikead-erlang-commons.tar.gz -C build .
