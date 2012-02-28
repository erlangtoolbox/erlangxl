#!/bin/sh
rm -rf build dist
rebar clean
rebar eunit || exit -1
rebar compile || exit -1
mkdir -p build
mkdir -p dist

MODULES="strikead_stdlib strikead_leveldb strikead_yaws strikead_csv strikead_eunit strikead_io"

for m in $MODULES
do
    MODOUT=build/$m
    mkdir -p $MODOUT

    cp -v -r $m/ebin $MODOUT
    [ -d $m/include ] && cp -v -r $m/include $MODOUT
done

tar -czf dist/strikead-erlang-commons.tar.gz -C build .
