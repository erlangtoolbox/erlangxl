#!/bin/sh
rebar clean
rebar eunit || exit -1
rebar compile || exit -1
mkdir -p out

MODULES="common leveldb yaws csv eunit io"

for m in $MODULES
do
    MODOUT=out/strikead_$m
    mkdir -p $MODOUT

    cp -v -r $m/ebin $MODOUT
    [ -d $m/include ] && cp -v -r $m/include $MODOUT
done
exit 0
