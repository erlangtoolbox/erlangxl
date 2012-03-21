#!/bin/sh
rm -rf build dist
MODULES="strikead_stdlib strikead_leveldb strikead_yaws strikead_csv strikead_eunit strikead_io strikead_json"

function cleanup() {
    for m in $MODULES
    do
        for b in `ls $m/*/*.bind`
        do
        rm -f $m/include/`basename $b .bind`.hrl
            rm -f $m/src/`basename $b .bind`.erl
        done
    done
}
cleanup

rebar clean
rebar compile || exit -1

for m in $MODULES
do
    for b in `ls $m/*/*.bind`
    do
	export ERL_LIBS=lib
        erl -pz strikead_json/ebin -pz strikead_stdlib/ebin -noshell -run strikead_json_binder compile $b $m -s init stop || exit $?
	unset  ERL_LIBS
    done
done

rebar eunit || exit -1

mkdir -p build
mkdir -p dist

cleanup

for m in $MODULES
do
    MODOUT=build/$m
    mkdir -p $MODOUT

    cp -v -r $m/ebin $MODOUT
    [ -d $m/include ] && cp -v -r $m/include $MODOUT
done

tar -czf dist/strikead-erlang-commons.tar.gz -C build .
