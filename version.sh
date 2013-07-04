#!/bin/sh
VSN=`git describe --tags --abbrev=0`
echo -n `printf "$VSN.%d" $BUILD_NUMBER`
