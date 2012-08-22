PREFIX=1.0.0
[ -z $BUILD_NUMBER ] && BUILD_NUMER=0

echo -n $PREFIX.`printf "%04d\n" $BUILD_NUMBER`
