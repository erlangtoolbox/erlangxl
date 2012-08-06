PREFIX=1.0.0
if [ -z $BUILD_NUMBER ]
then
	echo -n $PREFIX
else
	echo -n $PREFIX.$BUILD_NUMBER
fi
