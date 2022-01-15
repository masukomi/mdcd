#!/bin/sh

# required libraries

# args
# filepath
# http-client
# intarweb
# linenoise
# medea
# message-digest
# sha2
# shell
# simple-exceptions
# simple-loops
# spiffy
# spiffy-request-vars
# uri-common


VERSION="dev_version"
if [ "$1" != "" ]; then
  VERSION=$1
  perl -pi -e "s/VERSION_NUMBER_HERE/$1/" cerbo-server.scm
fi

echo "Removing previous builds"
rm -f *.link
rm -f *.import.scm
rm -f *.o

echo "Building libraries"
# note: the ordering here is intentional
# to guarantee that dependencies are built before
# they are used
libs=(
  listicles \
  masufiles \
  mdcd-sections \
  mdcd-config \
  mdcd
)
for lib in ${libs[@]}; do
	echo "building $lib"
	csc -static -unit $lib -cJ $lib".scm"
done


echo "-----------------------------------"
echo "Building mcdc executable..."
COMMAND="csc "
for lib in ${libs[@]}; do
	COMMAND="$COMMAND"$' \\\n -link '$lib
done
echo "Command to be run: "
COMMAND="$COMMAND"$'\\\n -link pathname-expand \\\n -static mdcd.scm'
echo "$COMMAND"
# echo "[DISABLED]"
eval "$COMMAND"


# echo "creating compressed release file..."
#
# version_dir="cerbo_server_$VERSION"
# rm -rf $version_dir
# mkdir $version_dir
# cp cerbo_server $version_dir/
# cp pc $version_dir/
# tar -czf $version_dir.tgz $version_dir
# rm -rf $version_dir
#
# echo "here's your SHA for homebrew"
# shasum -a 256 $version_dir.tgz
#
# function print_dlibs () {
#   binary=$1
#   echo "DLIBS used by $binary:"
#
#   DYLIBS=`otool -L $binary | grep "/opt" | awk -F' ' '{ print $1 }'`
#   if [ "$DYLIBS" == "" ]; then
#     echo "  None!"
#   else
#     for dylib in $DYLIBS
#     do
#       echo " - dylib $dylib"
#     done
#   fi
#
# }
#
# print_dlibs "cerbo_server"

if [ "$1" != "" ]; then
  perl -pi -e "s/$1/VERSION_NUMBER_HERE/" cerbo-server.scm
fi


echo "===================="
echo "Done."

