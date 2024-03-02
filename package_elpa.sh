#!/bin/bash

if [ "$#" != "1" ]; then
    echo "USAGE package_elpa.sh VERSION";
    echo "./package_elpa.sh v0.0.7";
    exit 1;
fi

version="$1"
echo "Packaging version ${version}..."

temp_dir=~/tmp/elpa-${version}
if [ -d ${temp_dir} ]; then
    echo "Directory ${temp_dir} already exists."
    exit 1;
fi

echo "Creating temp dir ${temp_dir}"
mkdir ${temp_dir}
if [ "$?" != "0" ]; then
    echo "Failed to create temp dir.";
    exit 1;
fi

echo "Copying files to temp directory ${temp_dir}..."
cp -r ./elpa/. ${temp_dir}/
if [ "$?" != "0" ]; then
    echo "Failed to copy files.";
    exit 1;
fi

echo "Deleting ELC files..."
find ${temp_dir} -iname '*.elc' -exec rm {} \;
if [ "$?" != "0" ]; then
    echo "Failed to delete ELC files.";
    exit 1;
fi

echo "Generating tarball..."
tar -zcf elpa-${version}.tgz -C ${temp_dir} .
if [ "$?" != "0" ]; then
    echo "Failed to generate tarball.";
    exit 1;
fi

echo "Deleting temp directory ${temp_dir}..."
rm -rf ${temp_dir}
if [ "$?" != "0" ]; then
    echo "Failed to delete temp directory.";
    exit 1;
fi

echo "Done"
