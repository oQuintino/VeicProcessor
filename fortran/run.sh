#!/bin/bash

cd ../data/raw || exit 1

"Runs emiss.exe from the build folder, but while being in data/raw (so the files are in the correct path)"
../../fortran/build/emiss.exe

cd - > /dev/null
