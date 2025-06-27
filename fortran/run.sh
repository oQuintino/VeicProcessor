#!/bin/bash

cd ../data/raw || exit 1

if [ ! -f ../../fortran/build/emiss.exe ]; then
    echo "❌ Executable not found. Run 'make' first."
    exit 1
fi

# Runs emiss.exe from the build folder, but while being in data/raw (so the files are in the correct path)
../../fortran/build/emiss.exe

cd - > /dev/null
