#!/bin/bash

mkdir build

gfortran -w -g0 -O2 -mtune=native emiss.f90 -o build/emiss.exe 2> /dev/null

if [ $? -eq 1 ]; then
    echo "Falha ao tentar compilar o arquivo emiss.f90"
    exit 1
fi
