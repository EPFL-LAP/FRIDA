#!/bin/bash

source env.env

echo "Building Dynamatic"
cd submodules/dynamatic/

./build.sh --release -f -t 8

echo "Building VTR"
cd vtr-verilog-to-routing
mkdir build
cd build

export CXX=/usr/bin/clang++
export CC=/usr/bin/clang

cmake .. -GNinja -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=1
ninja
cd ../../

