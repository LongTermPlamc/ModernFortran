#!/bin/bash

OBJECTIVE=$1

echo "Currently building $OBJECTIVE.exe"

#FC="caf"
FC="gfortran"

SRC_DIR="app"
BUILD_DIR="build"
MODERN_PATH="../../modernLib/build"

echo "Cleaning previous build..."
rm -rf "$BUILD_DIR"
rm -f $OBJECTIVE.exe
mkdir -p "$BUILD_DIR"

$FC "$SRC_DIR"/"$OBJECTIVE".f90 -I"$MODERN_PATH" -L"$MODERN_PATH" -lmodern -o "$OBJECTIVE".exe
#caf app/main.f90 -I../../modernLib/build -L../../modernLib/build -lmodern -o main.exe

echo "Compilation finished!"