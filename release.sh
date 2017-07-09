#!/bin/sh

set -ex

LTS=lts-8.4
GHC=8.0.2
PROC=$(uname -p)
PLATFORM=$(dpkg --print-architecture)
VERSION=$(awk '$1 == "version:" {print $2}' lxdfile.cabal)

stack clean
stack build

mkdir -p dist/release/
cp .stack-work/install/${PROC}-linux-nopie/${LTS}/${GHC}/bin/lxdfile dist/release/lxdfile_${VERSION}_${PLATFORM}
