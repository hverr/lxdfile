#!/bin/sh

set -ex

LTS=lts-6.14
GHC=7.10.3
PROC=$(uname -p)
PLATFORM=$(dpkg --print-architecture)
VERSION=$(awk '$1 == "version:" {print $2}' lxdfile.cabal)

stack clean
stack build

mkdir -p dist/release/
cp .stack-work/install/${PROC}-linux/${LTS}/${GHC}/bin/lxdfile dist/release/lxdfile_${VERSION}_${PLATFORM}
