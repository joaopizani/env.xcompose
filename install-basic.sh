#!/usr/bin/env bash

DIR="$(cd -P "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")" )" && pwd)"


GENERATORDIR="xcompose-generator"
MAINMODULE="XComposeGenerator"

XCOMPOSEFILEPATH="${DIR}/XCompose"


pushd "${DIR}/${GENERATORDIR}"
  echo "Rebuilding XCompose (the generator)..."
  ghc --make "${MAINMODULE}.hs"
popd

echo "Running the generator..."
"${DIR}/${GENERATORDIR}/${MAINMODULE}" "${XCOMPOSEFILEPATH}"
ln -s -f -n "${XCOMPOSEFILEPATH}" "${HOME}/.XCompose"

xrdb -merge "${HOME}/.XCompose"
echo "XComposed shortcuts generated and merged. Now set the Compose key in the OS."

ln -s -f -n "${DIR}/${GENERATORDIR}/XComposeGen/Unicode.hs" "${HOME}/Unicode.hs"

