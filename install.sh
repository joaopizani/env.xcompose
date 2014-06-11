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
