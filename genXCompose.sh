#!/usr/bin/env bash

DIR="$(cd -P "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")" )" && pwd)"

GENERATORNAME="genXCompose"

pushd "${DIR}"
  echo 'Rebuilding XCompose (the generator)...'
  ghc --make "${GENERATORNAME}.hs"
popd

echo Running the generator...
"${DIR}/${GENERATORNAME}" "$@"
ln -s -f -n "${DIR}/.XCompose" "${HOME}/.XCompose"
