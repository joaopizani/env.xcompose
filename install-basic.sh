#!/usr/bin/env bash

DIR="$(cd -P "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")" )" && pwd)"


GENERATORDIR="xcompose-generator"
MAINMODULE="XComposeGenerator"

XCOMPOSEFILEPATH="${DIR}/XCompose"

HOMEUNICODE_DIR_NAME="Unicode"
HOMEUNICODE_DIR="${HOME}/${HOMEUNICODE_DIR_NAME}"

HSUNICODE_MODULES_NAMES=('Letterlike' 'Symbols')
HSUNICODE_DIR_NAME="xcompose-generator/XComposeGen"
HSUNICODE_DIR="${DIR}/${HSUNICODE_DIR_NAME}"


pushd "${DIR}/${GENERATORDIR}"
  echo "Rebuilding XCompose (the generator)..."
  ghc --make "${MAINMODULE}.hs"
popd

echo "Running the generator..."
"${DIR}/${GENERATORDIR}/${MAINMODULE}" "${XCOMPOSEFILEPATH}"
ln -s -f -n "${XCOMPOSEFILEPATH}" "${HOME}/.XCompose"

xrdb -merge "${HOME}/.XCompose"
echo "XComposed shortcuts generated and merged. Now set the Compose key in the OS."

mkdir -p "${HOMEUNICODE_DIR}"
for m in "${HSUNICODE_MODULES_NAMES[@]}"; do
    ln -s -f -n "${HSUNICODE_DIR}/${m}.hs" "${HOMEUNICODE_DIR}/${m}.hs"
done

