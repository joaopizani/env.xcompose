#!/usr/bin/env bash

DIR="$(cd -P "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")" )" && pwd)"


sudo apt-get update
sudo apt-get install uim
im-config -n uim


"${DIR}/install-basic.sh"

