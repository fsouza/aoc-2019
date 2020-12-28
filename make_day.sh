#!/usr/bin/env bash

set -euo pipefail

if [ -z "${1}" ]; then
	echo "please specify the day" >&2
	exit 2
fi

if ! grep -q '^\d\d$' <<<"${1}"; then
	echo "invalid format for the day" >&2
	exit 2
fi

day=${1}
folder=day${day}

if [ -d "${folder}" ]; then
	echo "folder ${folder} already exists" >&2
	exit 1
fi

cp -rv template "${folder}"
pushd "${folder}"
mv day_nn.ml "day_${day}.ml"
mv day_nn.mli "day_${day}.mli"
find . -name dune -print0 | xargs -0 gsed -i 's/_nn/_'"${day}"'/'
dune build
popd
