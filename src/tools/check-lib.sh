#!/bin/bash

#
# Copyright 2014, NICTA
#
# This software may be distributed and modified according to the terms of
# the BSD 2-Clause license. Note that NO WARRANTY is provided.
# See "LICENSE_BSD2.txt" for details.
#
# @TAG(NICTA_BSD)
#

# Check whether a given Haskell library is installed.

if [ $# -ne 1 ]; then
    echo "Usage: $0 library" >&2
    exit 1
fi

SCRATCH=`mktemp -d`
pushd "${SCRATCH}" &>/dev/null
cat >Temp.hs <<EOT
import $1

main = return ()
EOT
ghc --make Temp.hs &>/dev/null
RET=$?
popd &>/dev/null
rm -rf "${SCRATCH}"
exit ${RET}
