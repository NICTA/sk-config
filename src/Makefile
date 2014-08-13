#
# Copyright 2014, NICTA
#
# This software may be distributed and modified according to the terms of
# the BSD 2-Clause license. Note that NO WARRANTY is provided.
# See "LICENSE_BSD2.txt" for details.
#
# @TAG(NICTA_BSD)
#

SHELL=/bin/bash

ifeq (${V},1)
Q:=
else
Q:=@
MAKEFLAGS += --no-print-directory
endif

.PHONY: default
default: sk-capdl

sk-capdl: Main.hs Objects.hs OutputC.hs XMLWrapper.hs ELFFile.hs Utils.hs OutputCapDL.hs CapDLObjects.hs OutputConfig.hs
	@echo " [GHC]  $@"
	${Q}which ghc &>/dev/null || { echo "GHC not found" >&2 ; exit 1 ; }
	${Q}./tools/check-lib.sh Text.XML.Light || \
        { echo "Required Haskell library xml not found" >&2 ; exit 1 ; }
	${Q}./tools/check-lib.sh Data.Elf || \
        { echo "Required Haskell library elf not found" >&2 ; exit 1 ; }
	${Q}./tools/check-lib.sh "Data.String.Utils (join, replace)" || \
        { echo "Required Haskell library MissingH not found" >&2 ; exit 1 ; }
	${Q}ghc --make $< -o $@

.PHONY: clean
clean:
	@echo " [CLEAN]"
	${Q}rm -f sk-capdl
	${Q}rm -f *.hi
	${Q}rm -f *.o
