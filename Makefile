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
default: src/sk-capdl docs/sk-capdl-manual.pdf sk-capdl-release.tgz

src/sk-capdl: src/Main.hs src/Objects.hs src/OutputC.hs \
        src/XMLWrapper.hs src/ELFFile.hs src/Utils.hs src/OutputCapDL.hs \
        src/CapDLObjects.hs src/Makefile src/tools/check-lib.sh
	@echo " [MAKE] $@"
	${Q}cd src && make $(notdir $@) V=${V} | \
        while read l; do echo "  $${l}"; done; exit $${PIPESTATUS[0]}

docs/sk-capdl-manual.pdf: docs/report.tex docs/Makefile \
        examples/sk-schema.xsd examples/sk-example.xml
	@echo " [MAKE] $@"
	${Q}cd docs && make $(notdir $@) V=${V} | \
        while read l; do echo "  $${l}"; done; exit $${PIPESTATUS[0]}

sk-capdl-release.tgz: src/Main.hs src/Objects.hs src/OutputC.hs \
        src/XMLWrapper.hs src/ELFFile.hs src/Utils.hs src/OutputCapDL.hs \
        src/CapDLObjects.hs src/Makefile src/tools/check-lib.sh \
        docs/sk-capdl-manual.pdf
	@echo " [TAR]  $@"
	${Q}rm -rf stage
	${Q}for i in $^; do mkdir -p stage/`dirname $$i` && cp $$i stage/$$i; done
	${Q}cd stage && tar caf ../$@ *

.PHONY: clean
clean:
	@echo " [CLEAN]"
	${Q}cd src && make clean V=${V} | \
        while read l; do echo "  $${l}"; done; exit $${PIPESTATUS[0]}
	${Q}cd docs && make clean V=${V} | \
        while read l; do echo "  $${l}"; done; exit $${PIPESTATUS[0]}
	${Q}rm -f sk-capdl-release.tgz
	${Q}rm -rf stage
