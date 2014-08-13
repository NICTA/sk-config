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

DOC=report
FINAL=sk-capdl-manual

ifeq (${V},1)
Q:=
else
Q:=@
MAKEFLAGS += --no-print-directory
endif

.PHONY: default
default: ${FINAL}.pdf

${FINAL}.pdf: ${DOC}.tex references.bib sk-example.xml sk-schema.xsd
	@echo " [PDFLATEX] $@"
	${Q}pdflatex "$<" &>/dev/null
	@echo " [BIBTEX] $@"
	${Q}bibtex ${DOC}.aux &>/dev/null
	@echo " [PDFLATEX] $@"
	${Q}pdflatex "$<" &>/dev/null
	@echo " [PDFLATEX] $@"
	${Q}pdflatex "$<" &>/dev/null
	@echo " [PDFLATEX] $@"
	${Q}pdflatex "$<" &>/dev/null
	${Q}! grep --quiet 'LaTeX Warning: There were undefined references' ${DOC}.log
	${Q}mv -f ${DOC}.pdf $@

sk-schema.xsd: ../examples/sk-schema.xsd
	@echo " [GEN] $@"
	${Q}tail --lines=+3 $< >$@

sk-example.xml: ../examples/sk-example.xml
	@echo " [GEN] $@"
	${Q}tail --lines=+3 $< >$@

.PHONY: clean
clean:
	@echo " [CLEAN] sk-capdl-manual.pdf ${DOC}.pdf ${DOC}.bbl ${DOC}.aux ${DOC}.blg ${DOC}.brf ${DOC}.log ${DOC}.out ${DOC}.toc sk-example.xml sk-scheme.dtd"
	${Q}rm -rf ${DOC}.pdf sk-capdl-manual.pdf ${DOC}.bbl ${DOC}.aux \
        ${DOC}.blg ${DOC}.brf ${DOC}.log ${DOC}.out ${DOC}.toc sk-example.xml \
        sk-scheme.dtd
