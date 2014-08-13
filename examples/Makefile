#
# Copyright 2014, NICTA
#
# This software may be distributed and modified according to the terms of
# the BSD 2-Clause license. Note that NO WARRANTY is provided.
# See "LICENSE_BSD2.txt" for details.
#
# @TAG(NICTA_BSD)
#

CC?=gcc

.PHONY: validate
validate: cell1 cell2
	./cell1
	./cell2

cell1: cell1_driver.c cell1_driver.h cell1.c test.h
	${CC} -Wall -Werror cell1.c cell1_driver.c -o cell1

cell2: cell2_driver.c cell2_driver.h cell2.c test.h
	${CC} -Wall -Werror cell2.c cell2_driver.c -o cell2

clean:
	rm -f cell1 cell2
