/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

/*
 * Example of generated file.
 * Author: Matthew Fernandez
 *
 * This represents my interpretation of Galois' spec according to their
 * programming guide.
 */
#ifndef _CELL1_DRIVER_H_
#define _CELL1_DRIVER_H_

#include <stddef.h>
#include <sys/types.h>

size_t region_input_size(void);
int region_input_read(void *dest, size_t size, off_t offset);
int region_input_write(void *src, size_t size, off_t offset);
void *region_input_base(void);

size_t region_output_size(void);
int region_output_read(void *dest, size_t size, off_t offset);
int region_output_write(void *src, size_t size, off_t offset);
void *region_output_base(void);

#endif /* !_CELL1_DRIVER_H_ */
