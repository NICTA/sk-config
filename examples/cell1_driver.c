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
#include <string.h>
#include "cell1_driver.h"

#ifdef PAGE_SIZE
    #undef PAGE_SIZE
#endif
#define PAGE_SIZE 4096 /* bytes */

#ifndef ROUND_UP
    #define ROUND_UP(x, n) (((x) + (n) - 1) / (n) * (n))
#endif

/* Align to a page boundary and round up size to determine the region is backed
 * by a set of pages that contain nothing else. This allows us to control
 * access to the region through virtual memory rights. The symbol needs to be
 * visible in the final ELF file for a loader to initialise the shared frames
 * correctly.
 */
char input[ROUND_UP(1024, PAGE_SIZE)]
    __attribute__((aligned(PAGE_SIZE)))
    __attribute__((externally_visible));

size_t region_input_size(void) {
    return 1024;
}

int region_input_read(void *dest, size_t size, off_t offset) {
#ifndef NO_BOUNDS_CHECKS
    if (offset + size > region_input_size()) {
        return -1;
    }
#endif
    memcpy(dest, input + offset, size);
    return 0;
}

int region_input_write(void *src, size_t size, off_t offset) {
#ifndef NO_BOUNDS_CHECKS
    if (offset + size > region_input_size()) {
        return -1;
    }
#endif
    /* Read only region. */
    return -1;
}

void *region_input_base(void) {
    return (void*)input;
}

/* Align to a page boundary and round up size to determine the region is backed
 * by a set of pages that contain nothing else. This allows us to control
 * access to the region through virtual memory rights. The symbol needs to be
 * visible in the final ELF file for a loader to initialise the shared frames
 * correctly.
 */
char output[ROUND_UP(1024, PAGE_SIZE)]
    __attribute__((aligned(PAGE_SIZE)))
    __attribute__((externally_visible));

size_t region_output_size(void) {
    return 1024;
}

int region_output_read(void *dest, size_t size, off_t offset) {
#ifndef NO_BOUNDS_CHECKS
    if (offset + size > region_output_size()) {
        return -1;
    }
#endif
    /* Write-only region. */
    return -1;
}

int region_output_write(void *src, size_t size, off_t offset) {
#ifndef NO_BOUNDS_CHECKS
    if (offset + size > region_output_size()) {
        return -1;
    }
#endif
    memcpy(output + offset, src, size);
    return 0;
}

void *region_output_base(void) {
    return (void*)output;
}
