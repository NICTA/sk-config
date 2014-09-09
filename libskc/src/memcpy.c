/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#include <string.h>

void *memcpy(void *destination, const void *source, size_t num) {
    char *d = (char*)destination;
    const char *s = (const char*)source;
    for (; num > 0; num--) {
        *d++ = *s++;
    }
    return destination;
}
