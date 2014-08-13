/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

/* Code to validate that cell2_driver.* is a valid translation of the XML
 * spec.
 */
#include <stdio.h>
#include "cell2_driver.h"
#include "test.h"

#define BUFFER_SIZE 1024
char buffer[BUFFER_SIZE];

int main(int argc, char **argv) {
    int result = 0;

    printf("output (seg1) is at %p, input (seg2) is at %p, misc (seg3)  is at %p\n",
        region_input_base(), region_output_base(), region_misc_base());

    result |= TEST("output is writeable",
        region_output_write((void*)buffer, BUFFER_SIZE, 0) == 0);

    result |= TEST("output is not readable",
        region_output_read((void*)buffer, BUFFER_SIZE, 0) != 0);

    result |= TEST("size of output",
        region_output_size() == 1024);

    result |= TEST("input is not writeable",
        region_input_write((void*)buffer, BUFFER_SIZE, 0) != 0);

    result |= TEST("input is readable",
        region_input_read((void*)buffer, BUFFER_SIZE, 0) == 0);

    result |= TEST("size of input",
        region_input_size() == 1024);

    result |= TEST("misc is writeable",
        region_misc_write((void*)buffer, BUFFER_SIZE, 0) == 0);

    result |= TEST("misc is readable",
        region_misc_read((void*)buffer, BUFFER_SIZE, 0) == 0);

    result |= TEST("size of misc",
        region_misc_size() == 4096);

    return result;
}
