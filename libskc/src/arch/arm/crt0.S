/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

.text

/* Cell entry point. */
.global _start
_start:
    /* sp already set correctly in the capdl spec */

    /* Call main. */
    bl      main

    /* Spin forever. This is fine as we assume the domain scheduler and that we
     * are the only thread in this domain.
     */
1:  b       1b

