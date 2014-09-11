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
    /* esp already set correctly in the capdl spec */

    /* pretend to push argv and argc onto the stack before calling main */
    sub     $0x10, %esp

    /* Call main. */
    call    main

    /* Spin forever. This is fine as we assume the domain scheduler and that we
     * are the only thread in this domain.
     */
1:  jmp     1b
