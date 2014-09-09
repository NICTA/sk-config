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
    /* Assume we have a stack that looks like:
     *
     *  _sk_stack:
     *  .space 4096  <-- guard
     *  .space 4096  <-- stack
     *  .space 4096  <-- guard
     *
     */
    leal    _sk_stack + 4096 * 2, %esp

    /* Call main. */
    call    main

    /* Spin forever. This is fine as we assume the domain scheduler and that we
     * are the only thread in this domain.
     */
1:  jmp     1b
