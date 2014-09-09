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
    ldr     sp, =_sk_stack + 4096 * 2

    /* Call main. */
    bl      main

    /* Spin forever. This is fine as we assume the domain scheduler and that we
     * are the only thread in this domain.
     */
1:  b       1b

/*
 * Symbols required for libgcc.
 */
.global raise
.global __aeabi_unwind_cpp_pr0
.global __aeabi_unwind_cpp_pr1
.global __aeabi_unwind_cpp_pr2
raise:
__aeabi_unwind_cpp_pr0:
__aeabi_unwind_cpp_pr1:
__aeabi_unwind_cpp_pr2:
    b       raise
