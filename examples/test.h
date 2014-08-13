/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

/* Utility macro for testing. */

#ifndef _TEST_H_
#define _TEST_H_

#define TEST(desc, op) \
    ({ \
        int _result; \
        printf("Testing " desc "..."); \
        if (op) { \
            printf("Success\n"); \
            _result = 0; \
        } else { \
            printf("Failed\n"); \
            _result = -1; \
        } \
        _result; \
    })

#endif /* ! _TEST_H_ */
