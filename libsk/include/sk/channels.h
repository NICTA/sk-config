/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef _SK_CHANNELS_H_
#define _SK_CHANNELS_H_

#include <stddef.h>

typedef struct channel *channel_p;

typedef enum {
    CHANNEL_TYPE_SEND = 0,
    CHANNEL_TYPE_RECV = 1,
} channel_type_t;

enum {
    CHANNEL_OK = 0,
    CHANNEL_INVALID = -1,
    CHANNEL_WRONG_TYPE = -2,
    CHANNEL_FULL = -3,
    CHANNEL_EMPTY = -4,
};

channel_p channel_new(void *base, size_t region_size, size_t msg_size,
    unsigned msg_count, channel_type_t type);

int channel_send(channel_p chan, void *msg);
int channel_recv(channel_p chan, void *msg);

int channel_try_send(channel_p chan, void *msg);
int channel_try_recv(channel_p chan, void *msg);

#endif
