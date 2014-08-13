/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#include <sk/channels.h>
#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/* We deviate from the libsk notes here by using 4 bytes instead of 1 as
 * per-message book-keeping. We need to do this because we have no 1 byte CAS
 * on the KZM.
 */
typedef uintptr_t lock_t;
enum {
    FREE = 0,
    USED = 1,
    INFLUX = 2,
};

static int cas(lock_t *ptr, lock_t old, lock_t new) {
    return __sync_bool_compare_and_swap(ptr, old, new);
}

typedef struct {
    lock_t occupied;
    char data[1]; /* We abuse this member to memcpy data. */
} slot_t;
typedef char slot_t_packed[__builtin_offsetof(slot_t, data)
    == sizeof(lock_t) ? 1 : -1];

struct channel {
    void *base;
    size_t msg_size;
    unsigned msg_count;
    channel_type_t type;
};

static slot_t *get_slot(channel_p chan, unsigned int index) {
    assert(chan != NULL);
    return (slot_t*)(chan->base + (sizeof(lock_t) + chan->msg_size) * index);
}

channel_p channel_new(void *base, size_t region_size, size_t msg_size,
        unsigned msg_count, channel_type_t type) {

    if (msg_count == 0) {
        /* Channel has zero capacity. */
        return NULL;
    }

    if (region_size < (msg_size + sizeof(lock_t)) * msg_count) {
        /* Region is not enough to accomodate channel capacity. */
        /* FIXME: The libsk docs omit any discussion of aligned accesses, which
         * they get away with because they use a byte for book-keeping. We need
         * to ensure accesses to the book-keeping (lock_t) are word-aligned and
         * hence need to factor in an extra fudge factor here.
         */
        return NULL;
    }

    if (type != CHANNEL_TYPE_SEND && type != CHANNEL_TYPE_RECV) {
        /* Channel type invalid. */
        return NULL;
    }

    channel_p chan = (channel_p)malloc(sizeof(struct channel));
    if (chan == NULL) {
        return NULL;
    }
    memset(chan, 0, sizeof(*chan));
    chan->base = base;
    chan->msg_size = msg_size;
    chan->msg_count = msg_count;
    chan->type = type;
    return chan;
}

/* FIXME: The libsk docs don't describe a destructor, but we leak memory if we
 * don't provide one.
 */

static int try_send(channel_p chan, void *msg) {
    assert(chan != NULL);
    assert(chan->type == CHANNEL_TYPE_SEND);

    for (int i = 0; i < chan->msg_count; i++) {
        slot_t *s = get_slot(chan, i);
        if (cas(&s->occupied, FREE, INFLUX)) {
            memcpy(&s->data, msg, chan->msg_size);
            int result = cas(&s->occupied, INFLUX, USED);
            assert(result == 1);
            return 0;
        }
    }
    return -1;
}

static int try_recv(channel_p chan, void *msg) {
    assert(chan != NULL);
    assert(chan->type == CHANNEL_TYPE_RECV);

    for (int i = 0; i < chan->msg_count; i++) {
        slot_t *s = get_slot(chan, i);
        if (cas(&s->occupied, USED, INFLUX)) {
            memcpy(msg, &s->data, chan->msg_size);
            int result = cas(&s->occupied, INFLUX, FREE);
            assert(result == 1);
            return 0;
        }
    }
    return -1;
}

int channel_send(channel_p chan, void *msg) {
    if (chan == NULL) {
        return CHANNEL_INVALID;
    }

    if (chan->type != CHANNEL_TYPE_SEND) {
        return CHANNEL_WRONG_TYPE;
    }

    while (try_send(chan, msg) != 0);
    return CHANNEL_OK;
}

int channel_recv(channel_p chan, void *msg) {
    if (chan == NULL) {
        return CHANNEL_INVALID;
    }

    if (chan->type != CHANNEL_TYPE_RECV) {
        return CHANNEL_WRONG_TYPE;
    }

    while (try_recv(chan, msg) != 0);
    return CHANNEL_OK;
}

int channel_try_send(channel_p chan, void *msg) {
    if (chan == NULL) {
        return CHANNEL_INVALID;
    }

    if (chan->type != CHANNEL_TYPE_SEND) {
        return CHANNEL_WRONG_TYPE;
    }

    if (try_send(chan, msg) != 0) {
        return CHANNEL_FULL;
    }

    return CHANNEL_OK;
}

int channel_try_recv(channel_p chan, void *msg) {
    if (chan == NULL) {
        return CHANNEL_INVALID;
    }

    if (chan->type != CHANNEL_TYPE_RECV) {
        return CHANNEL_WRONG_TYPE;
    }

    if (try_recv(chan, msg) != 0) {
        return CHANNEL_EMPTY;
    }

    return CHANNEL_OK;
}
