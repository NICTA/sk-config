/* 
Copyright (c)2013, Galois, Inc.

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Jonathan Daugherty nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

This material is based upon work supported by the Government under
Contract No. W15P7T-11-C-H209.
*/

#ifndef _LIBSKC_CHANNELS_H_
#define _LIBSKC_CHANNELS_H_

struct mem {
    void *base;
    size_t size;
};

/*
 * Channel types.
 */
#define CHANNEL_TYPE_SEND                 1
#define CHANNEL_TYPE_RECV                 2

/*
 * Channel operation status codes.
 */
#define CHANNEL_OK                        0
#define CHANNEL_INVALID                   -1
#define CHANNEL_EMPTY                     -2
#define CHANNEL_FULL                      -3
#define CHANNEL_INVALID_SLOT              -4
#define CHANNEL_INVALID_TYPE              -5
#define CHANNEL_INVALID_STATUS            -6
#define CHANNEL_NO_MATCHING_SLOT          -7

/*
 * Slot status values.
 */
#define SLOT_STATUS_FREE                  0
#define SLOT_STATUS_USED                  1

/*
 * Overwrite constants passed to write_channel.
 */
#define CHAN_NO_OVERWRITES                0
#define CHAN_ALLOW_OVERWRITES             1

struct channel_base {
    struct mem slots;
    struct mem reader_metadata;
    struct mem writer_metadata;

    size_t message_size;
    size_t num_slots;
};

struct read_channel {
    struct channel_base ch;
    int *last_read;
    int *num_read;
    int *last_written;
    int *num_written;
};

struct write_channel {
    struct channel_base ch;
    int *last_read;
    int *last_written;
    int *num_written;
    int *num_read;
    int allow_overwrites;
};

typedef struct read_channel* read_channel_p;
typedef struct write_channel* write_channel_p;

write_channel_p write_channel(write_channel_p chan, struct mem messages, struct mem reader_meta,
                  struct mem writer_meta, size_t msg_size,
                  size_t num_slots, int allow_overwrites);

read_channel_p read_channel(read_channel_p chan, struct mem messages, struct mem reader_meta,
                struct mem writer_meta, size_t msg_size,
                size_t num_slots);

// Send a message.
int channel_send(write_channel_p chan, void *buf);

// Receive a message.
int channel_recv(read_channel_p chan, void *buf);

// Return 1 if the channel has room to send.
int channel_can_send(write_channel_p chan);

// Return 1 if the channel has messages ready to be received.
int channel_can_recv(read_channel_p chan);

int channel_try_recv(read_channel_p chan, void *buf);
int channel_try_send(write_channel_p chan, void *buf);

#endif
