/* -*- mode: C; c-basic-offset: 4 -*- */
/* ex: set shiftwidth=4 tabstop=4 expandtab: */
/*
 * Copyright (c) 2012, Georgia Tech Research Corporation
 * All rights reserved.
 *
 * Author(s): Neil T. Dantam <ntd@gatech.edu>
 * Georgia Tech Humanoid Robotics Lab
 * Under Direction of Prof. Mike Stilman <mstilman@cc.gatech.edu>
 *
 *
 * This file is provided under the following "BSD-style" License:
 *
 *
 *   Redistribution and use in source and binary forms, with or
 *   without modification, are permitted provided that the following
 *   conditions are met:
 *
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 *   * Redistributions in binary form must reproduce the above
 *     copyright notice, this list of conditions and the following
 *     disclaimer in the documentation and/or other materials provided
 *     with the distribution.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
 *   CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 *   INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 *   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 *   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
 *   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 *   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 *   USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 *   AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *   LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 *   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *   POSSIBILITY OF SUCH DAMAGE.
 *
 */
#ifndef MOTGRAM_H
#define MOTGRAM_H

/** \file motgram.h */

struct mg_supervisor_data {
    uint64_t n_states;         /** Number of states */
    uint64_t n_terminals;      /** Number of termnals */
    union {
        uint8_t bits;          /** Bit size of each data member */
        uint64_t reserved[2];  /** Align and reserve space */
    };
    uint8_t table[1];          /** Transition matrix */
};


struct mg_supervisor_table {
    size_t state;
    size_t n_data;             /**< max size of data array (bytes) */
    // use 32-bit state numbers for now
    struct mg_supervisor_data *data;
};



typedef struct mg_supervisor_table mg_supervisor_table_t;

int
mg_supervisor_table_print( mg_supervisor_table_t *table, FILE *fout );

/** Return 0 on success */
int
mg_supervisor_table_fread( mg_supervisor_table_t *table, FILE *fin );

static ssize_t mg_supervisor_next_state( mg_supervisor_table_t *table,
                                  size_t terminal ) {
    uint64_t n_z = table->data->n_terminals;
    switch( table->data->bits ) {
    case 32:
        return ( (int32_t*)(table->data->table) )[table->state*n_z + terminal];
        break;
    default:
        assert(0);
    }
}

static _Bool mg_supervisor_allow ( mg_supervisor_table_t *table,
                            size_t terminal ) {
    return mg_supervisor_next_state(table,terminal) >= 0;
}


/* Optional Ach Support */
#ifdef ACH_H

/** Read a new data segment from ach channel */
ach_status_t
mg_supervisor_table_ach_get( mg_supervisor_table_t *table,
                             ach_channel_t *channel,
                             const struct timespec *ACH_RESTRICT abstime,
                             int ach_options );
#endif /* ACH_H */

#endif /*MOTGRAM_H*/
