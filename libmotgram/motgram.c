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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif // HAVE_CONFIG_H

#include <unistd.h>
#include <stdint.h>
#include <pthread.h>
#include <pthread.h>
#include <sys/stat.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>

#ifdef HAVE_ACH_H
#include <ach.h>
#endif // HAVE_ACH_H

#include "motgram.h"




#ifdef HAVE_ACH_H

ach_status_t
mg_supervisor_table_ach_get( mg_supervisor_table_t *table,
                             ach_channel_t *channel,
                             const struct timespec *ACH_RESTRICT abstime,
                             int ach_options ) {
    size_t frame_size = 0;
    ach_status_t r = ach_get( channel, table->data, table->n_data,
                              &frame_size, abstime, ach_options );
    while( ACH_OVERFLOW == r ) {
        assert( frame_size > table->n_data );
        // realloc data buffer
        if( table->data ) {
            free(table->data);
        }
        table->n_data = frame_size;
        table->data = (struct mg_supervisor_data*)malloc( table->n_data );
        // Retry the get
        r = ach_get( channel, table->data, table->n_data,
                     &frame_size, abstime, ach_options );
    }

    return r;
}
#endif // HAVE_ACH_H

int
mg_supervisor_table_fread( mg_supervisor_table_t *table, FILE *fin ) {
    struct mg_supervisor_data tmp_data;
    // read the header
    size_t r = fread( &tmp_data, 1, 32, fin );
    if( r != 32 ) return -1;

    // realloc if necessary
    size_t bytes = tmp_data.bits / 8;
    size_t needed = 32 + tmp_data.n_terminals * tmp_data.n_states * bytes;
    if(  needed > table->n_data ) {
        if( table->data ) free(table->data);
        table->n_data = needed;
        table->data = (struct mg_supervisor_data*)malloc( needed );
    }
    memcpy( table->data, &tmp_data, 32);

    r = fread( table->data->table, 1, needed - 32, fin );
    if( r != needed - 32 )
        return -2;
    else
        return 0;
}

int
mg_supervisor_table_print_head( mg_supervisor_table_t *table, FILE *fout ) {
    fprintf(fout,
            "states:   \t%"PRIu64"\n"
            "terminals:\t%"PRIu64"\n"
            "bits:     \t%"PRIu8"\n",
            table->data->n_states,
            table->data->n_terminals,
            table->data->bits );
    return 0;
}

int

mg_supervisor_table_print( mg_supervisor_table_t *table, FILE *fout ) {
    size_t state = table->state;
    mg_supervisor_table_print_head(table,fout);
    for( size_t i = 0; i < table->data->n_states; i ++ ) {
        table->state = i;
        for( size_t j = 0; j < table->data->n_terminals; j ++ ) {
            fprintf(fout,"%ld\t", mg_supervisor_next_state( table, j ) );
        }
        fprintf(fout,"\n");
    }
    table->state = state;
    return 0;
}
