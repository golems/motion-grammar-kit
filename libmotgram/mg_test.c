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

#ifdef HAVE_ACH_H
#include <ach.h>
#endif // HAVE_ACH_H

#include <stdio.h>
#include <assert.h>
#include <inttypes.h>
#include "motgram.h"



int main(int argc, char **argv) {
    printf("Hello World!\n");
    ach_channel_t chan;
    ach_status_t r = ach_open(&chan, "super", NULL );
    assert( ACH_OK == r );

    mg_supervisor_table_t table = {
        .state = 0,
        .n_data = 0,
        .data = NULL
    };

    r = mg_supervisor_table_ach_get( &table, &chan, NULL, ACH_O_LAST );
    if( !( ACH_OK == r || ACH_MISSED_FRAME == r ) ) {
        fprintf(stderr, "Error getting data: %s\n",
                ach_result_to_string( r ) );
    }
    uint64_t n_states = table.data->n_states;
    uint64_t n_terminals = table.data->n_terminals;
    int32_t *ptr = (int32_t*)table.data->table;

    printf( "states:\t%"PRIu64"\n"
            "terminals:\t%"PRIu64"\n"
            "bits:\t%"PRIu8"\n",
            n_states,
            n_terminals,
            table.data->bits );
    for( size_t i = 0; i < n_states; i ++ ) {
        for( size_t j = 0; j < n_terminals; j ++ ) {
            printf("%d\t", ptr[i*n_terminals+j]);
        }
        printf("\n");
    }
}
