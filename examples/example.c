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


#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <motgram.h>

/***************/
/* DEFINITIONS */
/***************/

/* Context Struct for Load/Unload Example */
typedef struct {
    int loaded;
} context_load_t;

/* Terminal Symbol (Token) Definitions */

static int load(context_load_t *cx) {
    printf("\"LOAD\"\n");
    cx->loaded++;
    return 0;
}

static int done(context_load_t *cx) {
    /* An extremely boring test */
    if( cx->loaded >= 10 ) {
        printf("\"DONE\"\n");
        return 0;
    } else {
        return -1;
    }
}

static int not_done(context_load_t *cx) {
    /* An extremely boring test */
    if( cx->loaded < 10 ) {
        printf("\"NOT-DONE\"\n");
        return 0;
    } else {
        return -1;
    }
}

static int unload(context_load_t *cx) {
    if( cx->loaded > 0 ) {
        printf("\"UNLOAD\"\n");
        cx->loaded--;
        return 0;
    } else {
        return -1;
    }
}

static int load_a(context_load_t *cx) {
    printf("\"LOAD-A\"\n");
    cx->loaded++;
    return 0;
}

static int load_b(context_load_t *cx) {
    printf("\"LOAD-B\"\n");
    cx->loaded++;
    return 0;
}

/*********************/
/* GENERATED PARSERS */
/*********************/
#include "load_parse.c"
#include "super_load_parse.c"

/*****************/
/* MAIN FUNCTION */
/*****************/
int main() {

    /*---- basic parser ----*/
    printf("BASIC\n"
           "-----\n");
    context_load_t cx_load = { .loaded = 0 };
    int i = load_parse( &cx_load, 0 );
    assert(0 == i );

    printf("\n\n");

    /*---- supervised parser ----*/
    printf("SUPERVISED\n"
           "----------\n");
    context_load_t cx_super = { .loaded = 0 };
    // A struct to hold the supervisor
    mg_supervisor_table_t supervisor = {.state = 0,
                                        .n_data = 0,
                                        .data = NULL };
    // read in the supervisor file
    FILE *fin = fopen("super.dat", "r");
    assert( fin );
    i = mg_supervisor_table_fread( &supervisor, fin );
    assert( 0 == i );
    // print the supervisor
    mg_supervisor_table_print( &supervisor, stdout );
    // now parse
    i = super_load_parse( &cx_super, &supervisor, 0 );
    assert(0 == i );

    return 0;

}
