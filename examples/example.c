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
    mg_supervisor_table_t *supervisor;
} context_load_t;

static int load(context_load_t *cx) {
    //printf("test-load: %d\n", cx->loaded);
    printf("\"LOAD\"\n");
    cx->loaded++;
    return 0;
}

static int done(context_load_t *cx) {
    //printf("test-done\n");
    /* An extremely boring test */
    if( cx->loaded >= 10 ) {
        printf("\"DONE\"\n");
        return 0;
    } else {
        return -1;
    }
}

static int not_done(context_load_t *cx) {
    //printf("test-done: state %d\n",
           //cx->supervisor ? cx->supervisor->state : -1 );
    /* An extremely boring test */
    if( cx->loaded < 10 ) {
        printf("\"NOT-DONE\"\n");
        return 0;
    } else {
        return -1;
    }
}

static int unload(context_load_t *cx) {
    //printf("test-unload\n");
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
    context_load_t cx_load = { .loaded = 0, .supervisor = NULL };

    // basic parser
    int i = load_parse( &cx_load, 0 );
    if( i )
        return i;

    printf("\n\nNext\n\n");

    // load supervisor table
    context_load_t cx_super = { .loaded = 0 };
    mg_supervisor_table_t supervisor = {.state = 0,
                                        .n_data = 0,
                                        .data = NULL };
    cx_super.supervisor = &supervisor;
    FILE *fin = fopen("super.dat", "r");
    assert( fin );
    i = mg_supervisor_table_fread( &supervisor, fin );
    assert( 0 == i );
    mg_supervisor_table_print( &supervisor, stdout );
    return super_load_parse( &cx_super, &supervisor, 0 );

}
