#include <stdio.h>

/***************/
/* DEFINITIONS */
/***************/

/* Context Struct for Load/Unload Example */
typedef struct {
    int loaded;
    int done;
} context_load_t;


static int load(context_load_t *cx) {
    printf("test-load\n");
    /* An extremely boring test */
    if( cx->loaded < 10 ) {
        printf("\"LOAD\"\n");
        cx->loaded++;
        return 0;
    } else {
        cx->done = 1;
        return -1;
    }
}

static int done(context_load_t *cx) {
    printf("test-done\n");
    if( cx->done ) {
        printf("\"DONE\"\n");
        return 0;
    } else {
        return -1;
    }
}

static int unload(context_load_t *cx) {
    printf("test-unload\n");
    if( cx->loaded > 0 ) {
        printf("\"UNLOAD\"\n");
        cx->loaded--;
        return 0;
    } else {
        return -1;
    }
}

/********************/
/* GENERATED PARSER */
/********************/
#include "load_parse.c"


/*****************/
/* MAIN FUNCTION */
/*****************/

int main() {
    context_load_t cx_load = { .loaded = 0, .done = 0 };
    int i = load_parse( &cx_load, 0 );

    return i;
}
