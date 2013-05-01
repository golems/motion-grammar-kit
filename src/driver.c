#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

// Motion parser written in C for LL(*)

typedef int bool_t;
typedef bool_t (*predicate_t)(observation_t);
typedef int terminal_t;
typedef int symbol_t; // terminal_t or nonterminal_t
typedef void (*mutator_t) ();
typedef void* untyped_t;

typedef enum {
  NONTERMINAL, // Simply a nonterminal
  PREDICATE,   // A predicate, should been defined before code generation (in the lisp program)
  MUTATOR,     // A side effect, you're expected to write one c function for each type of mutator
  KLEENE       // Like predicate, but "stay where you are" if true
} symbol_type_t ;

#define MAX_PRODUCTION_LENGTH 50
#define MAX_PRODUCTIONS 50
#define STORE_SIZE 2000
#define MAX_NODES 100
#define MAX_EDGES 500

typedef struct dfa dfa_t;
typedef struct production production_t;
typedef struct nonterminal nonterminal_t;

typedef struct production {
  int num_symbols;
  untyped_t data[MAX_PRODUCTION_LENGTH]; // Either (nonterminal_t, predicate_t, mutator_t)
  symbol_type_t types[MAX_PRODUCTION_LENGTH];
} production_t;

typedef struct nonterminal {
  //int num_productions;
  //production_t *productions[MAX_PRODUCTIONS]; // We don't need this probably
  dfa_t *automata;
} nonterminal_t;

observation_t store[STORE_SIZE];

int curr_beg; // From where we know our observations;
int curr_end; // Position of the end of store

bool_t queue_is_empty () {
  return curr_beg == curr_end;
}

extern observation_t make_observation();

observation_t fetch_observation() {
  assert(curr_beg != (curr_end+1)%STORE_SIZE && "Ran out of memory, queue size is too small");
  observation_t ret = store[curr_end] = make_observation();
  curr_end = (curr_end+1)%STORE_SIZE;
  return ret;
}

observation_t get_observation(int pos, bool_t *is_fetched_observation) {
  if (pos == curr_end) {
    if (is_fetched_observation != NULL) *is_fetched_observation = 1;
    return fetch_observation();
  } else {
    // assuming it's in range here, namely "pos < curr_end" (but in modulo and such)
    if (is_fetched_observation != NULL) *is_fetched_observation = 0;
    return store[pos];
  }
}

/// handle_mutator: Perform the action if it's the first time we observe it
void handle_mutator(mutator_t mutator, bool_t is_fetched_observation) {
  if(is_fetched_observation) {
    mutator();
  }
}

typedef struct dfa {
  int num_nodes;
  int start_node;
  production_t *final[MAX_NODES]; // If null then not a accept state. Otherwise predict to given production
  int range[MAX_NODES+1]; // the edges for node n are in the span [range[n], range[n+1])
  untyped_t edges[MAX_EDGES]; // predicate ==> if it turns true then take this edge. mutator ==> perform if new and always take edge
  symbol_type_t types[MAX_EDGES]; // The type of the edge. (Should only be mutator or predicate)
  int destinations[MAX_EDGES]; // by following edge j you end up at destinations[j]
} dfa_t;

production_t *run_dfa(dfa_t *automata) {
  int pred_pos = curr_beg;
  int curr_node = automata->start_node;
  while (1) {
    if(automata->final[curr_node]) {
      return automata->final[curr_node];
    }
    bool_t is_fetched_observation;
    observation_t obs = get_observation(pred_pos, &is_fetched_observation);
    int beg = automata->range[curr_node], end = automata->range[curr_node+1];
    int happened = 0; // Just for sanity check
    for(int i = beg; i < end; i++) {
      switch( automata->types[i] ) {
        case MUTATOR: {
          mutator_t mutator = (mutator_t) automata->edges[i];
          handle_mutator(mutator, is_fetched_observation);
          assert((beg+1 == end) && "We only expect one edge if there's a mutator");
          happened = 1;
                      }
          break;
        case PREDICATE: {
          predicate_t pred = (predicate_t) automata->edges[i];
          happened = pred(obs);
                        }
          break;
        default: assert(0 && "Invalid type");
      }
      if (happened) {
        pred_pos = (pred_pos+1)%STORE_SIZE;
        curr_node = automata->destinations[i];
        break;
      }
    }
    assert(happened); // Syntax error!
  }
}

void run_nonterminal(nonterminal_t *nt) {
  production_t *prod;
beginning:
  prod = run_dfa(nt->automata);
  for(int i = 0; i < prod->num_symbols; i++ ) {
    switch( prod->types[i] ) {
      case NONTERMINAL:
        if(i == prod->num_symbols-1) {
          nt = (nonterminal_t*) prod->data[i];
          goto beginning; // TCO
        }
        else {
          run_nonterminal((nonterminal_t*) prod->data[i]);
        }
        break;
      case KLEENE: {
        predicate_t pred = (predicate_t) prod->data[i];
        observation_t obs = get_observation(curr_beg, NULL);
        if(pred(obs)) {
          i--;
        }
        curr_beg = (curr_beg+1)%STORE_SIZE;
                   }
        break;
      case PREDICATE: {
        predicate_t pred = (predicate_t) prod->data[i];
        observation_t obs = get_observation(curr_beg, NULL);
        assert(pred(obs) && "Syntax error OR bug in dfa/prediction");
        curr_beg = (curr_beg+1)%STORE_SIZE;
                      }
        break;
      case MUTATOR: {
        mutator_t mutator = (mutator_t) prod->data[i];
        bool_t is_fetched_observation;
        get_observation(curr_beg, &is_fetched_observation);
        handle_mutator(mutator, is_fetched_observation);
        curr_beg = (curr_beg+1)%STORE_SIZE;
                    }
        break;
    }
  }
}
