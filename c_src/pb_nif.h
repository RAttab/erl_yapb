#include "utils/htable.h"
#include "erl_nif.h"
#include "pb.h"
#define yapb_unused __attribute__((unused))

struct pb_state
{
    struct htable *defs;
    ERL_NIF_TERM    atom_ok;
};

void print_map(struct htable *);
void parse_type(struct pb_field_def *, ERL_NIF_TERM);
static char *alloc_string(ErlNifBinary);
