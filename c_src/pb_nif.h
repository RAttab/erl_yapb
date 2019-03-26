#include "utils/htable.h"
#include "erl_nif.h"
#include "pb.h"
#define yapb_unused __attribute__((unused))

struct pb_state
{
    struct htable defs;
    ERL_NIF_TERM    atom_ok;
};

struct pb_message
{
    int32_t count;
    struct pb_field_def *fields;
};

struct pb_field_def
{
    ERL_NIF_TERM name;
    int32_t fnum;
    int32_t rnum;
    enum pb_type type;
    //occurence;
    //opts;
};

void print_map(struct htable *);
void parse_type(struct pb_field_def *, ERL_NIF_TERM);
static char *alloc_string(ErlNifBinary);
