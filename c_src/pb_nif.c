#include "erl_nif.h"
#include "pb_nif.h"
#include "cache.h"
#include "field.h"
#include "utils/htable.h"
#include "utils/type_pun.h"
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <assert.h>

static uint64_t stats_sum = 0;
static uint64_t stats_iterations = 0;
static size_t stats_values[1000000] = {0};

static void init(ErlNifEnv* env, struct pb_state *state)
{
    state->atom_ok = enif_make_atom(env, "ok");
    state->atom_int32 = enif_make_atom(env, "int32");
    state->atom_uint32 = enif_make_atom(env, "uint32");
    state->atom_sint32 = enif_make_atom(env, "sint32");
    state->atom_int64 = enif_make_atom(env, "int64");
    state->atom_uint64 = enif_make_atom(env, "uint64");
    state->atom_sint64 = enif_make_atom(env, "sint64");
    state->atom_float = enif_make_atom(env, "float");
    state->atom_double = enif_make_atom(env, "double");
    state->atom_true = enif_make_atom(env, "true");
    state->atom_false = enif_make_atom(env, "false");
}

static int load(ErlNifEnv* env, void **priv_data, yapb_unused ERL_NIF_TERM load_info)
{
    struct pb_state *state;
    state = enif_alloc(sizeof(*state));
    memset(state, 0, sizeof(*state));

    init(env, state);

    *priv_data = (void *) state;

    return 0;
}

static void unload(ErlNifEnv *env, yapb_unused void *priv_data){
    struct pb_state *state = (struct pb_state *) enif_priv_data(env);
    htable_reset(&state->defs);
    enif_free(state);
}

static ERL_NIF_TERM nif_encode(ErlNifEnv* env, yapb_unused int argc, yapb_unused const ERL_NIF_TERM argv[])
{
    struct pb_state *state = (struct pb_state *) enif_priv_data(env);
    return state->atom_ok;
}

void hexdump(const uint8_t *buffer, size_t len)
{
    for (size_t i = 0; i < len;) {
        printf("%6p: ", (void *) i);
        for (size_t j = 0; j < 16 && i < len; ++i, ++j) {
            if (j % 2 == 0) printf(" ");
            printf("%02x", buffer[i]);
        }
        printf("\n");
    }
}

inline void yapb_timer_start(struct timespec *t0)
{
    if (clock_gettime(CLOCK_MONOTONIC, t0)) abort();
}

inline uint64_t yapb_timer_elapsed(struct timespec *t0)
{
    struct timespec t1;
    if (clock_gettime(CLOCK_MONOTONIC, &t1)) abort();

    const uint64_t nano_sec = 1UL * 1000 * 1000 * 1000;

    uint64_t secs = t1.tv_sec - t0->tv_sec;
    uint64_t nanos = secs ?
        (nano_sec - t1.tv_nsec) + t0->tv_nsec :
        (uint64_t) (t1.tv_nsec - t0->tv_nsec);
    if (nanos > 10000)
    {
        enif_fprintf(stderr, "secs: %lu nanos: %lu ", secs, nanos);
        enif_fprintf(stderr, "t1.tv_sec: %lu t0.tv_sec: %lu ", t1.tv_sec, t0->tv_sec);
        enif_fprintf(stderr, "t1.tv_nsec: %lu t0.tv_nsec: %lu \n", t1.tv_nsec, t0->tv_nsec);
    }
    return nanos;
}

static ERL_NIF_TERM nif_decode(ErlNifEnv* env, yapb_unused int argc, const ERL_NIF_TERM argv[])
{
    struct timespec time = {0};
    yapb_timer_start(&time);
    ErlNifBinary bin;
    struct pb_reader reader = {0};
    struct pb_state *state = (struct pb_state *) enif_priv_data(env);
    enif_inspect_binary(env, argv[0], &bin);
    pb_read_init(&reader, bin.data, bin.size);

    struct htable_ret ret = htable_get(&state->defs, pun_ttoi(argv[1]));
    assert(ret.ok);
    const struct pb_message *message = pun_itom(ret.value);

    ERL_NIF_TERM *terms = enif_alloc(sizeof(*terms) * message->count + 1);

    // message name
    terms[0] = argv[1];
    while (reader.it != reader.end) {
        struct pb_tag tag = {0};
        pb_read_tag(&reader, &tag);
        assert(tag.field > 0);

        const struct pb_field_cache *field = find_field(message, tag.field);

        union pb_value value = {0};
        //enif_fprintf(stderr, "HAHAHAH: %d\n", field->type);
        assert(pb_read_field(&reader, tag.wire, field->type, &value));

        terms[tag.field] = make_term_from_type(env, field->type, &value);
    }

    ERL_NIF_TERM decoded = enif_make_tuple_from_array(env, terms, message->count + 1);
    //print_map(&result);
    uint64_t elapsed = yapb_timer_elapsed(&time);
    stats_sum += elapsed;
    stats_values[stats_iterations] = elapsed;
    stats_iterations += 1;

    return decoded;
}

static int lolsort(const void *a, const void *b)
{
    uint64_t aa = *((const uint64_t *) a);
    uint64_t bb = *((const uint64_t *) b);
    if (aa < bb) { return -1; }
    if (aa > bb) { return 1; }
    return 0;
}

static ERL_NIF_TERM nif_print_stats(ErlNifEnv* env, yapb_unused int argc, yapb_unused const ERL_NIF_TERM argv[])
{
    struct pb_state *state = (struct pb_state *) enif_priv_data(env);
    size_t n = sizeof(stats_values) / sizeof(stats_values[0]);

    qsort(&stats_values, sizeof(stats_values) / sizeof(stats_values[0]), sizeof(stats_values[0]), &lolsort);

    enif_fprintf(stderr, "Average: %lu on %lu iterations\n", stats_sum / stats_iterations, stats_iterations);
    enif_fprintf(stderr, "min: %lu max: %lu\n", stats_values[0], stats_values[n-1]);
    enif_fprintf(stderr, "median: %lu\n", stats_values[(n/2)-1]);
    uint64_t ninetieth = n * 0.9;
    uint64_t ninetynine = n * 0.99;
    enif_fprintf(stderr, "90th: %lu\n", stats_values[ninetieth]);
    enif_fprintf(stderr, "99th: %lu\n", stats_values[ninetynine]);
    return state->atom_ok;
}

static ERL_NIF_TERM nif_add_schema(ErlNifEnv* env, yapb_unused int argc, const ERL_NIF_TERM argv[])
{
    struct htable htable = {0};
    unsigned int len;
    ERL_NIF_TERM head, msg_tail, field_tail, meh, next;
    const ERL_NIF_TERM *array, *msg_array = NULL;
    int32_t arity;
    struct pb_state *state = (struct pb_state *) enif_priv_data(env);

    next = argv[0];
    enif_get_list_length(env, next, &len);
    initialize_cache(&htable, len);
    // loop over messages
    while (enif_get_list_cell(env, next, &head, &msg_tail)) {
        unsigned int fields_len;
        enif_get_list_cell(env, argv[0], &head, &meh);
        enif_get_tuple(env, head, &arity, &array);
        // fields are in array[1]
        // get message name
        enif_get_tuple(env, array[0], &arity, &msg_array);
        next = array[1];
        enif_get_list_length(env, next, &fields_len);
        struct pb_field_cache *fields = enif_alloc(fields_len * sizeof(*fields));
        for (unsigned int i = 0; i < fields_len; i++) {
            enif_get_list_cell(env, next, &head, &field_tail);
            parse_field(env, head, &fields[i]);
            fields[i].name = msg_array[1];
            next = field_tail;
        }

        struct pb_message *message = enif_alloc(sizeof(*message));
        message->count = fields_len;
        message->fields = fields;
        htable_put(&htable, pun_ttoi(msg_array[1]), pun_mtoi(message));

        next = msg_tail;
    }

    state->defs = htable;

    return state->atom_ok;
}

void print_map(struct htable *htable)
{
    for (struct htable_bucket *bucket = htable_next(htable, NULL);
            bucket;
            bucket = htable_next(htable, bucket))
    {

        struct pb_message *msg = pun_itom(bucket->value);
        for (int i = 0; i < msg->count; i++) {
            enif_fprintf(stderr, "fields len: %u\n", msg->count);
            enif_fprintf(stderr, "fnum: %u\n", msg->fields[i].fnum);
            enif_fprintf(stderr, "rnum: %u\n", msg->fields[i].rnum);
            enif_fprintf(stderr, "name: %T\n", msg->fields[i].name);
        }
    }
    //struct htable_ret ret = htable_get(&htable, msg_name);
}

static ErlNifFunc nif_functions[] = {
    {"encode", 0, nif_encode, 0},
    {"decode", 2, nif_decode, 2},
    {"print_stats", 0, nif_print_stats, 0},
    {"add_schema", 1, nif_add_schema, 2}
};

ERL_NIF_INIT(erl_yapb_nif, nif_functions, load, NULL, NULL, unload);
