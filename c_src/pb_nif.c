#include "erl_nif.h"
#include "pb_nif.h"
#include "pb.h"
#include "utils/htable.h"
#include "utils/type_pun.h"
#include <stdlib.h>
#include <string.h>

static ERL_NIF_TERM ATOM_INT32;
static void init(ErlNifEnv* env, struct pb_state *state)
{
    state->atom_ok = enif_make_atom(env, "ok");
    ATOM_INT32 = enif_make_atom(env, "int32");
}

struct entry
{
    pb_field_t field;
    enum pb_type type;
    union pb_value value;
};

struct htable make_entries_impl(struct entry **entries, size_t len)
{
    struct htable htable = {0};
    htable_reserve(&htable, len);

    for (size_t i = 0; i < len; i++)
        htable_put(&htable, entries[i]->field, (uint64_t) entries[i]);

    return htable;
}

static int load(ErlNifEnv* env, void **priv_data, yapb_unused ERL_NIF_TERM load_info)
{
    struct pb_state *state;
    state = malloc(sizeof(struct pb_state));

    init(env, state);

    *priv_data = (void *) state;

    return 0;
}

static ERL_NIF_TERM nif_encode(yapb_unused ErlNifEnv* env, yapb_unused int argc, yapb_unused const ERL_NIF_TERM argv[])
{
    struct pb_state *state = (struct pb_state *) enif_priv_data(env);
    return state->atom_ok;
}

static ERL_NIF_TERM nif_decode(yapb_unused ErlNifEnv* env, yapb_unused int argc, yapb_unused const ERL_NIF_TERM argv[])
{
    struct htable result = {0};
    ErlNifBinary    bin;
    enif_fprintf(stderr, "decode: %T\n", argv[2]);
    struct pb_reader reader = {0};
    struct pb_state *state = (struct pb_state *) enif_priv_data(env);
    enif_inspect_binary(env, argv[0], &bin);
    uint8_t *value = (uint8_t *) alloc_string(bin);
    pb_read_init(&reader, value, sizeof(argv[0]));

    //struct htable_ret ret = htable_get(&entries, tag.field);
    while (reader.it != reader.end) {
        struct pb_tag tag = {0};
        pb_read_tag(&reader, &tag);
        enif_fprintf(stderr, "tag: field=%lu, wire=%d\n", tag.field, tag.wire);

    //    struct htable_ret ret = htable_get(&entries, tag.field);
    //    assert(ret.ok);
    //    struct entry *entry = (struct entry *) ret.value;

    //    union pb_value value = {0};
    //    assert(pb_read_field(&reader, tag.wire, entry->type, &value));

    //    if (tag.wire != pb_wire_bytes)
    //        assert(value.u64 == entry->value.u64);
    //    else {
    //        size_t len = entry->value.bin.end - entry->value.bin.it;
    //        assert((value.bin.end - value.bin.it) == (ssize_t) len);
    //        assert(!memcmp(value.bin.it, entry->value.bin.it, len));
    //    }

    //    assert(htable_put(&result, entry->field, 0).ok);
    }
    print_map(&result);
    return state->atom_ok;
}

static ERL_NIF_TERM nif_add_schema(yapb_unused ErlNifEnv* env, yapb_unused int argc, yapb_unused const ERL_NIF_TERM argv[])
{
    char msg_name[50];
    //char type[50];
    unsigned int len;
    ERL_NIF_TERM head, tail, next;
    const ERL_NIF_TERM *array, *msg_array, *field_array = NULL;
    int32_t arity;
    struct pb_state *state = (struct pb_state *) enif_priv_data(env);

    struct htable htable = {0};

    next = argv[0];
    // loop over messages
    enif_get_list_length(env, next, &len);
    htable_reserve(&htable, len);
    while (enif_get_list_cell(env, next, &head, &tail)) {
        unsigned int fields_len;
        enif_get_list_cell(env, argv[0], &head, &tail);
        enif_get_tuple(env, head, &arity, &array);
        // fields are in array[1]
        // get message name
        enif_get_tuple(env, array[0], &arity, &msg_array);
        enif_get_atom(env, msg_array[1], msg_name, 50, ERL_NIF_LATIN1);
        next = array[1];
        enif_get_list_length(env, next, &fields_len);
        //struct pb_field_def *fields = calloc(fields_len, sizeof(*fields));
        struct pb_field_def *fields = enif_alloc(fields_len * sizeof(*fields));
        for (unsigned int i = 0; i < fields_len; i++) {
            enif_get_list_cell(env, next, &head, &tail);
            enif_get_tuple(env, head, &arity, &field_array);
            enif_fprintf(stderr, "head: %T\n", head);
            struct pb_field_def field = {0};
            enif_get_int(env, field_array[2], &field.fnum);
            enif_get_int(env, field_array[3], &field.rnum);
            //enif_get_atom(env, field_array[4], type, 50, ERL_NIF_LATIN1);
            parse_type(&field, field_array[4]);
            enif_fprintf(stderr, "type: %u\n", field.type);
            fields[i] = field;
            next = tail;
        }

        struct pb_message *message = enif_alloc(sizeof(*message));
        message->count = len;
        message->fields = fields;
        htable_put(&htable, pun_stoi(msg_name), pun_mtoi(message));

        next = tail;
    }

    print_map(&htable);
    //(void) state->defs;
    state->defs = &htable;
    enif_fprintf(stderr, "HERE: %T\n", state->atom_ok);

    return state->atom_ok;
}

void print_map(struct htable *htable)
{
    for (struct htable_bucket *bucket = htable_next(htable, NULL);
            bucket;
            bucket = htable_next(htable, bucket))
    {

        struct pb_message msg = pun_itom(bucket->value);
        for (int i = 0; i < msg.count; i++) {
            enif_fprintf(stderr, "content: %u\n", msg.count);
            enif_fprintf(stderr, "fnum: %u\n", msg.fields[1].fnum);
            enif_fprintf(stderr, "rnum: %u\n", msg.fields[1].rnum);
            //(void) msg;
        }
    }
    //struct htable_ret ret = htable_get(&htable, msg_name);
}

static char *alloc_string(ErlNifBinary bin)
{
    size_t key_len = bin.size;
    char *key = enif_alloc((key_len + 1) * sizeof(*key));
    if (!key) {
        return NULL;
    }

    memcpy(key, bin.data, key_len);

    key[key_len] = 0;

    return key;
}

void parse_type(struct pb_field_def *field, ERL_NIF_TERM type)
{
    if (type == ATOM_INT32)          { field->type = pb_32_uint; }
    //else if (term == state->atom_int64)     { field->type = field_int64; return RET_OK; }
}

static ErlNifFunc nif_functions[] = {
    {"encode", 0, nif_encode, 0},
    {"decode", 3, nif_decode, 2},
    {"add_schema", 1, nif_add_schema, 2}
};

ERL_NIF_INIT(erl_yapb_nif, nif_functions, &load, NULL, NULL, NULL);
