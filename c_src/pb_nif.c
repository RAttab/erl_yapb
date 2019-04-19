#include "erl_nif.h"
#include "pb_nif.h"
#include "cache.h"
#include "field.h"
#include "utils/htable.h"
#include "utils/type_pun.h"
#include <stdlib.h>
#include <string.h>
#include <assert.h>

static void init(ErlNifEnv* env, struct pb_state *state)
{
    state->atom_ok = enif_make_atom(env, "ok");
    state->atom_int32 = enif_make_atom(env, "int32");
    state->atom_int64 = enif_make_atom(env, "int64");
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

static ERL_NIF_TERM nif_decode(ErlNifEnv* env, yapb_unused int argc, const ERL_NIF_TERM argv[])
{
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
        assert(pb_read_field(&reader, tag.wire, field->type, &value));

        terms[tag.field] = make_term_from_type(env, field->type, &value);

        //enif_fprintf(stderr, "HAHAHAH: %u\n", tag.wire);
        //enif_fprintf(stderr, "HAHAHAH: %u\n", field->type);
        //enif_fprintf(stderr, "HAHAHAH: %u\n", value.s32);

    //    if (tag.wire != pb_wire_bytes)
    //        assert(value.u64 == entry->value.u64);
    //    else {
    //        size_t len = entry->value.bin.end - entry->value.bin.it;
    //        assert((value.bin.end - value.bin.it) == (ssize_t) len);
    //        assert(!memcmp(value.bin.it, entry->value.bin.it, len));
    //    }

    //    assert(htable_put(&result, entry->field, 0).ok);
    }

    ERL_NIF_TERM decoded = enif_make_tuple_from_array(env, terms, message->count + 1);
    //print_map(&result);
    return decoded;
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

//static char *alloc_string(ErlNifBinary bin)
//{
//    size_t key_len = bin.size;
//    char *key = enif_alloc((key_len + 1) * sizeof(*key));
//    if (!key) {
//        return NULL;
//    }
//
//    memcpy(key, bin.data, key_len);
//
//    key[key_len] = 0;
//
//    return key;
//}

//void infer_schema(struct pb_field_cache *fields, pb_tag tag)
//{
//    for (size_t i = 0; i < sizeof(fields); i++){
//        fields[i]
//    }
//}

static ErlNifFunc nif_functions[] = {
    {"encode", 0, nif_encode, 0},
    {"decode", 2, nif_decode, 2},
    {"add_schema", 1, nif_add_schema, 2}
};

ERL_NIF_INIT(erl_yapb_nif, nif_functions, load, NULL, NULL, unload);
