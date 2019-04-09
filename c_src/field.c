#include "erl_nif.h"
#include "cache.h"
#include "field.h"

void parse_type(ErlNifEnv *env, struct pb_field_cache *field, ERL_NIF_TERM type)
{
    struct pb_state *state = (struct pb_state *) enif_priv_data(env);

    if (type == state->atom_int32) { field->type = pb_32_uint; }
    //else if (term == state->atom_int64)     { field->type = field_int64; return RET_OK; }
}

void parse_field(ErlNifEnv *env, ERL_NIF_TERM term, struct pb_field_cache *field)
{
    int32_t arity;
    const ERL_NIF_TERM *field_array = NULL;

    enif_get_tuple(env, term, &arity, &field_array);
    enif_fprintf(stderr, "head: %T\n", term);
    enif_get_int(env, field_array[2], &field->fnum);
    enif_get_int(env, field_array[3], &field->rnum);
    //field->name = msg_array[1];
    parse_type(env, field, field_array[4]);
}

ERL_NIF_TERM make_term_from_type(ErlNifEnv *env, enum pb_type type, const union pb_value *value)
{
    struct pb_state *state = (struct pb_state *) enif_priv_data(env);
    switch (type)
    {
        case pb_bool: if (value->b) { return state->atom_true; } else { return state->atom_false; } break;
        case pb_32_uint:
        case pb_32_ufixed: return enif_make_uint(env, value->u32);
        default: return state->atom_ok;
    }
}
