#include "erl_nif.h"
#include "cache.h"
#include "field.h"

void parse_type(ErlNifEnv *env, struct pb_field_cache *field, ERL_NIF_TERM type)
{
    struct pb_state *state = (struct pb_state *) enif_priv_data(env);

    if (type == state->atom_int32) { field->type = pb_32_uint; }
    else if (type == state->atom_int64) { field->type = pb_64_uint; }
    else if (type == state->atom_float) { field->type = pb_32_float; }
    else if (type == state->atom_double) { field->type = pb_64_float; }
}

void parse_field(ErlNifEnv *env, ERL_NIF_TERM term, struct pb_field_cache *field)
{
    int32_t arity;
    const ERL_NIF_TERM *field_array = NULL;

    enif_get_tuple(env, term, &arity, &field_array);
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
        case pb_bool: if (value->b) { return state->atom_true; } else { return state->atom_false; };
        case pb_32_uint:
        case pb_32_ufixed: return enif_make_uint(env, value->u32);
        case pb_32_sint:
        case pb_32_sfixed: return enif_make_int(env, value->s32);
        case pb_64_uint:
        case pb_64_ufixed: return enif_make_uint64(env, value->u64);
        case pb_64_sint:
        case pb_64_sfixed: return enif_make_int64(env, value->s64);
        case pb_32_float: return enif_make_double(env, value->f32);
        case pb_64_float: return enif_make_double(env, value->f64);
        default: return state->atom_ok;
    }
}
