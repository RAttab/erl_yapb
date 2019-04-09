#pragma once

#include "erl_nif.h"
#include "pb.h"
#include "cache.h"

struct pb_field_cache
{
    ERL_NIF_TERM name;
    int32_t fnum;
    int32_t rnum;
    enum pb_type type;
    //occurence;
    //opts;
};

void parse_field(ErlNifEnv *env, ERL_NIF_TERM term, struct pb_field_cache *field);

void parse_type(ErlNifEnv *env, struct pb_field_cache *field, ERL_NIF_TERM type);

ERL_NIF_TERM make_term_from_type(ErlNifEnv *env, enum pb_type type, const union pb_value *value);
