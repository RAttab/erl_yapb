#pragma once

#include "erl_nif.h"
#include "field.h"
#include "utils/htable.h"

struct pb_state
{
    struct htable defs;
    ERL_NIF_TERM atom_ok;
    ERL_NIF_TERM atom_int32;
    ERL_NIF_TERM atom_true;
    ERL_NIF_TERM atom_false;
};

struct pb_message
{
    int32_t count;
    struct pb_field_cache *fields;
};

void initialize_cache(struct htable *htable, int len);

const struct pb_field_cache *find_field(const struct pb_message *message, int field_id);
