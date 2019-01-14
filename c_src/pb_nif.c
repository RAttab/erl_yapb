#include "erl_nif.h"
#include "pb.h"

static ERL_NIF_TERM ATOM_OK;
static void init(ErlNifEnv* env)
{
    ATOM_OK = enif_make_atom(env, "ok");
}

static int load(ErlNifEnv* env, __attribute__((unused)) void **priv_data, __attribute__((unused)) ERL_NIF_TERM load_info)
{

    init(env);

    return 0;
}

static ERL_NIF_TERM nif_encode(__attribute__((unused)) ErlNifEnv* env, __attribute__((unused)) int argc, __attribute__((unused)) const ERL_NIF_TERM argv[])
{
    return ATOM_OK;
}

static ErlNifFunc nif_functions[] = {
    {"encode", 0, nif_encode, 0}
};

ERL_NIF_INIT(erl_yapb_nif, nif_functions, &load, NULL, NULL, NULL);
