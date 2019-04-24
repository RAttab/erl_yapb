#include <string.h>
#include <string>

#include <erl_nif.h>

#include "da_message.pb.h"

#if GOOGLE_PROTOBUF_VERSION < 3000000
#error "The proto definitions use 'proto3' syntax."
#error "This feature appeared in protobuf 3, but"
#error "it appears your protobuf is older.  Please"
#error "update protobuf."
#endif

static int p_msg_da_record(ErlNifEnv *env, const ERL_NIF_TERM r,::da_record *m);
static ERL_NIF_TERM u_msg_da_record(ErlNifEnv *env, const ::da_record *m);

static void install_consts(ErlNifEnv *env)
{
}
static ERL_NIF_TERM gpb_x_no_value;
static ERL_NIF_TERM gpb_aa_da_record;

static void install_atoms(ErlNifEnv *env)
{
    gpb_x_no_value = enif_make_atom(env, "undefined");
    gpb_aa_da_record = enif_make_atom(env, "da_record");
}

static int is_key(ErlNifEnv *env,
                  const ERL_NIF_TERM x,
                  const ERL_NIF_TERM key_to_check_for)
{
    return enif_is_identical(x, key_to_check_for);
}
static ERL_NIF_TERM mk_key(ErlNifEnv *env,
                           const ERL_NIF_TERM key)
{
    return key;
}
static ERL_NIF_TERM
encode_msg_da_record(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary data;
    int byteSize;
    ::da_record *m = new ::da_record();

    if (argc != 1)
    {
        delete m;
        return enif_make_badarg(env);
    }

    if (m == NULL)
    {
        delete m;
        return enif_make_badarg(env);
    }

    if (!p_msg_da_record(env, argv[0], m))
    {
        delete m;
        return enif_make_badarg(env);
    }

    byteSize = m->ByteSize();
    if (!enif_alloc_binary(byteSize, &data))
    {
        delete m;
        return enif_make_badarg(env);
    }

    if (!m->SerializeToArray(data.data, byteSize))
    {
        delete m;
        return enif_make_badarg(env);
    }

    delete m;
    return enif_make_binary(env, &data);
}

static int
p_msg_da_record(ErlNifEnv *env, const ERL_NIF_TERM r, ::da_record *m)
{
    int arity;
    const ERL_NIF_TERM *elem;

    if (!enif_get_tuple(env, r, &arity, &elem))
        return 0;

    if (arity != 3)
        return 0;

    if (!enif_is_identical(elem[1], gpb_x_no_value))
    {
        unsigned int v;
        if (!enif_get_uint(env, elem[1], &v))
            return 0;
        m->set_a(v);
    }
    if (!enif_is_identical(elem[2], gpb_x_no_value))
    {
        unsigned int v;
        if (!enif_get_uint(env, elem[2], &v))
            return 0;
        m->set_b(v);
    }

    return 1;
}

static ERL_NIF_TERM
decode_msg_da_record(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary data;
    ERL_NIF_TERM res;
    ::da_record *m = new ::da_record();

    if (argc != 1)
    {
        delete m;
        return enif_make_badarg(env);
    }

    if (m == NULL)
    {
        delete m;
        return enif_make_badarg(env);
    }

    if (!enif_inspect_binary(env, argv[0], &data))
    {
        delete m;
        return enif_make_badarg(env);
    }

    if (!m->ParseFromArray(data.data, data.size))
    {
        delete m;
        return enif_make_badarg(env);
    }

    res = u_msg_da_record(env, m);
    delete m;
    return res;
}

static ERL_NIF_TERM
u_msg_da_record(ErlNifEnv *env, const ::da_record *m)
{
    ERL_NIF_TERM res;
    ERL_NIF_TERM rname = gpb_aa_da_record;
    ERL_NIF_TERM elem1;
    ERL_NIF_TERM elem2;

    elem1 = enif_make_uint(env, m->a());
    elem2 = enif_make_uint(env, m->b());

    res = enif_make_tuple(env, 3, rname, elem1, elem2);
    return res;
}

static int
load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    install_consts(env);
    install_atoms(env);
    return 0;
}

static int
reload(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

void
unload(ErlNifEnv *env, void *priv_data)
{
}

static int
upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data,
        ERL_NIF_TERM load_info)
{
    return 0;
}

static ErlNifFunc nif_funcs[] =
{
#if ERL_NIF_MAJOR_VERSION > 2 || (ERL_NIF_MAJOR_VERSION == 2 && ERL_NIF_MINOR_VERSION >= 7)
#ifdef ERL_NIF_DIRTY_SCHEDULER_SUPPORT
    {"encode_msg_da_record", 1, encode_msg_da_record, ERL_NIF_DIRTY_JOB_CPU_BOUND, },
    {"decode_msg_da_record", 1, decode_msg_da_record, ERL_NIF_DIRTY_JOB_CPU_BOUND, }
#else /* ERL_NIF_DIRTY_SCHEDULER_SUPPORT */
    {"encode_msg_da_record", 1, encode_msg_da_record, },
    {"decode_msg_da_record", 1, decode_msg_da_record, }
#endif /* ERL_NIF_DIRTY_SCHEDULER_SUPPORT */
#else /* before 2.7 or 17.3 */
    {"encode_msg_da_record", 1, encode_msg_da_record},
    {"decode_msg_da_record", 1, decode_msg_da_record}
#endif /* before 2.7 or 17.3 */
};

ERL_NIF_INIT(da_message, nif_funcs, load, reload, upgrade, unload)
