#include <string.h>
#include <string>

#include <erl_nif.h>

#include "stvlist.pb.h"

#if GOOGLE_PROTOBUF_VERSION < 3000000
#error "The proto definitions use 'proto3' syntax."
#error "This feature appeared in protobuf 3, but"
#error "it appears your protobuf is older.  Please"
#error "update protobuf."
#endif

static int p_msg_stvlist(ErlNifEnv *env, const ERL_NIF_TERM r,::stvlist *m);
static ERL_NIF_TERM u_msg_stvlist(ErlNifEnv *env, const ::stvlist *m);

static void install_consts(ErlNifEnv *env)
{
}
static ERL_NIF_TERM gpb_x_no_value;
static ERL_NIF_TERM gpb_aa_stvlist;

static void install_atoms(ErlNifEnv *env)
{
    gpb_x_no_value = enif_make_atom(env, "undefined");
    gpb_aa_stvlist = enif_make_atom(env, "stvlist");
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
encode_msg_stvlist(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary data;
    int byteSize;
    ::stvlist *m = new ::stvlist();

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

    if (!p_msg_stvlist(env, argv[0], m))
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
p_msg_stvlist(ErlNifEnv *env, const ERL_NIF_TERM r, ::stvlist *m)
{
    int arity;
    const ERL_NIF_TERM *elem;

    if (!enif_get_tuple(env, r, &arity, &elem))
        return 0;

    if (arity != 3)
        return 0;

    if (!enif_is_identical(elem[1], gpb_x_no_value))
    {
        ErlNifBinary b;
        if (enif_inspect_binary(env, elem[1], &b)) {
            m->set_tvid(reinterpret_cast<char *>(b.data), b.size);
        } else if (enif_is_list(env, elem[1])) {
            if (enif_inspect_iolist_as_binary(env, elem[1], &b)) {
                m->set_tvid(reinterpret_cast<char *>(b.data), b.size);
            } else {
                return 0;
            }
        } else {
            return 0;
        }
    }
    {
        ERL_NIF_TERM l = elem[2];

        while (!enif_is_empty_list(env, l))
        {
            ERL_NIF_TERM head, tail;

            if (!enif_get_list_cell(env, l, &head, &tail))
                return 0;

        {
            unsigned int v;
            if (!enif_get_uint(env, head, &v))
                return 0;
            m->add_ids(v);
        }
            l = tail;
        }
    }

    return 1;
}

static ERL_NIF_TERM
decode_msg_stvlist(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary data;
    ERL_NIF_TERM res;
    ::stvlist *m = new ::stvlist();

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

    res = u_msg_stvlist(env, m);
    delete m;
    return res;
}

static ERL_NIF_TERM
u_msg_stvlist(ErlNifEnv *env, const ::stvlist *m)
{
    ERL_NIF_TERM res;
    ERL_NIF_TERM rname = gpb_aa_stvlist;
    ERL_NIF_TERM elem1;
    ERL_NIF_TERM elem2;

    {
        unsigned char *data;
        unsigned int   bSize = m->tvid().size();
        const char    *bData = m->tvid().data();
        data = enif_make_new_binary(
                   env,
                   bSize,
                   &elem1);
        memmove(data, bData, bSize);
    }
    {
        unsigned int numElems = m->ids_size();
        ERL_NIF_TERM relem[numElems];
        unsigned int i;

        for (i = 0; i < numElems; i++)
            relem[i] = enif_make_uint(env, m->ids(i));
        elem2 = enif_make_list_from_array(env, relem, numElems);
    }

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
    {"encode_msg_stvlist", 1, encode_msg_stvlist, ERL_NIF_DIRTY_JOB_CPU_BOUND, },
    {"decode_msg_stvlist", 1, decode_msg_stvlist, ERL_NIF_DIRTY_JOB_CPU_BOUND, }
#else /* ERL_NIF_DIRTY_SCHEDULER_SUPPORT */
    {"encode_msg_stvlist", 1, encode_msg_stvlist, },
    {"decode_msg_stvlist", 1, decode_msg_stvlist, }
#endif /* ERL_NIF_DIRTY_SCHEDULER_SUPPORT */
#else /* before 2.7 or 17.3 */
    {"encode_msg_stvlist", 1, encode_msg_stvlist},
    {"decode_msg_stvlist", 1, decode_msg_stvlist}
#endif /* before 2.7 or 17.3 */
};

ERL_NIF_INIT(stvlist, nif_funcs, load, reload, upgrade, unload)
