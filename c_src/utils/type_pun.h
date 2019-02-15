#include "erl_nif.h"
#include "../pb.h"
#pragma once

inline uint64_t pun_ttoi(ERL_NIF_TERM value)
{
    return (union { uint64_t i; ERL_NIF_TERM t; }) { .t = value }.i;
}

inline ERL_NIF_TERM pun_itot(uint64_t value)
{
    return (union { uint64_t i; ERL_NIF_TERM t; }) { .i = value }.t;
}


inline uint64_t pun_mtoi(struct pb_message *value)
{
    return (union { uint64_t i; struct pb_message *t; }) { .t = value }.i;
}

inline struct pb_message pun_itom(uint64_t value)
{
    return *(union { uint64_t i; struct pb_message *t; }) { .i = value }.t;
}




inline uint64_t pun_stoi(char * value)
{
    return (union { uint64_t i; char * t; }) { .t = value }.i;
}

inline char * pun_itos(uint64_t value)
{
    return (union { uint64_t i; char * t; }) { .i = value }.t;
}
