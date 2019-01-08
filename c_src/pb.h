#pragma once

#include <stdint.h>
#include <stdef.h>

struct pb_bin
{
    size_t len, cap;
    uint8_t *data;
};

struct pb_codec
{
    uint8_t *it;
    uint8_t *end;
    struct pb_bin bin;
};

enum pb_type
{
    pb_bool,
    pb_enum,
    pb_message,

    pb_bytes,
    pb_string,

    pb_32_int,
    pb_32_uint,
    pb_32_sint,
    pb_32_fixed,
    pb_32_sfixed,
    pb_32_float,

    pb_64_int,
    pb_64_uint,
    pb_64_sint,
    pb_64_fixed,
    pb_64_sfixed,
    pb_64_float,
};

enum pb_wire
{
    pb_wire_varint = 0,
    pb_wire_32 = 5,
    pb_wire_64 = 1,
    pb_wire_data = 2
};

struct pb_tag
{
    uint32_t field;
    enum pb_wire wire;
};

union pb_field
{
    bool bool;

    int32_t s32;
    uint32_t u32;
    int64_t s64;
    uint64_t u64;

    float f32;
    double f64;
    struct pb_bin *bin;
};


bool pb_init(struct pb_codec *codec);
bool pb_reset(struct pb_codec *codec, uint8_t *data, size_t len);
bool pb_free(struct pb_codec *codec);

bool pb_read_tag(struct pb_codec *codec, struct pb_tag *tag);
bool pb_read_field(struct pb_codec *codec, enum pb_wire wire, enum pb_type type, union pb_field *field);
