#pragma once

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

typedef uint64_t pb_field_t;

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
    pb_32_ufixed,
    pb_32_sfixed,
    pb_32_float,

    pb_64_int,
    pb_64_uint,
    pb_64_sint,
    pb_64_ufixed,
    pb_64_sfixed,
    pb_64_float,
};

enum pb_wire
{
    pb_wire_varint = 0,
    pb_wire_32 = 5,
    pb_wire_64 = 1,
    pb_wire_bytes = 2
};

struct pb_tag
{
    pb_field_t field;
    enum pb_wire wire;
};

struct pb_buffer
{
    size_t cap;
    uint8_t *data;
};

struct pb_reader
{
    const uint8_t *it;
    const uint8_t *end;
};

struct pb_writer
{
    uint8_t *it;
    uint8_t *end;
    struct pb_buffer *buffer;
};

union pb_value
{
    bool b;

    int32_t s32;
    uint32_t u32;
    int64_t s64;
    uint64_t u64;

    float f32;
    double f64;
    struct pb_reader bin;
};

void pb_read_init(struct pb_reader *, uint8_t *data, size_t len);
bool pb_read_tag(struct pb_reader *, struct pb_tag *);
bool pb_read_field(struct pb_reader *, enum pb_wire, enum pb_type, union pb_value *);
bool pb_read_varint(struct pb_reader *, enum pb_type, union pb_value *);

void pb_buffer_resize(struct pb_buffer *, size_t cap);
void pb_buffer_free(struct pb_buffer *);

void pb_write_init(struct pb_writer *, struct pb_buffer *);
bool pb_write_field(struct pb_writer *, pb_field_t field, enum pb_type, union pb_value);
bool pb_write_varint(struct pb_writer *, enum pb_type, union pb_value);
