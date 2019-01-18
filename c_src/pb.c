#include "pb.h"

#include <string.h>
#include <stdlib.h>


// -----------------------------------------------------------------------------
// reader
// -----------------------------------------------------------------------------

void pb_read_init(struct pb_reader *reader, uint8_t *data, size_t len)
{
    *reader = (struct pb_reader) {
        .it = data,
        .end = data + len
    };
}

static bool read_data(struct pb_reader *reader, size_t len, void *dst)
{
    if (reader->it + len > reader->end) return false;
    memcpy(dst, reader->it, len);
    reader->it += len;
    return true;
}

static bool read_varint(struct pb_reader *reader, uint64_t *data)
{
    static const size_t shift = 7;
    static const uint64_t more_mask = 1UL << shift;
    static const uint64_t body_mask = (1UL << shift) - 1;

    if (reader->it == reader->end) return false;

    uint8_t byte;
    size_t pos = 0;
    *data = 0;

    do {
        byte = *reader->it; reader->it++;
        *data |= (byte & body_mask) << pos;
        pos += shift;
    } while ((byte & more_mask) && reader->it != reader->end);

    return !(byte & more_mask);
}

static bool read_bytes(struct pb_reader *reader, struct pb_reader *bin)
{
    size_t len = 0;
    if (!read_varint(reader, &len)) return false;
    if (reader->it + len > reader->end) return false;

    *bin = (struct pb_reader) {
        .it = reader->it,
        .end = reader->it + len,
    };

    reader->it += len;
    return true;
}

static int64_t zagzig(uint64_t value)
{
    return (value >> 1) ^ -(value & 0x1);
}

bool pb_read_tag(struct pb_reader *reader, struct pb_tag *tag)
{
    uint64_t data = 0;
    if (!read_varint(reader, &data)) return false;

    *tag = (struct pb_tag) {
        .field = data >> 3,
        .wire = data & 0x7
    };
    return true;
}

bool pb_read_varint(struct pb_reader *reader, enum pb_type type, union pb_value *value)
{
    switch (type) {

    case pb_32_uint:
    {
        uint64_t data = 0;
        if (!read_varint(reader, &data)) return false;
        if (data > UINT32_MAX) return false;
        value->u32 = data;
        return true;
    }

    case pb_bool:
    case pb_enum:
    case pb_64_uint:
        return read_varint(reader, &value->u64);

    case pb_32_sint:
    {
        if (!read_varint(reader, &value->u64)) return false;
        int64_t data = zagzig(value->u64);
        if (data > INT32_MAX || data < INT32_MIN) return false;
        value->s32 = data;
        return true;
    }

    case pb_64_sint:
        if (!read_varint(reader, &value->u64)) return false;
        value->s64 = zagzig(value->u64);
        return true;

    default: return false;
    }
}

bool pb_read_field(
        struct pb_reader *reader,
        enum pb_wire wire,
        enum pb_type type,
        union pb_value *value)
{
    switch (wire) {

    case pb_wire_varint:
        return pb_read_varint(reader, type, value);

    case pb_wire_32:
        switch (type) {
        case pb_32_ufixed:
        case pb_32_sfixed:
        case pb_32_float: return read_data(reader, sizeof(uint32_t), &value->u32);
        default: return false;
        }

    case pb_wire_64:
        switch (type) {
        case pb_64_ufixed:
        case pb_64_sfixed:
        case pb_64_float: return read_data(reader, sizeof(uint64_t), &value->u64);
        default: return false;
        }

    case pb_wire_bytes:
        return read_bytes(reader, &value->bin);

    default: return false;
    }
}

// -----------------------------------------------------------------------------
// writer
// -----------------------------------------------------------------------------

void pb_buffer_resize(struct pb_buffer *buffer, size_t cap)
{
    if (cap <= buffer->cap) return;

    if (!buffer->cap) buffer->cap = 8;
    while (cap > buffer->cap) buffer->cap *= 2;

    buffer->data = realloc(buffer->data, buffer->cap);
}

void pb_buffer_free(struct pb_buffer *buffer)
{
    free(buffer->data);
}


void pb_write_init(struct pb_writer *writer, struct pb_buffer *buffer)
{
    *writer = (struct pb_writer) {
        .it = buffer->data,
        .end = buffer->data + buffer->cap,
        .buffer = buffer,
    };
}

static void pb_write_ensure(struct pb_writer *writer, size_t len)
{
    if (writer->it + len <= writer->end) return;

    size_t pos = writer->it - writer->buffer->data;
    pb_buffer_resize(writer->buffer, (writer->it - writer->buffer->data) + len);

    writer->it = writer->buffer->data + pos;
    writer->end = writer->buffer->data + writer->buffer->cap;
}

static uint64_t zigzag(int64_t value)
{
    return (value << 1) ^ (value >> 63);
}


static inline size_t varint_size(uint64_t value)
{
    if (!value) return 1;
    return (64 - __builtin_clzl(value)) / 7 + 1;
}

static void write_varint(struct pb_writer *writer, uint64_t data)
{
    static const size_t shift = 7;
    static const uint64_t more_mask = 1UL << shift;
    static const uint64_t body_mask = (1UL << shift) - 1;

    pb_write_ensure(writer, varint_size(data));

    do {
        *writer->it = data & body_mask;
        *writer->it |= (data >>= shift) ? more_mask : 0;
        writer->it++;
    } while (data);
}

static void write_tag(struct pb_writer *writer, pb_field_t field, enum pb_wire wire)
{
    write_varint(writer, field << 3 | wire);
}

static void write_data(struct pb_writer *writer, size_t len, const void *data)
{
    pb_write_ensure(writer, len);
    memcpy(writer->it, data, len);
    writer->it += len;
}

bool pb_write_varint(struct pb_writer *writer, enum pb_type type, union pb_value value)
{
    switch (type) {
    case pb_bool:
    case pb_enum:
    case pb_64_uint: write_varint(writer, value.u64); return true;
    case pb_32_uint: write_varint(writer, value.u32); return true;
    case pb_64_sint: write_varint(writer, zigzag(value.s64)); return true;
    case pb_32_sint: write_varint(writer, zigzag(value.s32)); return true;
    default: return false;
    }
}

bool pb_write_field(
        struct pb_writer *writer,
        pb_field_t field,
        enum pb_type type,
        union pb_value value)
{
    switch (type) {

    case pb_bool:
    case pb_enum:
    case pb_32_uint:
    case pb_32_sint:
    case pb_64_uint:
    case pb_64_sint:
        write_tag(writer, field, pb_wire_varint);
        return pb_write_varint(writer, type, value);

    case pb_32_ufixed:
    case pb_32_sfixed:
    case pb_32_float:
        write_tag(writer, field, pb_wire_32);
        write_data(writer, sizeof(value.u32), &value.u32);
        return true;

    case pb_64_ufixed:
    case pb_64_sfixed:
    case pb_64_float:
        write_tag(writer, field, pb_wire_64);
        write_data(writer, sizeof(value.u64), &value.u64);
        return true;

    case pb_string:
    case pb_message:
    case pb_bytes:
        write_tag(writer, field, pb_wire_bytes);
        write_varint(writer, value.bin.end - value.bin.it);
        write_data(writer, value.bin.end - value.bin.it, value.bin.it);
        return true;

    default: return false;
    }
}
