#include "pb.h"


void pb_read_init(struct pb_reader *reader, uint8_t *data, size_t len)
{
    *reader = (struct pb_reader) {
        .it = data,
        .end = data + len;
    };
}

static bool read_data(struct pb_reader *reader, size_t len, void *dst)
{
    if (reader->it + len > reader->len) return false;
    memcpy(dst, reader->data, len);
    reader->it += len;
}

static bool read_bin(struct pb_reader *reader, union pb_field *field)
{
    size_t len = 0;
    if (!reader_read_varint(reader, &bin->len)) return false;
    if (reader->it + len > reader->end) return false;

    field->bin = (struct pb_reader) {
        .it = reader->it,
        .end = reader->it + len,
    };
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

static uint64_t zigzag(int64_t value)
{
    return (data << 1) ^ (value >> 63);
}

static int64_t zagzig(uint64_t value)
{
    return (data >> 1) ^ -(data & 0x1);
}

bool pb_read_tag(struct pb_reader *reader, struct pb_tag *tag)
{
    uint32_t data = 0;
    if (!reader_read_varint(reader, &data)) return false;

    *tag = (struct pb_tag) {
        .field = data >> 3,
        .wire = data & 0x7
    };
    return true;
}

bool pb_read_varint(struct pb_reader *reader, enum pb_type type, union pb_field *field)
{
    switch (type) {

    case pb_32_uint:
        if (!read_varint(reader, &field->u32)) return false;
        return field->u32 <= UINT32_MAX;

    case pb_bool:
    case pb_enum:
    case pb_64_uint:
        return read_varint(reader, &field->u64);

    case pb_32_sint:
    {
        if (!read_varint(reader, &field->u64)) return false;
        int64_t data = zagzig(field->u64);
        if (data > INT32_MAX || sdata < INT32_MIN) return false;
        field->s32 = data;
        return true;
    }

    case pb_64_sint:
        if (!read_varint(reader, &field->u64)) return false;
        field->s64 = zagzig(field->u64);
        return true;

    default: return false;
    }
}

bool pb_read_field(
        struct pb_reader *reader,
        enum pb_wire wire,
        enum pb_type type,
        union pb_field *field)
{
    switch (wire) {

    case pb_wire_varint:
        return pb_read_varint(reader, type, field);

    case pb_wire_32:
        switch (type) {
        case pb_32_ufixed:
        case pb_32_sfixed:
        case pb_32_float: return read_data(reader, sizeof(uint32_t), &field->u32);
        default: return false;
        }

    case pb_wire_64:
        switch (type) {
        case pb_64_ufixed:
        case pb_64_sfixed:
        case pb_64_float: return read_data(reader, sizeof(uint64_t), &field->u64);
        default: return false;
        }

    case pb_wire_data:
        return read_bin(reader, field);

    default: return false;
    }
}
