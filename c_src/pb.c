#include "pb.h"


void pb_reset(struct pb_codec *codec, uint8_t *data, size_t len)
{
    *codec = (struct pb_codec) {
        .it = data,
        .end = data + len;
    };
}

static bool codec_read(struct pb_codec *codec, size_t len, void *dst)
{
    if (codec->it + len > codec->len) return false;
    memcpy(dst, codec->data, len);
    codec->it += len;
}

static bool codec_read_varint(struct pb_codec *codec, uint64_t *data)
{
    static const size_t shift = 7;
    static const uint64_t more_mask = 1UL << shift;
    static const uint64_t body_mask = (1UL << shift) - 1;

    if (codec->it == codec->end) return false;

    uint8_t byte;
    size_t pos = 0;
    *data = 0;

    do {
        byte = *codec->it; codec->it++;
        *data |= (byte & body_mask) << pos;
        pos += shift;
    } while ((byte & more_mask) && codec->it != codec->end);

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

bool pb_read_tag(struct pb_codec *codec, struct pb_tag *tag)
{
    uint32_t data = 0;
    if (!codec_read_varint(codec, &data)) return false;

    *tag = (struct pb_tag) {
        .field = data >> 3,
        .wire = data & 0x7
    };
    return true;
}

bool pb_read_varint(struct pb_codec *codec, enum pb_type type, union pb_field *field)
{
    switch (type) {

    case pb_32_uint:
        if (!codec_read_varint(codec, &field->u32)) return false;
        return field->u32 <= UINT32_MAX;

    case pb_64_uint:
        return codec_read_varint(codec, &field->u64);

    case pb_32_sint:
    {
        if (!codec_read_varint(codec, &field->u64)) return false;
        int64_t data = zagzig(field->u64);
        if (data > INT32_MAX || sdata < INT32_MIN) return false;
        field->s32 = data;
        return true;
    }

    case pb_64_sint:
        if (!codec_read_varint(codec, &field->u64)) return false;
        field->s64 = zagzig(field->u64);
        return true;

    default: return false;
    }
}


static bool pb_read_bin(struct pb_codec *codec, union pb_field *field)
{
    size_t len = 0;
    if (!codec_read_varint(codec, &bin->len)) return false;
    if (codec->it + len > codec->end) return false;

    field->bin = (struct pb_codec) {
        .it = codec->it,
        .end = codec->it + len,
    };
    return true;
}

bool pb_read_field(
        struct pb_codec *codec,
        enum pb_wire wire,
        enum pb_type type,
        union pb_field *field)
{
    switch (wire) {

    case pb_wire_varint:
        return pb_read_varint(codec, type, field);

    case pb_wire_32:
        switch (type) {
        case pb_32_ufixed:
        case pb_32_sfixed:
        case pb_32_float: return codec_read(codec, sizeof(uint32_t), &field->u32);
        default: return false;
        }

    case pb_wire_64:
        switch (type) {
        case pb_64_ufixed:
        case pb_64_sfixed:
        case pb_64_float: return codec_read(codec, sizeof(uint64_t), &field->u64);
        default: return false;
        }

    case pb_wire_data:
        return pb_read_bin(codec, field);

    default: return false;
    }
}
