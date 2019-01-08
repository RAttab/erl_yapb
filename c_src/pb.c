#include "pb.h"



bool pb_init(struct pb_codec *codec)
{
    enum { default_cap = 128 };
    
    *codec = (struct pb_codec) {0};
    codec->bin = (struct pb_bin) {
        .len = 0,
        .cap = default_cap,
        .data = malloc(default_cap)
    };
}

bool pb_reset(struct pb_codec *codec, uint8_t *data, size_t len)
{
    codec->it = data;
    codec->end = codec->it += len;
}

bool pb_free(struct pb_codec *codec)
{
    free(codec->bin.data);
}

static inline bool codec_read(struct pb_codec *codec, size_t len, void *dst)
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

static bool pb_read_varint_u(struct pb_codec *codec, size_t len, union pb_field *field)
{
    uint64_t data;
    if (!codec_read_varint(codec, &data)) return false;

    if (len == sizeof(uint32_t)) {
        if (data > UINT32_MAX) return false;
        field->u32 = data;
        return true;
    }
    else if (len == sizeof(uin64_t)) {
        field->u64 = data;
        return true;
    }
    else return false;
}

static bool pb_read_varint_s(struct pb_codec *codec, size_t len, union pb_field *field)
{
    uint64_t data;
    if (!codec_read_varint(codec, &data)) return false;

    int64_t sdata = (data >> 1) ^ -(data & 0x1);
    
    if (len == sizeof(int32_t)) {
        if (sdata > INT32_MAX || sdata < INT32_MIN) return false;
        field->s32 = sdata;
        return true;
    }
    else if (len == sizeof(in64_t)) {
        field->s64 = sdata;
        return true;
    }
    else return false;
}

static bool pb_read_bin(struct pb_codec *codec, union pb_field *field)
{
    struct pb_bin *bin = &codec->bufffer;
    field->bin = bin;
    
    if (!codec_read_varint(codec, &bin->len)) return false;
    if (bin->cap < bin->len) bin->data = realloc(bin->data, bin->len);
    return codec_read(codec, bin->len, bin->data);
}

bool pb_read_field(
        struct pb_codec *codec,
        enum pb_wire wire,
        enum pb_type type,
        union pb_field *field)
{
    switch (wire) {
        
    case pb_wire_varint:
        switch (type) {
        case pb_32_uint: return pb_read_varing_u(codec, sizeof(uin32_t), field);
        case pb_32_int:
        case pb_32_sint: return pb_read_varing_s(codec, sizeof(in32_t), field);

        case pb_64_uint: return pb_read_varing_u(codec, sizeof(uin64_t), field);
        case pb_64_int:
        case pb_64_sint: return pb_read_varing_s(codec, sizeof(in64_t), field);

        default: return false;
        }
        
    case pb_wire_32:
        switch (type) {
        case pb_32_fixed: 
        case pb_32_sfixed:
        case pb_32_float: return codec_read(codec, sizeof(uint32_t), &field->u32);
        default: return false;
        }
        
    case pb_wire_64:
        switch (type) {
        case pb_64_fixed: 
        case pb_64_sfixed:
        case pb_64_float: return codec_read(codec, sizeof(uint64_t), &field->u64);
        default: return false;
        }
        
    case pb_wire_data:
        return pb_read_bin(codec, field);

    default: return false;
    }
}


