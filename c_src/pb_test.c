#include "pb.h"
#include "utils/htable.h"

#include <stdlib.h>
#include <stdint.h>
#include <limits.h>
#include <assert.h>
#include <float.h>
#include <string.h>

#if 1
# include <stdio.h>
# define debug(fmt) fprintf(stderr, fmt)
# define debuga(fmt, ...) fprintf(stderr, fmt, __VA_ARGS__)
#else
# define debug(fmt) ((void) 0)
# define debuga(fmt, ...) ((void) 0)
#endif

struct entry
{
    pb_field_t field;
    enum pb_type type;
    union pb_value value;
};

struct entry *entry(pb_field_t field, enum pb_type type, union pb_value value)
{
    struct entry *ptr = malloc(sizeof(*ptr));
    *ptr = (struct entry) {.field = field, .type = type, .value = value};
    return ptr;
}

struct htable make_entries_impl(struct entry **entries, size_t len)
{
    struct htable htable = {0};
    htable_reserve(&htable, len);

    for (size_t i = 0; i < len; i++)
        htable_put(&htable, entries[i]->field, (uint64_t) entries[i]);

    return htable;
}

void check_impl(struct htable entries)
{
    size_t len = 0;
    struct pb_buffer buffer = {0};

    debug("\ntest: ");
    for (struct htable_bucket *bucket = htable_next(&entries, NULL);
         bucket;
         bucket = htable_next(&entries, bucket))
    {
        struct entry *entry = (struct entry *) bucket->value;
        debuga("%lu{%d,%lx} ", entry->field, entry->type, entry->value.u64);
        (void) entry;
    }
    debug("\n");

    {
        struct pb_writer writer = {0};
        pb_write_init(&writer, &buffer);

        for (struct htable_bucket *bucket = htable_next(&entries, NULL);
             bucket;
             bucket = htable_next(&entries, bucket))
        {
            struct entry *entry = (struct entry *) bucket->value;
            assert(pb_write_field(&writer, entry->field, entry->type, entry->value));
        }

        len = writer.it - buffer.data;
    }

    debug("buffer:");
    for (size_t i = 0; i < len; ++i) debuga(" %02x", buffer.data[i]);
    debug("\n");

    struct htable result = {0};
    {
        struct pb_reader reader = {0};
        pb_read_init(&reader, buffer.data, len);

        while (reader.it != reader.end) {
            struct pb_tag tag = {0};
            assert(pb_read_tag(&reader, &tag));
            debuga("tag: field=%lu, wire=%d\n", tag.field, tag.wire);

            struct htable_ret ret = htable_get(&entries, tag.field);
            assert(ret.ok);
            struct entry *entry = (struct entry *) ret.value;

            union pb_value value = {0};
            assert(pb_read_field(&reader, tag.wire, entry->type, &value));

            if (tag.wire != pb_wire_bytes)
                assert(value.u64 == entry->value.u64);
            else {
                size_t len = entry->value.bin.end - entry->value.bin.it;
                assert((value.bin.end - value.bin.it) == (ssize_t) len);
                assert(!memcmp(value.bin.it, entry->value.bin.it, len));
            }

            assert(htable_put(&result, entry->field, 0).ok);
        }
    }

    for (struct htable_bucket *bucket = htable_next(&result, NULL);
         bucket;
         bucket = htable_next(&result, bucket))
    {
        assert(htable_get(&entries, bucket->key).ok);
    }

    for (struct htable_bucket *bucket = htable_next(&entries, NULL);
         bucket;
         bucket = htable_next(&entries, bucket))
    {
        assert(htable_get(&result, bucket->key).ok);
        free((struct entry *) bucket->value);
    }

    htable_reset(&result);
    htable_reset(&entries);
    pb_buffer_free(&buffer);
}


#define check(...)                                                      \
    do {                                                                \
        struct entry *raw[] = { __VA_ARGS__ };                          \
        check_impl(make_entries_impl(raw, sizeof(raw) / sizeof(raw[0]))); \
    } while (false);


#define value(type, value) \
    (union pb_value) { .type = value }

#define value_str(str)                                  \
    (union pb_value) {                                  \
        .bin = (struct pb_reader) {                     \
            .it = (const uint8_t *) str,                \
            .end = (const uint8_t *) str + sizeof(str)  \
        }                                               \
    }


void type_test(void)
{
    check(entry(01, pb_bool, value(b, true)));
    check(entry(02, pb_bool, value(b, false)));
    check(entry(03, pb_enum, value(u64, 0)));
    check(entry(04, pb_enum, value(u64, 1)));

    check(entry(10, pb_32_uint, value(u32, 0)));
    check(entry(11, pb_32_uint, value(u32, UINT32_MAX)));
    check(entry(12, pb_32_sint, value(s32, INT32_MIN)));
    check(entry(13, pb_32_sint, value(s32, 0)));
    check(entry(14, pb_32_sint, value(s32, INT32_MAX)));

    check(entry(20, pb_32_ufixed, value(u32, 0)));
    check(entry(21, pb_32_ufixed, value(u32, UINT32_MAX)));
    check(entry(22, pb_32_sfixed, value(s32, INT32_MIN)));
    check(entry(23, pb_32_sfixed, value(s32, 0)));
    check(entry(24, pb_32_sfixed, value(s32, INT32_MAX)));

    check(entry(30, pb_64_uint, value(u64, 0)));
    check(entry(31, pb_64_uint, value(u64, UINT64_MAX)));
    check(entry(32, pb_64_sint, value(s64, INT64_MIN)));
    check(entry(33, pb_64_sint, value(s64, 0)));
    check(entry(34, pb_64_sint, value(s64, INT64_MAX)));

    check(entry(40, pb_64_ufixed, value(u64, 0)));
    check(entry(41, pb_64_ufixed, value(u64, UINT64_MAX)));
    check(entry(42, pb_64_sfixed, value(s64, INT64_MIN)));
    check(entry(43, pb_64_sfixed, value(s64, 0)));
    check(entry(44, pb_64_sfixed, value(s64, INT64_MAX)));

    check(entry(50, pb_32_float, value(f32, 0.0)));
    check(entry(51, pb_32_float, value(f32, FLT_MIN)));
    check(entry(52, pb_32_float, value(f32, FLT_TRUE_MIN)));
    check(entry(53, pb_32_float, value(f32, FLT_MAX)));
    check(entry(54, pb_32_float, value(f32, FLT_EPSILON)));

    check(entry(60, pb_64_float, value(f64, 0.0)));
    check(entry(61, pb_64_float, value(f64, DBL_MIN)));
    check(entry(62, pb_64_float, value(f64, DBL_TRUE_MIN)));
    check(entry(63, pb_64_float, value(f64, DBL_MAX)));
    check(entry(64, pb_64_float, value(f64, DBL_EPSILON)));

    check(entry(70, pb_string, value_str("")));
    check(entry(71, pb_string, value_str("a")));
    check(entry(72, pb_string, value_str("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")));
}


int main(int argc, char **argv)
{
    (void) argc, (void) argv;

    type_test();

    return 0;
}
