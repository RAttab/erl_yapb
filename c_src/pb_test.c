#include "pb.h"
#include "htable.h"

#include <stdlib.h>
#include <assert.h>

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


#define value(type, value) \
    (union pb_value) { .type = value }



void check_impl(struct htable entries)
{
    size_t len = 0;
    struct pb_buffer buffer = {0};

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

    struct htable result = {0};
    {
        struct pb_reader reader = {0};
        pb_read_init(&reader, buffer.data, len);

        while (reader.it != reader.end) {
            struct pb_tag tag = {0};
            assert(pb_read_tag(&reader, &tag));

            struct htable_ret ret = htable_get(&entries, tag.field);
            assert(ret.ok);
            struct entry *entry = (struct entry *) ret.value;

            union pb_value value = {0};
            assert(pb_read_field(&reader, tag.wire, entry->type, &value));

            assert(value.u64 == entry->value.u64);
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
}

#define check(...)                                                      \
    do {                                                                \
        struct entry *raw[] = { __VA_ARGS__ };                          \
        check_impl(make_entries_impl(raw, sizeof(raw) / sizeof(raw[0]))); \
    } while (false);

void basic_test(void)
{
    check(
            entry(0, pb_bool, value(b, 1)),
            entry(1, pb_32_sint, value(s32, -12)));
}


int main(int argc, char **argv)
{
    (void) argc, (void) argv;

    basic_test();

    return 0;
}
