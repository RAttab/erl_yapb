#include "utils/htable.h"
#include "cache.h"

void initialize_cache(struct htable *htable, int len)
{
    htable_reserve(htable, len);
}

const struct pb_field_cache *find_field(const struct pb_message *message, int field_id)
{
    for (int i = 0; i < message->count; i++) {
        if (message->fields[i].fnum == field_id) { return &message->fields[i]; }
    }
    return NULL;
}
