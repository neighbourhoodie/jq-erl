/*
 * Sources:
 *
 *      - https://github.com/stedolan/jq/blob/master/src/main.c
 *      - https://github-wiki-see.page/m/stedolan/jq/wiki/C-API:-jq-program-invocation
 */

#include <jq.h>

int main(int argc, char *argv[])
{

    // to build objects structurally instead of parsing strings...
    //
    // jv doc = jv_object();
    // jv_object_set(doc, jv_string("foo"), jv_number(42));

    jv doc = jv_parse("{ \"foo\": [16, 32, 64] }");
    jv result;

    char *program = ".foo[] | . + 1";
    int jq_flags = 0;
    int fmt_flags = JV_PRINT_PRETTY | JV_PRINT_SPACE1;

    jq_state *jq = jq_init();
    jq_compile(jq, program);
    jq_start(jq, doc, jq_flags);
    result = jv_null();

    printf("---- value type: %s\n", jv_kind_name(jv_get_kind(doc)));
    jv_show(doc, fmt_flags);
    printf("\n");

    while (1) {
        jv_free(result);
        result = jq_next(jq);

        if (!jv_is_valid(result)) {
            break;
        }

        printf("---- result type: %s\n", jv_kind_name(jv_get_kind(result)));
        jv_show(result, fmt_flags);
        printf("\n");
    }

    jq_teardown(&jq);
    jv_free(result);
    jv_free(doc);
    return 0;
}
