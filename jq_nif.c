#include <erl_nif.h>
#include <jq.h>
#include <stdlib.h>
#include <string.h>

//------------------------------------------------------------------------------
// Erlang term helpers
//------------------------------------------------------------------------------

static ERL_NIF_TERM ok(ErlNifEnv *env, ERL_NIF_TERM value)
{
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), value);
}

static ERL_NIF_TERM error(ErlNifEnv *env, char *message)
{
    return enif_make_tuple2(env,
            enif_make_atom(env, "error"),
            enif_make_string(env, message, ERL_NIF_LATIN1));
}

//------------------------------------------------------------------------------
// Conversions between Erlang and native values
//------------------------------------------------------------------------------

static char *binary_to_cstr(ErlNifEnv *env, ERL_NIF_TERM term)
{
    ErlNifBinary binary;
    char *str = NULL;

    if (enif_inspect_binary(env, term, &binary)) {
        str = malloc(binary.size + 1);
        memcpy(str, binary.data, binary.size);
        str[binary.size] = '\0';
    }
    return str;
}

//------------------------------------------------------------------------------
// jq/2
//------------------------------------------------------------------------------

static ERL_NIF_TERM jq_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    char *program = NULL;
    char value[1024];
    jq_state *jq = jq_init();
    jv doc = jv_null();
    jv result = jv_null();
    int jq_flags = 0;
    int fmt_flags = JV_PRINT_PRETTY | JV_PRINT_SPACE2;
    ERL_NIF_TERM ret;

    program = binary_to_cstr(env, argv[0]);

    if (program == NULL) {
        ret = error(env, "failed to transfer jq program");
        goto cleanup;
    }

    if (!enif_get_string(env, argv[1], value, sizeof(value), ERL_NIF_LATIN1)) {
        ret = error(env, "failed to transfer JSON value");
        goto cleanup;
    }

    doc = jv_parse(value);

    printf("---- [c] converted jv value:\n");
    jv_show(doc, fmt_flags);
    printf("\n");

    // jq_compile(jq, program);
    // jq_start(jq, doc, jq_flags);

    // result = jv_dump_string(jq_next(jq), fmt_flags);
    // const char *res = jv_string_value(result);

    // ret = enif_make_int(env, strlen(program));
    ret = ok(env, enif_make_string(env, program, ERL_NIF_LATIN1));

cleanup:
    free(program);
    jv_free(doc);
    jv_free(result);
    jq_teardown(&jq);

    return ret;
}

//------------------------------------------------------------------------------
// jq_simple/0
//------------------------------------------------------------------------------

static ERL_NIF_TERM jq_simple_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    jq_state *jq = jq_init();
    // jq_compile(jq, ".foo");
    return enif_make_int(env, 42);
}

//------------------------------------------------------------------------------
// NIF setup boilerplate
//------------------------------------------------------------------------------

static ErlNifFunc nif_funcs[] = {
    {"jq", 2, jq_nif, 0},
    {"jq_simple", 0, jq_simple_nif, 0}
};

ERL_NIF_INIT(jq, nif_funcs, NULL, NULL, NULL, NULL);
