#include <erl_nif.h>
#include <jq.h>
#include <stdlib.h>
#include <string.h>

static ERL_NIF_TERM jq_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    char program[256];
    char value[1024];
    jq_state *jq = jq_init();
    jv json_value = jv_null();
    jv result = jv_null();
    int jq_flags = 0;
    int fmt_flags = 0;
    ERL_NIF_TERM ret;

    enif_get_string(env, argv[0], program, sizeof(program), ERL_NIF_LATIN1);
    enif_get_string(env, argv[1], value, sizeof(value), ERL_NIF_LATIN1);

    json_value = jv_parse(value);

    // jq_compile(jq, program);
    // jq_start(jq, json_value, jq_flags);

    // result = jv_dump_string(jq_next(jq), fmt_flags);
    // const char *res = jv_string_value(result);

    // ret = enif_make_int(env, strlen(program));
    ret = enif_make_string(env, program, ERL_NIF_LATIN1);

    // jv_free(json_value);
    // jv_free(result);
    // jq_teardown(&jq);

    return ret;
}

static ERL_NIF_TERM jq_simple_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    jq_state *jq = jq_init();
    // jq_compile(jq, ".foo");
    return enif_make_int(env, 42);
}

static ErlNifFunc nif_funcs[] = {
    {"jq", 2, jq_nif, 0},
    {"jq_simple", 0, jq_simple_nif, 0}
};

ERL_NIF_INIT(jq, nif_funcs, NULL, NULL, NULL, NULL);
