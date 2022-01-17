#include <erl_nif.h>
#include <jq.h>
#include <stdlib.h>
#include <string.h>

//------------------------------------------------------------------------------
// Resource declarations
//------------------------------------------------------------------------------
//
// To let us reuse a compiled jq program to evaluate on multiple input
// documents, we need to create a resource term to hold onto the `jq_state *`
// returned by `jq_init()`, and pass this pointer to `jq_teardown()` when
// Erlang releases the term.
//
// The value stored in this resource is a `jq_state *` pointer, so pointers
// into the resource's memory block are `jq_state **`.

static ErlNifResourceType *jq_resource;

static void jq_resource_destroy(ErlNifEnv *env, void *obj)
{
    printf("---- [c] jq_resource_destroy()\n");
    jq_teardown((jq_state **)obj);
}

static int on_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    jq_resource = enif_open_resource_type(env, "jq", "jq_resource",
            jq_resource_destroy,
            ERL_NIF_RT_CREATE,
            NULL);

    return 0;
}

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
// jq_compile/1
//------------------------------------------------------------------------------
//
// This compiles a jq program passed as an Erlang binary into a `jq_state`
// struct, and we store a pointer to this in a resource term that we return for
// later use with jq_eval/2.

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

static ERL_NIF_TERM jq_compile_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    char *program = binary_to_cstr(env, argv[0]);
    jq_state **jq_ptr = NULL;
    ERL_NIF_TERM ret;

    if (program == NULL) {
        return error(env, "failed to transfer jq program");
    }

    printf("---- [c] jq_compile: <<%s>>\n", program);

    jq_ptr = enif_alloc_resource(jq_resource, sizeof(jq_state *));
    *jq_ptr = jq_init();

    if (1 /* jq_compile(*jq_ptr, program) */) {
        ret = ok(env, enif_make_resource(env, jq_ptr));
    } else {
        ret = error(env, "failed to compile jq program");
    }

    enif_release_resource(jq_ptr);
    free(program);
    return ret;
}

//------------------------------------------------------------------------------
// Conversions between Erlang and native values
//------------------------------------------------------------------------------
//
// These functions convert between the Erlang representation of JSON values and
// the equivalent 'jv' values used by jq. The following schema is enforced on
// the Erlang representation:
//
// - Objects are represented as {[ {Key, Value}* ]} i.e. a 1-tuple containing a
//   list of pairs. Keys must be either atoms or binaries, both of which are
//   converted to strings. Values are any valid JSON value.
//
// - Arrays are represented as [Value*], i.e. a list of zero or more values.
//
// - Strings are represented as binaries, not Erlang strings. They're
//   transferred byte-for-byte into jv strings, which should be fine if the input
//   was UTF-8.
//
// - Because Erlang strings are just lists, they will be converted to JSON
//   arrays. An Erlang string used as an object key will cause conversion to
//   fail.
//
// - Erlang integers and floats are converted to jv numbers. The
//   `enif_get_int64()` function is used so that values up to the IEEE 754 max
//   safe int value of 2^53 - 1 are preserved. On conversion from jv, integers
//   with values less than 2^31 become Erlang ints, all other values become
//   floats.
//
// - The values `true`, `false` and `null` are represented by the atoms of the
//   same name.
//
// An input value not obeying these rules will cause the conversion to fail,
// indicated by the function returning 0. It may have partially written the
// converted value on failure, in which case this value must not be used.

static int term_is_key(ErlNifEnv *env, ERL_NIF_TERM term)
{
    return enif_is_atom(env, term) || enif_is_binary(env, term);
}

static int erl_to_jv(ErlNifEnv *env, ERL_NIF_TERM term, jv *out, int is_key)
{
    jv key, value;
    int tuple_size = 0;
    unsigned int list_size = 0, i = 0;
    const ERL_NIF_TERM *items = NULL;
    ERL_NIF_TERM list, head;
    ErlNifBinary binary;
    ErlNifSInt64 num_int;
    double num_float = 0;
    char atom[256];

    if (enif_get_tuple(env, term, &tuple_size, &items)) {
        if (tuple_size != 1 || !enif_is_list(env, items[0])) {
            return 0;
        }

        *out = jv_object();
        list = items[0];

        while (enif_get_list_cell(env, list, &head, &list)) {
            if (!enif_get_tuple(env, head, &tuple_size, &items)) {
                return 0;
            }
            if (tuple_size != 2 || !term_is_key(env, items[0])) {
                return 0;
            }
            if (erl_to_jv(env, items[0], &key, 1) && erl_to_jv(env, items[1], &value, 0)) {
                *out = jv_object_set(*out, key, value);
            } else {
                return 0;
            }
        }
    } else if (enif_get_list_length(env, term, &list_size)) {
        *out = jv_array_sized(list_size);
        list = term;
        i = 0;

        while (enif_get_list_cell(env, list, &head, &list)) {
            if (erl_to_jv(env, head, &value, 0)) {
                *out = jv_array_set(*out, i++, value);
            } else {
                return 0;
            }
        }
    } else if (enif_inspect_binary(env, term, &binary)) {
        *out = jv_string_sized((const char *)binary.data, binary.size);

    } else if (enif_get_int64(env, term, &num_int)) {
        *out = jv_number(num_int);

    } else if (enif_get_double(env, term, &num_float)) {
        *out = jv_number(num_float);

    } else if (enif_get_atom(env, term, atom, sizeof(atom), ERL_NIF_LATIN1)) {
        if (is_key) {
            if (enif_get_atom_length(env, term, &i, ERL_NIF_LATIN1)) {
                *out = jv_string_sized(atom, i);
            } else {
                return 0;
            }
        } else if (!strcmp(atom, "true")) {
            *out = jv_true();
        } else if (!strcmp(atom, "false")) {
            *out = jv_false();
        } else if (!strcmp(atom, "null")) {
            *out = jv_null();
        } else {
            return 0;
        }
    } else {
        return 0;
    }

    return 1;
}

static int jv_to_erl(ErlNifEnv *env, jv json, ERL_NIF_TERM *out)
{
    int size = 0, i = 0;
    ERL_NIF_TERM *list = NULL, key, value;
    unsigned char *buf;

    switch (jv_get_kind(json)) {
        case JV_KIND_OBJECT:
            size = jv_object_length(jv_copy(json));
            list = calloc(size, sizeof(ERL_NIF_TERM));
            i = 0;

            jv_object_foreach(json, jv_key, jv_value) {
                if (jv_to_erl(env, jv_key, &key) && jv_to_erl(env, jv_value, &value)) {
                    list[i++] = enif_make_tuple2(env, key, value);
                } else {
                    free(list);
                    return 0;
                }
            }
            *out = enif_make_tuple1(env, enif_make_list_from_array(env, list, size));
            free(list);
            break;

        case JV_KIND_ARRAY:
            size = jv_array_length(jv_copy(json));
            list = calloc(size, sizeof(ERL_NIF_TERM));

            jv_array_foreach(json, idx, jv_value) {
                if (jv_to_erl(env, jv_value, &value)) {
                    list[idx] = value;
                } else {
                    free(list);
                    return 0;
                }
            }
            *out = enif_make_list_from_array(env, list, size);
            free(list);
            break;

        case JV_KIND_STRING:
            size = jv_string_length_bytes(json);
            buf = enif_make_new_binary(env, size, out);
            memcpy(buf, jv_string_value(json), size);
            break;

        case JV_KIND_NUMBER:
            if (jv_is_integer(json)) {
                *out = enif_make_int64(env, jv_number_value(json));
            } else {
                *out = enif_make_double(env, jv_number_value(json));
            }
            break;

        case JV_KIND_TRUE:
            *out = enif_make_atom(env, "true");
            break;

        case JV_KIND_FALSE:
            *out = enif_make_atom(env, "false");
            break;

        case JV_KIND_NULL:
            *out = enif_make_atom(env, "null");
            break;

        default:
            return 0;
    }

    return 1;
}

//------------------------------------------------------------------------------
// jq_eval/2
//------------------------------------------------------------------------------

static ERL_NIF_TERM jq_eval_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    jq_state **jq_ptr = NULL;
    jv doc, result = jv_null();
    int jq_flags = 0;
    int fmt_flags = JV_PRINT_PRETTY | JV_PRINT_SPACE2;
    ERL_NIF_TERM ret, item;

    if (!erl_to_jv(env, argv[1], &doc, 0)) {
        ret = error(env, "failed to convert Erlang JSON value");
        goto cleanup;
    }

    //----------------------------------------------------------------
    // round-trip test implementation
    //----------------------------------------------------------------

    printf("---- [c] converted jv value:\n");
    jv_show(doc, fmt_flags);
    printf("\n");

    if (jv_to_erl(env, doc, &ret)) {
        ret = ok(env, enif_make_list1(env, ret));
    } else {
        ret = error(env, "failed to convert jv JSON value");
    }
    goto cleanup;

    //----------------------------------------------------------------
    // jq implementation
    //----------------------------------------------------------------

    if (!enif_get_resource(env, argv[0], jq_resource, (void **)&jq_ptr)) {
        ret = error(env, "failed to read compiled jq program");
        goto cleanup;
    }

    jq_start(*jq_ptr, doc, jq_flags);
    ret = enif_make_list(env, 0);

    while (1) {
        jv_free(result);
        result = jq_next(*jq_ptr);

        if (!jv_is_valid(result)) {
            break;
        } else if (jv_to_erl(env, result, &item)) {
            ret = enif_make_list_cell(env, item, ret);
        } else {
            ret = error(env, "failed to convert jv JSON value");
            goto cleanup;
        }
    }
    ret = ok(env, ret);

cleanup:
    jv_free(doc);
    jv_free(result);

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
    {"jq_compile", 1, jq_compile_nif, 0},
    {"jq_eval", 2, jq_eval_nif, 0},
    {"jq_simple", 0, jq_simple_nif, 0}
};

ERL_NIF_INIT(jq, nif_funcs, on_load, NULL, NULL, NULL);
