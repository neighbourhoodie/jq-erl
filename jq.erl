-module(jq).
-export([run/0]).
-on_load(init/0).

init() ->
    case erlang:load_nif("./jq_nif", 0) of
        {error, {Code, Msg}} ->
            io:format("---- [erl] init result: [~w]: ~s~n", [Code, Msg]);
        Else ->
            io:format("---- [erl] init result: [~w]~n", [Else])
    end.

run() ->
    Value = {[
        {<<"numbers">>, [-1, 0, 16, 32.5, 64]},
        {<<"limits">>, {[
            {<<"max_i32">>, 2147483647},
            {<<"max_u32">>, 4294967295},
            {<<"max_safe_int">>, 9007199254740991}
        ]}},
        {<<"float">>, 3.14159e256},
        {<<"nested">>, {[
            {atom_key, [<<"bin_value">>]}
        ]}},
        {<<"bools">>, [true, false, null]}
    ]},
    {ok, Program} = jq_compile(<<".numbers[1]">>),

    io:format("---- [erl] value type: ~s~n~s~n", [json_type(Value), json_show(Value)]),

    case jq_eval(Program, Value) of
        {ok, Results} ->
            io:format("---- [erl] jq/2 result:~n~s~n", [json_show(Results)]);
        {error, Msg} ->
            io:format("---- [erl] jq/2 failed: ~s~n", [Msg])
    end,

    io:format("---- [erl] jq_simple/0 result: ~w~n", [jq_simple()]).

jq_compile(_) ->
    erlang:nif_error("jq NIF library not loaded").

jq_eval(_, _) ->
    erlang:nif_error("jq NIF library not loaded").

jq_simple() ->
    erlang:nif_error("jq NIF library not loaded").

json_type({T}) when is_list(T) -> "object";
json_type(T) when is_list(T)   -> "array";
json_type(T) when is_binary(T) -> "string";
json_type(T) when is_number(T) -> "number";
json_type(true)                -> "boolean";
json_type(false)               -> "boolean";
json_type(null)                -> "null";
json_type(_)                   -> "<invalid>".

json_show(V) -> json_show(V, 2).
json_show(V, N) -> lists:flatten(json_show(V, N, 0)).

json_show({[]}, _, _) ->
    "{}";
json_show({L}, N, T) when is_list(L) ->
    Rows = lists:map(fun({Key, Value}) ->
        tab(T + N) ++ json_show(Key, N, T + N) ++ ": " ++ json_show(Value, N, T + N)
    end, L),
    Sep = io_lib:format(",~n", []),
    io_lib:format("{~n~s~n~s}", [string:join(Rows, Sep), tab(T)]);

json_show([], _, _) ->
    "[]";
json_show(L, N, T) when is_list(L) ->
    Rows = lists:map(fun(Value) ->
        tab(T + N) ++ json_show(Value, N, T + N)
    end, L),
    Sep = io_lib:format(",~n", []),
    io_lib:format("[~n~s~n~s]", [string:join(Rows, Sep), tab(T)]);

json_show(V, _, _) when is_binary(V) ->
    io_lib:format("\"~s\"", [V]);
json_show(V, _, _) when is_number(V) or is_atom(V) ->
    io_lib:format("~w", [V]);
json_show(_, _, _) ->
    "<invalid>".

tab(T) ->
    [$ || _ <- lists:seq(1, T)].
