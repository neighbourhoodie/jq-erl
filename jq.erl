-module(jq).
-export([run/0]).
-on_load(init/0).

init() ->
    case erlang:load_nif("./jq_nif", 0) of
        {error, {Code, Msg}} ->
            io:format("---- init result: [~w]: ~s~n", [Code, Msg]);
        Else ->
            io:format("---- init result: [~w]~n", [Else])
    end.

run() ->
    Program = ".foo[0]",
    Value = {[
        {<<"foo">>, [16, 32, 64]}
    ]},
    JsonValue = json_show(Value),
    io:format("---- value type: ~s~n", [json_type(Value)]),
    io:format("~s~n", [JsonValue]),
    io:format("---- result: ~w~n", [jq(Program, JsonValue)]),
    io:format("---- result: ~w~n", [jq_simple()]).

jq(_, _) ->
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
