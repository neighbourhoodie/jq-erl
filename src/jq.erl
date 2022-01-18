-module(jq).
-export([run/0]).
-on_load(init/0).

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end,
    io:format("---- [erl] Loading: ~p~n", [PrivDir]),
    erlang:load_nif(filename:join(PrivDir, "jq"), 0).

run() ->
    Doc = {[
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
        {<<"empty">>, [{[]}, []]},
        {<<"bools">>, [true, false, null]}
    ]},
    io:format("---- [erl] value type: ~s~n~s~n", [json_type(Doc), json_show(Doc)]),

    Programs = [
        <<".numbers">>,
        <<".numbers[]">>,
        <<".numbers[] | (. - 3) * 6">>,
        <<".limits | keys">>,
        <<".. | .atom_key? // empty">>
    ],
    lists:foreach(fun(Program) ->
        {ok, Jq} = jq_compile(Program),
        io:format("~n==== [erl] program: <<~s>>~n", [Program]),

        case jq_eval(Jq, Doc) of
            {ok, Results} ->
                lists:foreach(fun(Result) ->
                    io:format("---- [erl] jq/2 result: ~s~n~s~n",
                            [json_type(Result), json_show(Result)])
                end, Results);
            {error, Msg} ->
                io:format("---- [erl] jq/2 failed: ~s~n", [Msg])
        end
    end, Programs).

jq_compile(_) ->
    erlang:nif_error("jq NIF library not loaded").

jq_eval(_, _) ->
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
