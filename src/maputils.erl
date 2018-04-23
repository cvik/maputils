%% Erlang maps utillity functions
%%

-module(maputils).

%% API
-export([make_hierarchi/2,
         make_hierarchi/3,
         deep_get/2,
         deep_put/3,
         flatten_hierarchi/1]).

%% API ------------------------------------------------------------------------

make_hierarchi(Order, Maps) ->
    make_hierarchi(Order, Maps, dont_keep).

make_hierarchi([], Maps, _) ->
    Maps;
make_hierarchi([Key|Order], Maps, Keep) ->
    Map = split_on_key(Key, Maps, Keep),
    F = fun(InnerKey, InnerMaps, Acc) ->
            Acc#{InnerKey=>make_hierarchi(Order, InnerMaps, Keep)}
        end,
    maps:fold(F, #{}, Map).

deep_get([Key|Keys], Map) ->
    case maps:find(Key, Map) of
        {ok, InnerMap} when is_map(InnerMap) ->
            deep_get(Keys, InnerMap);
        {ok, Val} ->
            Val;
        error ->
            {error, {key_not_found, Key}}
    end;
deep_get([], Value) ->
    Value.

deep_put([Key|Keys], Value, Map) when is_map(Map) ->
    case maps:is_key(Key, Map) of
        true ->
            Map#{Key := deep_put(Keys, Value, maps:get(Key, Map))};
        false ->
            Map#{Key => deep_put(Keys, Value, #{})}
    end;
deep_put([], Value, _) ->
    Value;
deep_put(_, _, Map) ->
    exit({error, {badmap, Map}}).

%% TODO: Implement
flatten_hierarchi(Map) -> Map.

%% Internal -------------------------------------------------------------------

split_on_key(Key, Maps, Keep) ->
    F = fun(#{Key:=Val} = Map, Acc) ->
                case maps:find(Val, Acc) of
                    {ok, InnerMaps} when Keep == keep ->
                        Acc#{Val:=[Map|InnerMaps]};
                    {ok, InnerMaps} when Keep == dont_keep ->
                        Acc#{Val:=[maps:remove(Key, Map)|InnerMaps]};
                    error when Keep == keep ->
                        Acc#{Val=>[Map]};
                    error when Keep == dont_keep ->
                        Acc#{Val=>[maps:remove(Key, Map)]}
                end;
           (_, _) ->
                exit({key_not_found, Key})
        end,
    lists:foldl(F, #{}, Maps).
