%% Erlang maps utillity functions
%%

-module(maputils).

%% API
-export([make_hierarchy/2,
         make_hierarchy/3,
         deep_get/2,
         deep_put/3,
         flatten_hierarchi/1]).

%% API ------------------------------------------------------------------------

make_hierarchy(Order, Maps) ->
    make_hierarchy(Order, Maps, dont_keep).

make_hierarchy([], Maps, _) ->
    Maps;
make_hierarchy([Key|Order], Maps, Keep) ->
    Map = split_on_key(Key, Maps, Keep),
    F = fun(InnerKey, InnerMaps, Acc) ->
            Acc#{InnerKey=>make_hierarchy(Order, InnerMaps, Keep)}
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

deep_put([Key|Keys], Value, Map) ->
    case maps:find(Key, Map) of
        {ok, InnerMap} ->
            Map#{Key := deep_put(Keys, Value, InnerMap)};
        error ->
            Map#{Key => deep_put(Keys, Value, #{})}
    end;
deep_put([], Value, _) ->
    Value.

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
