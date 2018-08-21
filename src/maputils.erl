%% Erlang maps utility functions
%%

-module(maputils).

%% API
-export([make_hierarchy/2,
         make_hierarchy/3,
         deep_get/2,
         deep_put/3,
         append/3,
         inc/2,
         inc/3,
         merge_with/3]).

-type key() :: any().

%% API ------------------------------------------------------------------------

-spec make_hierarchy([key()], map()) -> map()
                                      | {error, {key_not_found, key()}}.
make_hierarchy(Order, Maps) ->
    make_hierarchy(Order, Maps, dont_keep).

-spec make_hierarchy([key()], map(), keep|dont_keep) -> map()
                                                      | {error, {key_not_found, key()}}.
make_hierarchy([], Maps, _) ->
    Maps;
make_hierarchy([Key|Order], Maps, Keep) ->
    Map = split_on_key(Key, Maps, Keep),
    F = fun(InnerKey, InnerMaps, Acc) ->
            Acc#{InnerKey=>make_hierarchy(Order, InnerMaps, Keep)}
        end,
    maps:fold(F, #{}, Map).

-spec deep_get([key()], map()) -> any()
                                | {error, {key_not_found, any()}}.
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

-spec deep_put([key()], any(), map()) -> map().
deep_put([Key|Keys], Value, Map) ->
    case maps:find(Key, Map) of
        {ok, InnerMap} ->
            Map#{Key := deep_put(Keys, Value, InnerMap)};
        error ->
            Map#{Key => deep_put(Keys, Value, #{})}
    end;
deep_put([], Value, _) ->
    Value.

-spec append(key(), any(), map()) -> map().
append(Key, Value, Map) ->
    case maps:find(Key, Map) of
        {ok, PrevValue} ->
            Map#{Key:=[Value|PrevValue]};
        error ->
            Map#{Key=>[Value]}
    end.

-spec inc(key(), map()) -> map() | {error, badarg}.
inc(Key, Map) ->
    inc(Key, 1, Map).

-spec inc(key(), number(), map()) -> map() | {error, badarg}.
inc(Key, Amount, Map) ->
    case maps:find(Key, Map) of
        {ok, Counter} when is_number(Counter) ->
            Map#{Key:=Counter+Amount};
        {ok, _} ->
            {error, badarg};
        error ->
            Map#{Key=>Amount}
    end.

-spec merge_with(fun((key(), any(), any()) -> any()), map(), map())
                -> map() | {error, badarg}.
merge_with(Fun, Map1, Map2) when is_function(Fun, 3), is_map(Map1), is_map(Map2) ->
    maps:fold(fun (K, V1, Acc) ->
                      case maps:find(K, Acc) of
                          {ok, V2} ->
                              maps:update(K, Fun(K, V1, V2), Acc);
                          error ->
                              maps:put(K, V1, Acc)
                      end
              end,
              Map1, Map2);
merge_with(_, _, _) ->
    {error, badarg}.


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
