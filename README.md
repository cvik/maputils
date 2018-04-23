## Maputils - Utilities for erlang maps

## Exports

```erlang
deep_get(Keys, Map) -> Value
deep_put(Keys, Value, Map) -> Map
make_hierarchi(Order, Maps) -> Map
make_hierarchi(Order, Maps, dont_keep | keep) -> Map
```

## Example usage

```erlang
1> l(maputils).
ok
2> maputils:deep_get(["1", "deep_key"], #{"1" => #{"deep_key" => "deep_value"}}).
"deep_value"
3> maputils:deep_put(["1", "2", "deep_key"], deep_value, #{}).
#{"1" => #{"2" => #{"deep_key" => deep_value}}}
4> maputils:make_hierarchi(["id", "2"],
                           [#{"id" => 1, "2" => 2, three => 3},
                            #{"id" => a, "2" => b, three => c},
                            #{"id" => x, "2" => y, three => z}]).
#{1 => #{2 => [#{three => 3}]},
  a => #{b => [#{three => c}]},
  x => #{y => [#{three => z}]}}
5> maputils:make_hierarchi(["id", "2"],
                           [#{"id" => 1, "2" => 2, three => 3},
                            #{"id" => a, "2" => b, three => c},
                            #{"id" => x, "2" => y, three => z}],
                           keep).
#{1 => #{2 => [#{three => 3,"2" => 2,"one" => 1}]},
  a => #{b => [#{three => c,"2" => b,"one" => a}]},
  x => #{y => [#{three => z,"2" => y,"one" => x}]}}
```

## License

Apache license version 2.0. See the LICENSE file for details.
