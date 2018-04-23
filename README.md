## Maputils - Utilities for erlang maps

## Exports

```erlang
deep_get(Keys, Map) -> Value
deep_put(Keys, Value, Map) -> Map
```

## Example usage

```erlang
1> l(maputils).
ok
2> maputils:deep_get(["1", "deep_key"], #{"1" => #{"deep_key" => "deep_value"}}).
"deep_value"
3> maputils:deep_put(["1", "2", "deep_key"], deep_value, #{}).
#{"1" => #{"2" => #{"deep_key" => deep_value}}}
```

## License

Apache license version 2.0. See the LICENSE file for details.
