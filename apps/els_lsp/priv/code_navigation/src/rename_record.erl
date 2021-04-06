-module(rename_record).
-record(record_foo, {field_a :: integer(), field_b}).

function_a(X) ->
  X = #record_foo{field_a = 42},
  Y = X#record_foo.field_a,
  #record_foo.field_b.
