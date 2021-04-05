-module(rename_record).
-record(record_a, {field_a :: integer(), field_b}).
-type record_a() :: #record_a{}.

-spec function_a() -> record_a().
function_a() ->
  Rec = #record_a{field_a = 1},
  { Rec#record_a{field_a = 2}
  , Rec#record_a.field_a
  , #record_a.field_b
  }.
