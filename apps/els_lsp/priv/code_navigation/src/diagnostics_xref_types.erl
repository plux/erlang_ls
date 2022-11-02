-module(diagnostics_xref_types).

-export_type([type_a/0,
              non_existing/0
             ]).

-type type_a() :: maps:bad().

-spec function_a(maps:iterator()) -> maps:bad().
function_a(_) ->
    ok.
