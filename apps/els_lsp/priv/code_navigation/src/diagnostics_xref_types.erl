-module(diagnostics_xref_types).

-export_type([type_a/0,
              non_existing/0
             ]).

-type type_a() :: code_navigation:non_existing().

-spec function_a(orddict:orddict()) -> code_navigation:non_existing().
function_a(_) ->
    ok.
