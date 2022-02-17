-module(els_code_action_provider).

-behaviour(els_provider).

-export([ handle_request/2
        , is_enabled/0
        ]).

-include("els_lsp.hrl").

-type state() :: any().

%%==============================================================================
%% els_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() -> true.

-spec handle_request(any(), state()) -> {any(), state()}.
handle_request({document_codeaction, Params}, State) ->
  #{ <<"textDocument">> := #{ <<"uri">> := Uri}
   , <<"range">>        := RangeLSP
   , <<"context">>      := Context } = Params,
  Result = code_actions(Uri, RangeLSP, Context),
  {Result, State}.

%%==============================================================================
%% Internal Functions
%%==============================================================================


%% @doc Result: `(Command | CodeAction)[] | null'
-spec code_actions(uri(), range(), map()) -> [map()].
code_actions(Uri, _Range, #{<<"diagnostics">> := Diagnostics}) ->
  lists:flatten([make_code_action(Uri, D) || D <- Diagnostics]).

-spec replace_range_action(uri(), binary(), binary(), binary(), range())
                          -> map().
replace_range_action(Uri, Title, Kind, Text, Range) ->
  #{ title => Title
   , kind => Kind
   , command =>
       els_command:make_command( Title
                               , <<"replace-range">>
                               , [#{ uri   => Uri
                                   , text  => Text
                                   , range => Range
                                   }])
   }.

-spec make_code_action(uri(), map()) -> [map()].
make_code_action(Uri, #{ <<"message">> := Message
                       , <<"range">>   := Range } = _Diagnostic) ->
  make_code_action(
    [ {"function (.*) is unused", fun action_export_function/3}
    , {"variable '(.*)' is unused", fun action_ignore_variable/3}
    , {"variable '(.*)' is unbound", fun action_suggest_variable/3}
    , {"Module name '.*' does not match file name '(.*)'",
       fun action_fix_module_name/3}
    ], Uri, Range, Message).

-spec make_code_action([{string(), Fun}], uri(), range(), binary()) -> [map()]
          when Fun :: fun((uri(), range(), [binary()]) -> [map()]).
make_code_action([], _Uri, _Range, _Message) ->
  [];
make_code_action([{RE, Fun}|Rest], Uri, Range, Message) ->
  Actions = case re:run(Message, RE, [{capture, all_but_first, binary}]) of
              {match, Matches} ->
                Fun(Uri, Range, Matches);
              nomatch ->
                []
            end,
  Actions ++ make_code_action(Rest, Uri, Range, Message).

-spec action_export_function(uri(), range(), [binary()]) -> [map()].
action_export_function(Uri, _Range, [UnusedFun]) ->
  {ok, Document} = els_utils:lookup_document(Uri),
  case els_poi:sort(els_dt_document:pois(Document, [module, export])) of
    [] ->
      [];
    POIs ->
      #{range := #{to := {Line, _Col}}} = lists:last(POIs),
      Pos = {Line + 1, 1},
      [ replace_range_action( Uri
                            , <<"Export ", UnusedFun/binary>>
                            , ?CODE_ACTION_KIND_QUICKFIX
                            , <<"-export([", UnusedFun/binary, "]).\n">>
                            , els_protocol:range(#{from => Pos, to => Pos}))]
  end.

-spec action_ignore_variable(uri(), range(), [binary()]) -> [map()].
action_ignore_variable(Uri, Range, [UnusedVariable]) ->
  {ok, Document} = els_utils:lookup_document(Uri),
  POIs = els_poi:sort(els_dt_document:pois(Document, [variable])),
  VarRange = var_range(els_range:to_poi_range(Range), UnusedVariable, POIs),
  [ replace_range_action( Uri
                        , <<"Add '_' to '", UnusedVariable/binary, "'">>
                        , ?CODE_ACTION_KIND_QUICKFIX
                        , <<"_", UnusedVariable/binary>>
                        , els_protocol:range(VarRange))].

-spec action_suggest_variable(uri(), range(), [binary()]) -> [map()].
action_suggest_variable(Uri, Range, [Var]) ->
  %% Supply a quickfix to replace an unbound variable with the most similar
  %% variable name in scope.
  {ok, Document} = els_utils:lookup_document(Uri),
  POIs = els_poi:sort(els_dt_document:pois(Document, [variable])),
  VarRange = var_range(els_range:to_poi_range(Range), Var, POIs),
  ScopeRange = els_scope:variable_scope_range(VarRange, Document),
  VarsInScope = [atom_to_binary(Id, utf8) || #{range := R, id := Id} <- POIs,
                                             els_range:in(R, ScopeRange),
                                             els_range:compare(R, VarRange)],
  case [{els_utils:levenshtein_distance(V, Var), V} ||
         V <- VarsInScope,
         V =/= Var,
         binary:at(Var, 0) =:= binary:at(V, 0)]
  of
    [] ->
      [];
    VariableDistances ->
      {_, SimilarVariable} = lists:min(VariableDistances),
      [ replace_range_action( Uri
                            , <<"Did you mean '", SimilarVariable/binary, "'?">>
                            , ?CODE_ACTION_KIND_QUICKFIX
                            , SimilarVariable
                            , els_protocol:range(VarRange)) ]
  end.

-spec action_fix_module_name(uri(), range(), [binary()]) -> [map()].
action_fix_module_name(Uri, Range, [FileName]) ->
  [ replace_range_action( Uri
                        , <<"Change to -module(", FileName/binary, ").">>
                        , ?CODE_ACTION_KIND_QUICKFIX
                        , FileName
                        , Range)].

-spec var_range(poi_range(), binary(), [poi()]) -> poi_range().
var_range(#{from := {Line, _}}, Var, POIs) ->
  %% Hack for OTP <24 as range from diagnostics doesn't include columns
  VarAtom = binary_to_atom(Var, utf8),
  hd([R || #{range := R, id := Id} <- POIs,
           els_range:in(R, #{from => {Line, 1}, to => {Line + 1, 1}}),
           Id =:= VarAtom]).

%%------------------------------------------------------------------------------
