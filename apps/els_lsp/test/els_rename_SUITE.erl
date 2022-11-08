-module(els_rename_SUITE).

-include("els_lsp.hrl").

%% CT Callbacks
-export([
    suite/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    all/0
]).

%% Test cases
-export([
    rename_behaviour_callback/1,
    rename_macro/1,
    rename_module/1,
    rename_variable/1,
    rename_variable_list_comp/1,
    rename_function/1,
    rename_function_quoted_atom/1,
    rename_type/1,
    rename_opaque/1,
    rename_parametrized_macro/1,
    rename_macro_from_usage/1,
    rename_record/1,
    rename_record_field/1,
    prepare_rename/1
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%%==============================================================================
%% Types
%%==============================================================================
-type config() :: [{atom(), any()}].

%%==============================================================================
%% CT Callbacks
%%==============================================================================
-spec suite() -> [tuple()].
suite() ->
    [{timetrap, {seconds, 30}}].

-spec all() -> [atom()].
all() ->
    els_test_utils:all(?MODULE).

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    els_test_utils:init_per_suite(Config).

-spec end_per_suite(config()) -> ok.
end_per_suite(Config) ->
    els_test_utils:end_per_suite(Config).

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(TestCase, Config) ->
    els_test_utils:init_per_testcase(TestCase, Config).

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(TestCase, Config) ->
    els_test_utils:end_per_testcase(TestCase, Config).

%%==============================================================================
%% Testcases
%%==============================================================================
-spec rename_behaviour_callback(config()) -> ok.
rename_behaviour_callback(Config) ->
    Uri = ?config(rename_uri, Config),
    Line = 2,
    Char = 9,
    NewName = <<"new_awesome_name">>,
    Result = rename(Uri, Line, Char, NewName),
    Expected = #{
        changes =>
            #{
                binary_to_atom(Uri, utf8) =>
                    [
                        #{
                            newText => NewName,
                            range =>
                                #{
                                    'end' => #{character => 19, line => 2},
                                    start => #{character => 10, line => 2}
                                }
                        }
                    ],
                binary_to_atom(?config(rename_usage1_uri, Config), utf8) =>
                    [
                        #{
                            newText => NewName,
                            range =>
                                #{
                                    'end' => #{character => 18, line => 6},
                                    start => #{character => 9, line => 6}
                                }
                        },
                        #{
                            newText => NewName,
                            range =>
                                #{
                                    'end' => #{character => 9, line => 9},
                                    start => #{character => 0, line => 9}
                                }
                        },
                        #{
                            newText => NewName,
                            range =>
                                #{
                                    'end' => #{character => 9, line => 11},
                                    start => #{character => 0, line => 11}
                                }
                        },
                        #{
                            newText => NewName,
                            range =>
                                #{
                                    'end' => #{character => 15, line => 8},
                                    start => #{character => 6, line => 8}
                                }
                        }
                    ],
                binary_to_atom(?config(rename_usage2_uri, Config), utf8) =>
                    [
                        #{
                            newText => NewName,
                            range =>
                                #{
                                    'end' => #{character => 18, line => 6},
                                    start => #{character => 9, line => 6}
                                }
                        },
                        #{
                            newText => NewName,
                            range =>
                                #{
                                    'end' => #{character => 9, line => 8},
                                    start => #{character => 0, line => 8}
                                }
                        }
                    ]
            }
    },
    assert_changes(Expected, Result).

-spec rename_variable(config()) -> ok.
rename_variable(Config) ->
    Uri = ?config(rename_variable_uri, Config),
    UriAtom = binary_to_atom(Uri, utf8),
    NewName = <<"NewAwesomeName">>,
    %%
    Result1 = rename(Uri, 3, 3, NewName),
    Expected1 = #{
        changes => #{
            UriAtom => [
                change(NewName, {3, 2}, {3, 5}),
                change(NewName, {2, 4}, {2, 7})
            ]
        }
    },
    Result2 = rename(Uri, 2, 5, NewName),
    Expected2 = #{
        changes => #{
            UriAtom => [
                change(NewName, {3, 2}, {3, 5}),
                change(NewName, {2, 4}, {2, 7})
            ]
        }
    },
    Result3 = rename(Uri, 6, 3, NewName),
    Expected3 = #{
        changes => #{
            UriAtom => [
                change(NewName, {6, 18}, {6, 21}),
                change(NewName, {6, 2}, {6, 5}),
                change(NewName, {5, 9}, {5, 12}),
                change(NewName, {4, 4}, {4, 7})
            ]
        }
    },
    Result4 = rename(Uri, 11, 3, NewName),
    Expected4 = #{
        changes => #{
            UriAtom => [
                change(NewName, {11, 2}, {11, 5}),
                change(NewName, {10, 4}, {10, 7})
            ]
        }
    },
    %% Spec
    Result5 = rename(Uri, 13, 10, NewName),
    Expected5 = #{
        changes => #{
            UriAtom => [
                change(NewName, {14, 15}, {14, 18}),
                change(NewName, {13, 18}, {13, 21}),
                change(NewName, {13, 10}, {13, 13})
            ]
        }
    },
    %% Record
    Result6 = rename(Uri, 18, 19, NewName),
    Expected6 = #{
        changes => #{
            UriAtom => [
                change(NewName, {19, 20}, {19, 23}),
                change(NewName, {18, 19}, {18, 22})
            ]
        }
    },
    %% Macro
    Result7 = rename(Uri, 21, 20, NewName),
    Expected7 = #{
        changes => #{
            UriAtom => [
                change(NewName, {21, 26}, {21, 29}),
                change(NewName, {21, 20}, {21, 23}),
                change(NewName, {21, 14}, {21, 17})
            ]
        }
    },
    %% Type
    Result8 = rename(Uri, 23, 11, NewName),
    Expected8 = #{
        changes => #{
            UriAtom => [
                change(NewName, {23, 11}, {23, 14}),
                change(NewName, {23, 19}, {23, 22})
            ]
        }
    },
    %% Opaque
    Result9 = rename(Uri, 24, 15, NewName),
    Expected9 = #{
        changes => #{
            UriAtom => [
                change(NewName, {24, 15}, {24, 18}),
                change(NewName, {24, 23}, {24, 26})
            ]
        }
    },
    %% Callback
    Result10 = rename(Uri, 1, 15, NewName),
    Expected10 = #{
        changes => #{
            UriAtom => [
                change(NewName, {1, 23}, {1, 26}),
                change(NewName, {1, 15}, {1, 18})
            ]
        }
    },
    %% If
    Result11 = rename(Uri, 29, 4, NewName),
    Expected11 = #{
        changes => #{
            UriAtom => [
                change(NewName, {29, 11}, {29, 14}),
                change(NewName, {29, 4}, {29, 7})
            ]
        }
    },
    assert_changes(Expected1, Result1),
    assert_changes(Expected2, Result2),
    assert_changes(Expected3, Result3),
    assert_changes(Expected4, Result4),
    assert_changes(Expected5, Result5),
    assert_changes(Expected6, Result6),
    assert_changes(Expected7, Result7),
    assert_changes(Expected8, Result8),
    assert_changes(Expected9, Result9),
    assert_changes(Expected10, Result10),
    assert_changes(Expected11, Result11).

-spec rename_variable_list_comp(config()) -> ok.
rename_variable_list_comp(Config) ->
    Uri = ?config(variable_list_comp_uri, Config),
    UriAtom = binary_to_atom(Uri, utf8),
    NewName = <<"NewAwesomeName">>,
    %% one: Skip LC
    Result1 = rename(Uri, 3, 4, NewName),
    Result1 = rename(Uri, 5, 4, NewName),
    Expected1 = #{
        changes => #{
            UriAtom => [
                change(NewName, {3, 4}, {3, 7}),
                change(NewName, {5, 4}, {5, 7})
            ]
        }
    },
    %% one: Rename in LC only
    Result2 = rename(Uri, 4, 6, NewName),
    Result2 = rename(Uri, 4, 13, NewName),
    Expected2 = #{
        changes => #{
            UriAtom => [
                change(NewName, {4, 6}, {4, 9}),
                change(NewName, {4, 13}, {4, 16})
            ]
        }
    },
    %% two: Rename in first LC only
    Result3 = rename(Uri, 8, 6, NewName),
    Result3 = rename(Uri, 8, 13, NewName),
    Expected3 = #{
        changes => #{
            UriAtom => [
                change(NewName, {8, 6}, {8, 9}),
                change(NewName, {8, 13}, {8, 16})
            ]
        }
    },
    %% two: Rename in second LC only
    Result4 = rename(Uri, 9, 6, NewName),
    Result4 = rename(Uri, 9, 13, NewName),
    Expected4 = #{
        changes => #{
            UriAtom => [
                change(NewName, {9, 6}, {9, 9}),
                change(NewName, {9, 13}, {9, 16})
            ]
        }
    },
    %% three: Rename all Var (no Var in LC pattern)
    Result5 = rename(Uri, 12, 4, NewName),
    Result5 = rename(Uri, 13, 6, NewName),
    Result5 = rename(Uri, 14, 4, NewName),
    Expected5 = #{
        changes => #{
            UriAtom => [
                change(NewName, {12, 4}, {12, 7}),
                change(NewName, {13, 6}, {13, 9}),
                change(NewName, {14, 4}, {14, 7})
            ]
        }
    },
    %% four: Rename Var2, second LC generator pattern
    Result6 = rename(Uri, 17, 12, NewName),
    Result6 = rename(Uri, 18, 21, NewName),
    Expected6 = #{
        changes => #{
            UriAtom => [
                change(NewName, {17, 12}, {17, 16}),
                change(NewName, {18, 21}, {18, 25})
            ]
        }
    },
    %% four: FIXME: Bug, shouldn't rename Var inside second LC
    %% Result7 = rename(Uri, 17, 7, NewName),
    %% Result7 = rename(Uri, 17, 21, NewName),
    %% Expected7 = #{
    %%     changes => #{
    %%         UriAtom => [
    %%             change(NewName, {17, 7}, {17, 10}),
    %%             change(NewName, {17, 21}, {17, 24})
    %%         ]
    %%     }
    %% },
    assert_changes(Expected1, Result1),
    assert_changes(Expected2, Result2),
    assert_changes(Expected3, Result3),
    assert_changes(Expected4, Result4),
    assert_changes(Expected5, Result5),
    assert_changes(Expected6, Result6),
    %% assert_changes(Expected7, Result7),
    ok.

-spec rename_macro(config()) -> ok.
rename_macro(Config) ->
    Uri = ?config(rename_h_uri, Config),
    Line = 0,
    Char = 13,
    NewName = <<"NEW_AWESOME_NAME">>,
    NewNameUsage = <<"?NEW_AWESOME_NAME">>,
    Result = rename(Uri, Line, Char, NewName),
    Expected = #{
        changes =>
            #{
                binary_to_atom(Uri, utf8) =>
                    [
                        #{
                            newText => NewName,
                            range =>
                                #{
                                    'end' => #{character => 17, line => 0},
                                    start => #{character => 8, line => 0}
                                }
                        }
                    ],
                binary_to_atom(?config(rename_usage1_uri, Config), utf8) =>
                    [
                        #{
                            newText => NewNameUsage,
                            range =>
                                #{
                                    'end' => #{character => 13, line => 15},
                                    start => #{character => 3, line => 15}
                                }
                        },
                        #{
                            newText => NewNameUsage,
                            range =>
                                #{
                                    'end' => #{character => 25, line => 15},
                                    start => #{character => 15, line => 15}
                                }
                        }
                    ],
                binary_to_atom(?config(rename_usage2_uri, Config), utf8) =>
                    [
                        #{
                            newText => NewNameUsage,
                            range =>
                                #{
                                    'end' => #{character => 26, line => 11},
                                    start => #{character => 16, line => 11}
                                }
                        }
                    ]
            }
    },
    assert_changes(Expected, Result).

-spec rename_module(config()) -> ok.
rename_module(Config) ->
    UriA = ?config(rename_module_a_uri, Config),
    UriB = ?config(rename_module_b_uri, Config),
    NewName = <<"new_module">>,
    Path = filename:dirname(els_uri:path(UriA)),
    NewUri = els_uri:uri(filename:join(Path, <<NewName/binary, ".erl">>)),
    #{documentChanges := Result} = rename(UriA, 0, 14, NewName),
    Expected = [
        %% Module attribute
        #{
            edits => [change(NewName, {0, 8}, {0, 23})],
            textDocument => #{uri => UriA, version => null}
        },
        %% Rename file
        #{
            kind => <<"rename">>,
            newUri => NewUri,
            oldUri => UriA
        },
        %% Implicit function
        #{
            edits => [change(NewName, {12, 10}, {12, 25})],
            textDocument => #{uri => UriB, version => null}
        },
        %% Function application
        #{
            edits => [change(NewName, {11, 2}, {11, 17})],
            textDocument => #{uri => UriB, version => null}
        },
        %% Import
        #{
            edits => [change(NewName, {3, 8}, {3, 23})],
            textDocument => #{uri => UriB, version => null}
        },
        %% Type application
        #{
            edits => [change(NewName, {7, 18}, {7, 33})],
            textDocument => #{uri => UriB, version => null}
        },
        %% Behaviour
        #{
            edits => [change(NewName, {2, 11}, {2, 26})],
            textDocument => #{uri => UriB, version => null}
        }
    ],
    ?assertEqual([], Result -- Expected),
    ?assertEqual([], Expected -- Result),
    ?assertEqual(lists:sort(Expected), lists:sort(Result)).

-spec rename_function(config()) -> ok.
rename_function(Config) ->
    Uri = ?config(rename_function_uri, Config),
    ImportUri = ?config(rename_function_import_uri, Config),
    NewName = <<"new_function">>,
    %% Function
    Result = rename(Uri, 4, 2, NewName),
    %% Function clause
    Result = rename(Uri, 6, 2, NewName),
    %% Application
    Result = rename(ImportUri, 7, 18, NewName),
    %% Implicit fun
    Result = rename(Uri, 13, 10, NewName),
    %% Export entry
    Result = rename(Uri, 1, 9, NewName),
    %% Import entry
    Result = rename(ImportUri, 2, 26, NewName),
    %% Spec
    Result = rename(Uri, 3, 2, NewName),
    Expected = #{
        changes =>
            #{
                binary_to_atom(Uri, utf8) =>
                    [
                        change(NewName, {12, 23}, {12, 26}),
                        change(NewName, {13, 10}, {13, 13}),
                        change(NewName, {15, 27}, {15, 30}),
                        change(NewName, {17, 11}, {17, 14}),
                        change(NewName, {18, 2}, {18, 5}),
                        change(NewName, {1, 9}, {1, 12}),
                        change(NewName, {3, 6}, {3, 9}),
                        change(NewName, {4, 0}, {4, 3}),
                        change(NewName, {6, 0}, {6, 3}),
                        change(NewName, {8, 0}, {8, 3})
                    ],
                binary_to_atom(ImportUri, utf8) =>
                    [
                        change(NewName, {7, 18}, {7, 21}),
                        change(NewName, {2, 26}, {2, 29}),
                        change(NewName, {6, 2}, {6, 5})
                    ]
            }
    },
    assert_changes(Expected, Result).

-spec rename_function_quoted_atom(config()) -> ok.
rename_function_quoted_atom(Config) ->
    Uri = ?config(rename_function_uri, Config),
    Line = 21,
    Char = 2,
    NewName = <<"new_function">>,
    Result = rename(Uri, Line, Char, NewName),
    Expected = #{
        changes =>
            #{
                binary_to_atom(Uri, utf8) =>
                    [
                        change(NewName, {29, 23}, {29, 36}),
                        change(NewName, {30, 10}, {30, 23}),
                        change(NewName, {32, 27}, {32, 40}),
                        change(NewName, {34, 2}, {34, 15}),
                        change(NewName, {1, 16}, {1, 29}),
                        change(NewName, {20, 6}, {20, 19}),
                        change(NewName, {21, 0}, {21, 13}),
                        change(NewName, {23, 0}, {23, 13}),
                        change(NewName, {25, 0}, {25, 13})
                    ]
            }
    },
    assert_changes(Expected, Result).

-spec rename_type(config()) -> ok.
rename_type(Config) ->
    Uri = ?config(rename_type_uri, Config),
    NewName = <<"new_type">>,
    %% Definition
    Result = rename(Uri, 3, 7, NewName),
    %% Application
    Result = rename(Uri, 5, 18, NewName),
    %% Fully qualified application
    Result = rename(Uri, 4, 30, NewName),
    %% Export
    Result = rename(Uri, 1, 14, NewName),
    Expected = #{
        changes =>
            #{
                binary_to_atom(Uri, utf8) =>
                    [
                        change(NewName, {5, 18}, {5, 21}),
                        change(NewName, {4, 30}, {4, 33}),
                        change(NewName, {1, 14}, {1, 17}),
                        change(NewName, {3, 6}, {3, 9})
                    ]
            }
    },
    assert_changes(Expected, Result).

-spec rename_opaque(config()) -> ok.
rename_opaque(Config) ->
    Uri = ?config(rename_type_uri, Config),
    NewName = <<"new_opaque">>,
    %% Definition
    Result = rename(Uri, 4, 10, NewName),
    %% Application
    Result = rename(Uri, 5, 29, NewName),
    %% Export
    Result = rename(Uri, 1, 24, NewName),
    Expected = #{
        changes =>
            #{
                binary_to_atom(Uri, utf8) =>
                    [
                        change(NewName, {5, 26}, {5, 29}),
                        change(NewName, {1, 21}, {1, 24}),
                        change(NewName, {4, 8}, {4, 11})
                    ]
            }
    },
    assert_changes(Expected, Result).

-spec rename_parametrized_macro(config()) -> ok.
rename_parametrized_macro(Config) ->
    Uri = ?config(rename_h_uri, Config),
    Line = 2,
    Char = 16,
    NewName = <<"NEW_AWESOME_NAME">>,
    NewNameUsage = <<"?NEW_AWESOME_NAME">>,
    Result = rename(Uri, Line, Char, NewName),
    Expected = #{
        changes =>
            #{
                binary_to_atom(Uri, utf8) =>
                    [
                        #{
                            newText => NewName,
                            range =>
                                #{
                                    'end' => #{character => 30, line => 2},
                                    start => #{character => 8, line => 2}
                                }
                        }
                    ],
                binary_to_atom(?config(rename_usage1_uri, Config), utf8) =>
                    [
                        #{
                            newText => NewNameUsage,
                            range =>
                                #{
                                    'end' => #{character => 26, line => 18},
                                    start => #{character => 3, line => 18}
                                }
                        },
                        #{
                            newText => NewNameUsage,
                            range =>
                                #{
                                    'end' => #{character => 54, line => 18},
                                    start => #{character => 31, line => 18}
                                }
                        }
                    ],
                binary_to_atom(
                    ?config(rename_usage2_uri, Config), utf8
                ) =>
                    [
                        #{
                            newText => NewNameUsage,
                            range =>
                                #{
                                    'end' => #{character => 52, line => 14},
                                    start => #{character => 29, line => 14}
                                }
                        }
                    ]
            }
    },
    assert_changes(Expected, Result).

-spec rename_macro_from_usage(config()) -> ok.
rename_macro_from_usage(Config) ->
    Uri = ?config(rename_usage1_uri, Config),
    Line = 15,
    Char = 7,
    NewName = <<"NEW_AWESOME_NAME">>,
    NewNameUsage = <<"?NEW_AWESOME_NAME">>,
    Result = rename(Uri, Line, Char, NewName),
    Expected = #{
        changes =>
            #{
                binary_to_atom(?config(rename_h_uri, Config), utf8) =>
                    [
                        #{
                            newText => NewName,
                            range =>
                                #{
                                    'end' => #{character => 17, line => 0},
                                    start => #{character => 8, line => 0}
                                }
                        }
                    ],
                binary_to_atom(?config(rename_usage1_uri, Config), utf8) =>
                    [
                        #{
                            newText => NewNameUsage,
                            range =>
                                #{
                                    'end' => #{character => 13, line => 15},
                                    start => #{character => 3, line => 15}
                                }
                        },
                        #{
                            newText => NewNameUsage,
                            range =>
                                #{
                                    'end' => #{character => 25, line => 15},
                                    start => #{character => 15, line => 15}
                                }
                        }
                    ],
                binary_to_atom(?config(rename_usage2_uri, Config), utf8) =>
                    [
                        #{
                            newText => NewNameUsage,
                            range =>
                                #{
                                    'end' => #{character => 26, line => 11},
                                    start => #{character => 16, line => 11}
                                }
                        }
                    ]
            }
    },
    assert_changes(Expected, Result).

-spec rename_record(config()) -> ok.
rename_record(Config) ->
    HdrUri = ?config(rename_h_uri, Config),
    UsageUri = ?config(rename_usage1_uri, Config),
    NewName = <<"new_record_name">>,
    NewNameUsage = <<"#new_record_name">>,

    Expected = #{
        changes =>
            #{
                binary_to_atom(HdrUri, utf8) =>
                    [
                        #{
                            newText => NewName,
                            range =>
                                #{
                                    'end' => #{character => 18, line => 4},
                                    start => #{character => 8, line => 4}
                                }
                        }
                    ],
                binary_to_atom(UsageUri, utf8) =>
                    [
                        #{
                            newText => NewNameUsage,
                            range =>
                                #{
                                    'end' => #{character => 29, line => 20},
                                    start => #{character => 18, line => 20}
                                }
                        },
                        #{
                            newText => NewNameUsage,
                            range =>
                                #{
                                    'end' => #{character => 18, line => 22},
                                    start => #{character => 7, line => 22}
                                }
                        },
                        #{
                            newText => NewNameUsage,
                            range =>
                                #{
                                    'end' => #{character => 18, line => 24},
                                    start => #{character => 7, line => 24}
                                }
                        },
                        #{
                            newText => NewNameUsage,
                            range =>
                                #{
                                    'end' => #{character => 15, line => 26},
                                    start => #{character => 4, line => 26}
                                }
                        }
                    ]
            }
    },

    %% definition
    assert_changes(Expected, rename(HdrUri, 4, 10, NewName)),
    assert_changes(Expected, rename(UsageUri, 22, 10, NewName)).

-spec rename_record_field(config()) -> ok.
rename_record_field(Config) ->
    HdrUri = ?config(rename_h_uri, Config),
    UsageUri = ?config(rename_usage1_uri, Config),
    NewName = <<"new_record_field_name">>,

    Expected = #{
        changes =>
            #{
                binary_to_atom(HdrUri, utf8) =>
                    [
                        #{
                            newText => NewName,
                            range =>
                                #{
                                    'end' => #{character => 33, line => 4},
                                    start => #{character => 21, line => 4}
                                }
                        }
                    ],
                binary_to_atom(UsageUri, utf8) =>
                    [
                        #{
                            newText => NewName,
                            range =>
                                #{
                                    'end' => #{character => 42, line => 20},
                                    start => #{character => 30, line => 20}
                                }
                        },
                        #{
                            newText => NewName,
                            range =>
                                #{
                                    'end' => #{character => 31, line => 22},
                                    start => #{character => 19, line => 22}
                                }
                        },
                        #{
                            newText => NewName,
                            range =>
                                #{
                                    'end' => #{character => 31, line => 24},
                                    start => #{character => 19, line => 24}
                                }
                        },
                        #{
                            newText => NewName,
                            range =>
                                #{
                                    'end' => #{character => 28, line => 26},
                                    start => #{character => 16, line => 26}
                                }
                        }
                    ]
            }
    },

    %% definition
    Result = rename(HdrUri, 4, 25, NewName),
    assert_changes(Expected, Result),
    %% usage
    assert_changes(Expected, rename(UsageUri, 22, 25, NewName)).

-spec prepare_rename(config()) -> ok.
prepare_rename(Config) ->
    Uri = ?config(rename_uri, Config),
    %% Pointing to something that isn't a renameable POI should
    %% cause prepareRename to fail.
    ?assertEqual(prepare_rename_failed, rename(Uri, 1, 1, <<"NewName">>)),
    ?assertEqual(prepare_rename_failed, rename(Uri, 99, 99, <<"NewName">>)).

assert_changes(#{changes := ExpectedChanges}, #{changes := Changes}) ->
    ?assertEqual(maps:keys(ExpectedChanges), maps:keys(Changes)),
    Pairs = lists:zip(
        lists:sort(maps:to_list(Changes)),
        lists:sort(maps:to_list(ExpectedChanges))
    ),
    [
        begin
            ?assertEqual(ExpectedKey, Key),
            ?assertEqual(lists:sort(Expected), lists:sort(Change))
        end
     || {{Key, Change}, {ExpectedKey, Expected}} <- Pairs
    ],
    ok.

change(NewName, {FromL, FromC}, {ToL, ToC}) ->
    #{
        newText => NewName,
        range => #{
            start => #{character => FromC, line => FromL},
            'end' => #{character => ToC, line => ToL}
        }
    }.

rename(Uri, Line, Character, NewName) ->
    case els_client:prepare_rename(Uri, Line, Character) of
        #{result := null} ->
            prepare_rename_failed;
        #{result := _PreResult} ->
            case els_client:rename(Uri, Line, Character, NewName) of
                #{result := null} ->
                    rename_failed;
                #{result := Result} ->
                    Result
            end
    end.
