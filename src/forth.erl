-module(forth).

-export([evaluate/1]).


trim(Str) ->
    string:trim(Str, both).

parse(Instructions) ->
    convert_ints(lists:map(fun trim/1, string:split(Instructions, " ", all))).

maybe_convert_int(Str) ->
    case string:to_integer(Str) of
        {Int, []} ->
            Int;
        _ ->
            Str
    end.

convert_ints(Tokens) ->
    lists:map(fun maybe_convert_int/1, Tokens).

default_words() ->
    dict:from_list(
      [
       {"+",     {2,   fun([A, B], W) -> {[B+A], W} end}},
       {"-",     {2,   fun([A, B], W) -> {[B-A], W} end}},
       {"*",     {2,   fun([A, B], W) -> {[B*A], W} end}},
       {"/",     {2,   fun([A, B], W) -> {[B div A], W} end}},
       {"dup",   {1,   fun([A], W)    -> {[A, A], W} end}},
       {"drop",  {1,   fun([_A], W)    -> {[], W} end}},
       {"swap",  {2,   fun([A, B], W) -> {[B, A], W} end}},
       {"over",  {2,   fun([A, B], W) -> {[B, A, B], W} end}}
      ]).


interpret(_Next, {ok, {Arity, Fun}}, RemainingInputs, Stack, Words) ->
    {Args, StackTail} = lists:split(Arity, Stack),
    {Result, NewWords} = Fun(Args, Words),
    {RemainingInputs, Result ++ StackTail, NewWords};
interpret(Next, error, RemainingInputs, Stack, Words) ->
    {RemainingInputs, [Next|Stack], Words}.


evaluate(Instructions) ->
    real_evaluate({parse(Instructions), [], default_words()}).

real_evaluate({[], Stack, _Words}) ->
    lists:reverse(Stack);
real_evaluate({[H|T], Stack, Words}) ->
    real_evaluate(interpret(H, dict:find(H, Words), T, Stack, Words)).
