-module(forth).

-export([evaluate/1]).
%%-compile(export_all).

%% The inputs as provided by the Exercism tests try to make life
%% easier to expressing word definitions as distinct strings. I would
%% rather deal with the input as one long stream, so in this function
%% I take a list of strings and tokenize them into one list.
%%
%% The filtering is done to allow for extraneous whitespace in the
%% strings, which doesn't occur in the test file but makes me feel
%% better.
collapse(I) ->
    lists:filter(fun([]) -> false;
                    (_) -> true
                 end,
                 lists:flatmap(fun(Str) -> string:split(Str, " ", all) end,
                                         I)).

parse(Instructions) ->
    convert_ints(collapse(Instructions)).

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
       {"over",  {2,   fun([A, B], W) -> {[B, A, B], W} end}},
       %% Words, words, words are important
       {":",     fun accumulate/4 }
      ]).

accumulate(Next, Inputs, Stack, Words) ->
    accumulate(Next, Inputs, Stack, Words, [], undefined).

%% This function will crash by design if ":" is found in the middle of
%% a new definition.
accumulate(":", [H|T], Stack, Words, [], undefined) ->
    accumulate(H, T, Stack, Words, [], undefined);
accumulate(NewWord, [H|T], Stack, Words, [], undefined) ->
    accumulate(H, T, Stack, Words, [], NewWord);
accumulate(";", Inputs, Stack, Words, Accum, NewWord) ->
    {Inputs, Stack, dict:store(NewWord, lists:reverse(Accum), Words)};
accumulate(Word, [H|T], Stack, Words, Accum, NewWord) ->
    accumulate(H, T, Stack, Words, [Word|Accum], NewWord).


interpret(Next, {ok, BypassFun}, RemainingInputs, Stack, Words)
  when is_function(BypassFun) ->
    {InputTail, NewStack, NewWords} =
        BypassFun(Next, RemainingInputs, Stack, Words),
    {InputTail, NewStack, NewWords};
interpret(_Next, {ok, ReplacementWords}, RemainingInputs, Stack, Words)
  when is_list(ReplacementWords) ->
    {ReplacementWords ++ RemainingInputs, Stack, Words};
interpret(_Next, {ok, {StackFun, ExecuteFun}}, RemainingInputs, Stack, Words)
  when is_function(StackFun) ->
    {ToProcess, StackTail} = StackFun(Stack),
    {Result, NewWords} = ExecuteFun(ToProcess, Words),
    {RemainingInputs, Result ++ StackTail, NewWords};
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
