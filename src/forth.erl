-module(forth).

-export([evaluate/1]).

%%%%% Tokenization

parse(Instructions) ->
    convert_ints(collapse(Instructions)).

convert_ints(Tokens) ->
    lists:map(fun maybe_convert_int/1, Tokens).

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

maybe_convert_int(Str) ->
    case string:to_integer(Str) of
        {Int, []} ->
            Int;
        _ ->
            Str
    end.

%% Our dictionary of words contains three types of values:

%%   * Built-in words are a 3-tuple: `builtin`, arity and anonymous
%%     function to take that arity of parameters off the stack (as a
%%     list) and return a list of new values. The last value on the
%%     stack is always the first item in the parameter list, and the
%%     resulting list is prepended to the current stack.

%%   * The word definition word (":") is unique; it supplies a bypass
%%     function that will be used in place of `interpret/5` to process
%%     the inputs up to ";". Its value tuple is `define`, `undefined`,
%%     and the bypass function.
%%
%%   * Custom-defined words, not present in this default dictionary,
%%     will have a 3-tuple with `custom`, a list of words and values
%%     to treat as replacement input, and *the word dictionary as it
%%     existed when that word was defined*

default_words() ->
    dict:from_list(
      [
       {"+",     {builtin, 2,   fun([A, B]) ->  [B+A] end}},
       {"-",     {builtin, 2,   fun([A, B]) ->  [B-A] end}},
       {"*",     {builtin, 2,   fun([A, B]) ->  [B*A] end}},
       {"/",     {builtin, 2,   fun([A, B]) ->  [B div A] end}},
       {"dup",   {builtin, 1,   fun([A])    ->  [A, A] end}},
       {"drop",  {builtin, 1,   fun([_A])   ->  [] end}},
       {"swap",  {builtin, 2,   fun([A, B]) ->  [B, A] end}},
       {"over",  {builtin, 2,   fun([A, B]) ->  [B, A, B] end}},
       {":",     {define,  undefined, fun new_word/4}}
      ]).

%% Ensure we fail if the input attempts to redefine numbers.
new_word(Next, _Inputs, _Stack, _Words) when not is_list(Next) ->
    {};
new_word(Next, [H|T], Stack, Words) ->
    new_word(H, T, Stack, Words, [], string:to_lower(Next)).

%% This function will crash by design if ":" is found in the middle of
%% a new definition.
new_word(":", _Input, _Stack, _Words, _Accum, _NewWord) ->
    {};
new_word(";", Inputs, Stack, Words, Accum, NewWord) ->
    {Inputs, Stack, dict:store(NewWord, {custom, lists:reverse(Accum), Words}, Words)};
new_word(WordOrNumber, [H|T], Stack, Words, Accum, NewWord) ->
    new_word(H, T, Stack, Words, [WordOrNumber|Accum], NewWord).


interpret(_Next, {ok, {define, undefined, BypassFun}}, [H|T], Stack, Words) ->
    {InputTail, NewStack, NewWords} =
        BypassFun(H, T, Stack, Words),
    {InputTail, NewStack, NewWords};
interpret(_Next, {ok, {custom, Replacements, WordsSnapshot}}, Inputs, Stack, Words) ->
    {Inputs, lists:reverse(real_evaluate({Replacements, Stack, WordsSnapshot})), Words};
interpret(_Next, {ok, {builtin, Arity, Fun}}, Inputs, Stack, Words) ->
    {Args, StackTail} = lists:split(Arity, Stack),
    Result = Fun(Args),
    {Inputs, Result ++ StackTail, Words};
interpret(Next, error, Inputs, Stack, Words) when is_integer(Next) ->
    {Inputs, [Next|Stack], Words};
interpret(_Next, error, _RemainingInputs, _Stack, _Words) ->
    %% This word wasn't found in our dictionary.
    {}.


evaluate(Instructions) ->
    real_evaluate({parse(Instructions), [], default_words()}).

real_evaluate({[], Stack, _Words}) ->
    lists:reverse(Stack);
real_evaluate({[H|T], Stack, Words}) ->
    real_evaluate(interpret(H, word_lookup(H, Words), T, Stack, Words)).

word_lookup(Number, _Words) when is_integer(Number) ->
    error;
word_lookup(MaybeWord, Words) ->
    dict:find(string:to_lower(MaybeWord), Words).
