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

%%%%% Word dictionary

%% Our dictionary of words contains three types of values:

%%   * Built-in words are a 3-tuple: `builtin`, arity and anonymous
%%     function to take that arity of parameters off the stack (as a
%%     list) and return a list of new values. The last value on the
%%     stack is always the first item in the parameter list, and the
%%     resulting list is prepended to the current stack.
%%
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

%%%%% Interpreter

%% `interpret/5` takes:
%%
%%    * The next value discovered in our tokenized input. Typically
%%      this is ignored because it has already been used to find the
%%      corresponding value in our dictionary of words, but if the
%%      next token is a number we will need to know what it is to
%%      place it on the stack.
%%
%%    * The results of the dictionary lookup. Either an `{ok, Value}`
%%      tuple or `error` if the token does not appear. See the
%%      comments for `default_words/0` to explain the dictionary
%%      values.
%%
%%    * The remaining tokenized inputs. This list will be modified by
%%      `interpret` if we encounter a word definition block.
%%
%%    * The current stack. Will always be modified except when
%%      erroneous input is encountered.
%%
%%    * The current dictionary of words. Will be modified only when a
%%      new word definition is encountered.
%%
%% `interpret/5` returns a tuple: `{NewInputs, NewStack, NewWords}`
%% (the new tokenized input list, the new stack, and the new
%% dictionary of words)
%%
%% Any erroneous input should result in an exception; the test code
%% does not expect useful error messages.
interpret(_Next, {ok, {define, undefined, BypassFun}}, [H|T], Stack, Words) ->
    {InputTail, NewStack, NewWords} =
        BypassFun(H, T, Stack, Words),
    {InputTail, NewStack, NewWords};
interpret(_Next, {ok, {custom, Replacements, WordsSnapshot}}, Inputs, Stack, Words) ->
    %% When we encounter a custom word as input, we have to evaluate
    %% it in the context of the words *as they existed when the word
    %% was defined*. Thus we invoke `real_evaluate` recursively in
    %% that context.
    %%
    %% Ordinarily we would need to be concerned that the recursive
    %% invocation of `interpret` might modify the inputs or dictionary
    %% of words, and that those modifications would be lost because
    %% `real_evaluate` only returns the results of its evaluation as a
    %% (reversed) stack of values, but those side-effects are
    %% precluded because we do not allow new words to be defined as
    %% part of a custom word.
    %%
    %% In short, this is illegal: ": newword : lazynewword value ; value ;"
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

%% Our API: Pass one or more strings as a list. This will fail if a
%% string is passed without being nested inside an outer list.
evaluate(Instructions) ->
    real_evaluate({parse(Instructions), [], default_words()}).

%% Iterate over a list of tokenized inputs, invoking `interpret/5`
%% with each token, but allowing `interpret` to update any of our
%% state values, including the list of inputs, so the iteration must
%% be recursive.
%%
%% The 3 state values: our list of inputs, our stack, and our
%% dictionary of words, both custom and built-in.
real_evaluate({[], Stack, _Words}) ->
    lists:reverse(Stack);
real_evaluate({[H|T], Stack, Words}) ->
    real_evaluate(interpret(H, word_lookup(H, Words), T, Stack, Words)).

%% I introduced this indirect lookup because I didn't want to call
%% `string:to_lower/1` on an integer. As it turns out, that seems to
%% be safe, at least with Erlang 22.0, but I don't want to assume it's
%% universally safe.
word_lookup(Number, _Words) when is_integer(Number) ->
    error;
word_lookup(MaybeWord, Words) ->
    dict:find(string:to_lower(MaybeWord), Words).
