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

evaluate(Instructions) ->
    evaluate(parse(Instructions), []).

evaluate([], Stack) ->
    lists:reverse(Stack);
evaluate([H|T], Stack) when is_integer(H) ->
    evaluate(T, [H|Stack]);
evaluate(["+"|T], [A, B|Stack]) when is_integer(A), is_integer(B) ->
    evaluate(T, [A+B|Stack]);
evaluate(["-"|T], [A, B|Stack]) when is_integer(A), is_integer(B) ->
    evaluate(T, [B-A|Stack]);
evaluate(["*"|T], [A, B|Stack]) when is_integer(A), is_integer(B) ->
    evaluate(T, [A*B|Stack]);
evaluate(["/"|T], [A, B|Stack]) when is_integer(A), is_integer(B) ->
    evaluate(T, [B div A|Stack]);
evaluate(["dup"|T], [A|Stack]) ->
    evaluate(T, [A,A|Stack]);
evaluate(["drop"|T], [_A|Stack]) ->
    evaluate(T, Stack);
evaluate(["swap"|T], [A, B|Stack]) ->
    evaluate(T, [B, A|Stack]);
evaluate(["over"|T], [A, B|Stack]) ->
    evaluate(T, [B, A, B|Stack]).
