:- begin_tests('parser/_pieces').
:- use_module('../../src/parser/_pieces').

test(parse_empty, [setup(string_codes(" ", Codes))]) :-
    '_pieces':parse_empty(empty, Codes, []).

test(parse_empty_should_fail, fail) :-
    numlist(0, 10000, L),
    member(X, L),
    X \== 32,
    '_pieces':parse_empty(_, [X], []).

test(parse_optional_piece, [setup(numlist(9812, 9823, List)), all(X == List)]) :-
    parse_optional_piece(p(_,_), [X], []).

test(parse_optional_piece_empty, [setup(string_codes(" ", Codes)), all(Piece == [empty])]) :-
    parse_optional_piece(Piece, Codes, []).

:- end_tests('parser/_pieces').