:- begin_tests(digits).
:- use_module('../../src/parser/_digits').

test(parse_digit_1, [setup(string_codes("1", Codes)), true(X =:= 1)]) :-
    parse_digit(X, Codes, []).

test(parse_digit_2, [setup(string_codes("2", Codes)), true(X =:= 2)]) :-
    parse_digit(X, Codes, []).

test(parse_digit_3, [setup(string_codes("3", Codes)), true(X =:= 3)]) :-
    parse_digit(X, Codes, []).

test(parse_digit_4, [setup(string_codes("4", Codes)), true(X =:= 4)]) :-
    parse_digit(X, Codes, []).

test(parse_digit_5, [setup(string_codes("5", Codes)), true(X =:= 5)]) :-
    parse_digit(X, Codes, []).

test(parse_digit_6, [setup(string_codes("6", Codes)), true(X =:= 6)]) :-
    parse_digit(X, Codes, []).

test(parse_digit_7, [setup(string_codes("7", Codes)), true(X =:= 7)]) :-
    parse_digit(X, Codes, []).

test(parse_digit_8, [setup(string_codes("8", Codes)), true(X =:= 8)]) :-
    parse_digit(X, Codes, []).

test(parse_digit_9, [setup(string_codes("9", Codes)), true(X =:= 9)]) :-
    parse_digit(X, Codes, []).

test(parse_digit_0, [setup(string_codes("0", Codes)), true(X =:= 0)]) :-
    parse_digit(X, Codes, []).

generate_failing_codes(X) :-
    string_codes("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ", Codes),
    member(X, Codes).

test(parse_digit_should_fail, [fail]) :-
    generate_failing_codes(X),
    parse_digit(_, [X], []).

:- end_tests(digits).
