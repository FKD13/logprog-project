:- begin_tests('parser/chess_parser').
:- use_module('../../src/parser/chess_parser').


test(parse_optional_newline, [setup(string_codes("\n", Codes)), all(X = [Codes, []])]) :-
    chess_parser:parse_optional_newline(Codes, X).

test(parse_optional_newline_fail, [setup(string_codes(" ", Codes)), fail]) :-
    chess_parser:parse_optional_newline(Codes, []).

test(parse_optional_rocade_yes, [setup(string_codes("♛♚♕♔", Codes)), all(X = [yes])]) :-
    chess_parser:parse_optional_rocade(b, queen, X, Codes, R1),
    chess_parser:parse_optional_rocade(b, king, X, R1, R2),
    chess_parser:parse_optional_rocade(w, queen, X, R2, R3),
    chess_parser:parse_optional_rocade(w, king, X, R3, []).

test(parse_optional_rocade_no, [setup(string_codes("    ", Codes)), all(X = [no])]) :-
    chess_parser:parse_optional_rocade(b, queen, X, Codes, R1),
    chess_parser:parse_optional_rocade(b, king, X, R1, R2),
    chess_parser:parse_optional_rocade(w, queen, X, R2, R3),
    chess_parser:parse_optional_rocade(w, king, X, R3, []).

test(parse_optional_rocade_fails, fail) :-
    numlist(0, 10000, L),
    string_codes("♛♚♕♔ ", Codes),
    member(X, L),
    \+ member(X, Codes),
    chess_parser:parse_optional_rocade(_, _, _, [X], []).

:- end_tests('parser/chess_parser').
