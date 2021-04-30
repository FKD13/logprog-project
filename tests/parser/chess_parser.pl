:- begin_tests('parser/chess_parser').
:- use_module('../../src/parser/chess_parser').


test(parse_optional_newline, [setup(string_codes("\n", Codes)), set(X = [Codes, []])]) :-
    chess_parser:parse_optional_newline(Codes, X).

test(parse_optional_newline_fail, [setup(string_codes(" ", Codes)), fail]) :-
    chess_parser:parse_optional_newline(Codes, []).

test(parse_optional_rocade_yes, [setup(string_codes("♛♚♕♔", Codes)), set(X = [yes])]) :-
    chess_parser:parse_optional_rocade(b, queen, X, Codes, R1),
    chess_parser:parse_optional_rocade(b, king, X, R1, R2),
    chess_parser:parse_optional_rocade(w, queen, X, R2, R3),
    chess_parser:parse_optional_rocade(w, king, X, R3, []).

test(parse_optional_rocade_no, [setup(string_codes("    ", Codes)), set(X = [no])]) :-
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

test(parse_last_line, [set(X = [[], [10]])]) :-
    string_codes("  abcdefg\n", Codes),
    chess_parser:parse_last_line(Codes, X).

test(parse_last_line_fail, fail) :-
    string_codes(" abcdefg\n", Codes),
    chess_parser:parse_last_line(Codes, _).
    
test(parse_optional_turn_indicator) :-
    string_codes("\u261A", Codes),
    chess_parser:parse_optional_turn_indicator(yes, Codes, []).

test(parse_optional_turn_indicator) :-
    chess_parser:parse_optional_turn_indicator(no, [10], [10]).

test(parse_optional_turn_indicator_fail, fail) :-
    numlist(0, 10000, L),
    string_codes("\u261A", [Code]),
    member(X, L),
    X \== Code,
    chess_parser:parse_optional_turn_indicator(no, [X], []).

'_parse_metadata'(Meta, Str) :-
    string_codes(Str, Codes),
    chess_parser:parse_metadata(Meta, Codes, []).


test(parse_metadata) :-
    '_parse_metadata'(m(no , no , no ), " [  ]"),
    '_parse_metadata'(m(yes, no , no ), " [♛ ]"),
    '_parse_metadata'(m(no , yes, no ), " [ ♚]"),
    '_parse_metadata'(m(yes, yes, no ), " [♛♚]"),
    '_parse_metadata'(m(no , no , yes), " [  ]☚"),
    '_parse_metadata'(m(yes, no , yes), " [♛ ]☚"),
    '_parse_metadata'(m(no , yes, yes), " [ ♚]☚"),
    '_parse_metadata'(m(yes, yes, yes), " [♛♚]☚"),
    '_parse_metadata'(m(no , no , no ), " [  ]"),
    '_parse_metadata'(m(yes, no , no ), " [♕ ]"),
    '_parse_metadata'(m(no , yes, no ), " [ ♔]"),
    '_parse_metadata'(m(yes, yes, no ), " [♕♔]"),
    '_parse_metadata'(m(no , no , yes), " [  ]☚"),
    '_parse_metadata'(m(yes, no , yes), " [♕ ]☚"),
    '_parse_metadata'(m(no , yes, yes), " [ ♔]☚"),
    '_parse_metadata'(m(yes, yes, yes), " [♕♔]☚").

:- end_tests('parser/chess_parser').
