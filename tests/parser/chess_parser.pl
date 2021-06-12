:- begin_tests('parser/chess_parser').
:- use_module('../../src/parser/chess_parser').
:- use_module('../test_utils').


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

test(parse_optional_en_passant, [setup(string_codes("e2", Codes)), set(Coord = [2/5])]) :-
    chess_parser:parse_optional_en_passant(Coord, Codes, []).

test(parse_optional_en_passant, [setup(string_codes("e2", Codes)), set(Coord = [no])]) :-
    chess_parser:parse_optional_en_passant(Coord, Codes, Codes).

valid_en_passant_generator(C, R) :-
    string_codes("12345678", Rows),
    string_codes("abcdefgh", Cols),
    member(C, Cols),
    member(R, Rows).

test(parse_optional_en_passant, [forall(valid_en_passant_generator(C, R)), set(Coord = [_/_])]) :-
    chess_parser:parse_optional_en_passant(Coord, [C, R], []).

invalid_en_passant_generator(C, R) :-
    string_codes("09", Rows),
    string_codes("ijklmnopqrstuvwxyz", Cols),
    member(C, Cols),
    member(R, Rows).

test(parse_optional_en_passant_fails, [forall(invalid_en_passant_generator(C, R)), fail]) :-
    chess_parser:parse_optional_en_passant(_/_, [C, R], []).

test(parse_last_line, [set(X = [[], [10]])]) :-
    string_codes("  abcdefgh\n", Codes),
    chess_parser:parse_last_line(Codes, X).

test(parse_last_line_fail, fail) :-
    string_codes(" abcdefgh\n", Codes),
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
    '_parse_metadata'(m(no , no , no , no), " [  ]"),
    '_parse_metadata'(m(yes, no , no , no), " [♛ ]"),
    '_parse_metadata'(m(no , yes, no , no), " [ ♚]"),
    '_parse_metadata'(m(yes, yes, no , no), " [♛♚]"),
    '_parse_metadata'(m(no , no , yes, no), " [  ]☚"),
    '_parse_metadata'(m(yes, no , yes, no), " [♛ ]☚"),
    '_parse_metadata'(m(no , yes, yes, no), " [ ♚]☚"),
    '_parse_metadata'(m(yes, yes, yes, no), " [♛♚]☚"),
    '_parse_metadata'(m(no , no , no , no), " [  ]"),
    '_parse_metadata'(m(yes, no , no , no), " [♕ ]"),
    '_parse_metadata'(m(no , yes, no , no), " [ ♔]"),
    '_parse_metadata'(m(yes, yes, no , no), " [♕♔]"),
    '_parse_metadata'(m(no , no , yes, no), " [  ]☚"),
    '_parse_metadata'(m(yes, no , yes, no), " [♕ ]☚"),
    '_parse_metadata'(m(no , yes, yes, no), " [ ♔]☚"),
    '_parse_metadata'(m(yes, yes, yes, no), " [♕♔]☚"),
    '_parse_metadata'(m(no , no , no , 2/5), " [  e2]"),
    '_parse_metadata'(m(yes, no , no , 2/5), " [♛ e2]"),
    '_parse_metadata'(m(no , yes, no , 2/5), " [ ♚e2]"),
    '_parse_metadata'(m(yes, yes, no , 2/5), " [♛♚e2]"),
    '_parse_metadata'(m(no , no , yes, 2/5), " [  e2]☚"),
    '_parse_metadata'(m(yes, no , yes, 2/5), " [♛ e2]☚"),
    '_parse_metadata'(m(no , yes, yes, 2/5), " [ ♚e2]☚"),
    '_parse_metadata'(m(yes, yes, yes, 2/5), " [♛♚e2]☚"),
    '_parse_metadata'(m(no , no , no , 2/5), " [  e2]"),
    '_parse_metadata'(m(yes, no , no , 2/5), " [♕ e2]"),
    '_parse_metadata'(m(no , yes, no , 2/5), " [ ♔e2]"),
    '_parse_metadata'(m(yes, yes, no , 2/5), " [♕♔e2]"),
    '_parse_metadata'(m(no , no , yes, 2/5), " [  e2]☚"),
    '_parse_metadata'(m(yes, no , yes, 2/5), " [♕ e2]☚"),
    '_parse_metadata'(m(no , yes, yes, 2/5), " [ ♔e2]☚"),
    '_parse_metadata'(m(yes, yes, yes, 2/5), " [♕♔e2]☚").

test(parse_board, [setup(start_board(SB)), set([B, M1, M2] = [[SB, m(yes, yes, yes, no), m(yes, yes, no, no)]])]) :-
    string_codes("8 ♜♞♝♛♚♝♞♜ [♛♚]\n7 ♟♟♟♟♟♟♟♟\n6         \n5         \n4         \n3         \n2 ♙♙♙♙♙♙♙♙\n1 ♖♘♗♕♔♗♘♖ [♕♔]☚\n  abcdefgh", Codes),    
    parse_board(B, M1, M2, Codes, []).

:- end_tests('parser/chess_parser').
