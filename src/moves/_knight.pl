:- module('_knight', [get_knight_moves/4]).

:- use_module('../moves/_utils').

get_knight_moves(Board, Piece, R/C, L1-T) :-
    RP2 is R + 2, CP2 is C + 2,
    RP1 is R + 1, CP1 is C + 1,
    RM1 is R - 1, CM1 is C - 1,
    RM2 is R - 2, CM2 is C - 2,
    test_position(Board, Piece, R/C, RP2/CP1, L1-L2),
    test_position(Board, Piece, R/C, RM2/CP1, L2-L3),
    test_position(Board, Piece, R/C, RP1/CP2, L3-L4),
    test_position(Board, Piece, R/C, RM1/CP2, L4-L5),
    test_position(Board, Piece, R/C, RP1/CM2, L5-L6),
    test_position(Board, Piece, R/C, RM1/CM2, L6-L7),
    test_position(Board, Piece, R/C, RP2/CM1, L7-L8),
    test_position(Board, Piece, R/C, RM2/CM1, L8-T).