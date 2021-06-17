:- module('rokade', [get_rokade_moves/4]).

:- use_module('_horizontal').
:- use_module('_vertical').
:- use_module('_diagonal').
:- use_module('_knight').
:- use_module('_surroundings').

:- use_module('../utils/board_utils').
:- use_module('../utils/color').

get_rokade_moves(Board, m(Queen, King, yes, _), Color, Long-T) :- 
    long_rokade(Queen, Board, Color, Long-Short),
    short_rokade(King, Board, Color, Short-T_), T_ = T.

long_rokade(no , _    , _, T-T  ).
long_rokade(yes, Board, w, Moves) :- long_rokade_color(Board, w, b, 1, Moves).
long_rokade(yes, Board, b, Moves) :- long_rokade_color(Board, b, w, 8, Moves).

long_rokade_color(Board, Color, NextColor, R, [ r(m(p(Color, king), R/5, R/3), m(p(Color, rook), R/1, R/4)) | T]-T) :-
    safe(Board, R/5, Color, NextColor),
    get_piece_at(Board, R/1, p(Color, rook)),
    safe_and_empty(Board, R/2, Color, NextColor),
    safe_and_empty(Board, R/3, Color, NextColor),
    safe_and_empty(Board, R/4, Color, NextColor),
    get_piece_at(Board, R/5, p(Color, king)).
long_rokade_color(_, _, _, _, T-T).

short_rokade(no , _    , _, T-T  ).
short_rokade(yes, Board, w, Moves) :- short_rokade_color(Board, w, b, 1, Moves).
short_rokade(yes, Board, b, Moves) :- short_rokade_color(Board, b, w, 8, Moves).

short_rokade_color(Board, Color, NextColor, R, [ r(m(p(Color, king), R/5, R/7), m(p(Color, rook), R/8, R/6)) | T]-T) :-
    safe(Board, R/5, Color, NextColor),
    get_piece_at(Board, R/5, p(Color, king)),
    safe_and_empty(Board, R/6, Color, NextColor),
    safe_and_empty(Board, R/7, Color, NextColor),
    get_piece_at(Board, R/8, p(Color, rook)).
short_rokade_color(_, _, _, _, T-T).

safe_and_empty(Board, Coord, Color, NextColor) :-
    get_piece_at(Board, Coord, empty),
    safe(Board, Coord, Color, NextColor).

safe(Board, R/C, Color, NextColor) :- 
    \+ danger_vertical(Board   , R/C, Color, NextColor),
    \+ danger_diagonal(Board   , R/C, Color, NextColor),
    \+ danger_knight(Board     , R/C, Color, NextColor),
    \+ danger_surrounding(Board, R/C, Color, NextColor),
    CPlus is C + 1, CMin is C - 1,
    \+ danger_pawn(Board, R/CPlus, Color),
    \+ danger_pawn(Board, R/CMin , Color).

danger_pawn(Board, R/C, w) :- RPlus is R + 1, get_piece_at(Board, RPlus/C, p(b, pawn)).
danger_pawn(Board, R/C, b) :- RMin  is R - 1, get_piece_at(Board, RMin/C , p(w, pawn)).

danger_surrounding(Board, R/C, Color, NextColor) :-
    danger_X(get_surrounding_moves, Board, R/C, Color, NextColor, king, [king]).

danger_vertical(Board, Coord, Color, NextColor) :- 
    danger_X(get_vertical_moves, Board, Coord, Color, NextColor, rook, [queen, rook]).

danger_diagonal(Board, Coord, Color, NextColor) :-
    danger_X(get_diagonal_moves, Board, Coord, Color, NextColor, bishop, [queen, bishop]).

danger_knight(Board, Coord, Color, NextColor) :-
    danger_X(get_knight_moves, Board, Coord, Color, NextColor, knight, [knight]).

danger_X(MoveGenerator, Board, Coord, Color, NextColor, Type, DangerTypes) :-
    call(MoveGenerator, Board, p(Color, Type), Coord, Moves-[]), !,
    member(m(p(NextColor, OtherType), _, _), Moves),
    member(OtherType, DangerTypes).