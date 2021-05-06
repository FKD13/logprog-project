:- module(board_utils, [get_piece_at/3]).

%!  get_piece_at(+Board, +Coord, -Piece)
%
%   Get the piece at a specific position on the board.
%
%   @arg Board A board to find pieces in.
%   @arg Coord The position to get the piece from.
%   @arg Piece The retrieved piece.
get_piece_at(Board, R/C, Piece) :-
    compound(Board),
    arg(R, Board, Row),
    compound(Row),
    arg(C, Row, Piece).


set_piece_at(Board, R/C, Piece, Out) :-
    Out = b(_,_,_,_,_,_,_,_),
    set_piece_at_rows(Board, R/C, Piece, Out, 8).

set_piece_at_rows(Board, R/C, Piece, Out, 0).
set_piece_at_rows(Board, R/C, Piece, Out, R) :-
    Row = r(_,_,_,_,_,_,_,_),
    arg(R, Out, Row).

set_piece_at_rows(Board, R/_, _, Out, N) :-
    R \== N,
    N1 is N - 1,
    arg(N, Board, Row),
    arg(N, Out, Row).

set_piece_at_cols(OldRow, C, Piece, Row, N).