:- module(board_utils, [get_piece_at/3, set_piece_at/4]).

%!  get_piece_at(+Board, +Coord, -Piece)
%
%   Get the piece at a specific position on the board.
%
%   @arg Board A board to find pieces in.
%   @arg Coord The position to get the piece from.
%   @arg Piece The retrieved piece.
get_piece_at(Board, R/C, Piece) :-
    arg(R, Board, Row),
    arg(C, Row, Piece).


%!  set_piece_at(+Board, +Coord, +Piece, -OutBoard)
%
%   Set the piece at a specific position on the board.
%   Makes a copy of the board.
%
%   @arg Board The original board to set the piece in.
%   @arg Coord The position to place the piece.
%   @arg Piece The piece to place on the board.
%   @arg Out The output board.
set_piece_at(Board, R/C, Piece, Out) :-
    copy_term(Board, Out),
    arg(R, Out, Row),
    setarg(C, Row, Piece).