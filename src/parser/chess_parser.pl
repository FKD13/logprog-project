:- module(chess_parser, [parse_board/5]).

/** <module> chess_parser - Chess board parsing
*/

:- use_module('_columns').
:- use_module('_digits').
:- use_module('_pieces').


%!  parse_board(-Board, -MetaDataWhite, -MetaDataBlack, +List, +End)
%   
%   Parse the board.
%   The board consists of several compounds to represent it the first one is the Board compound.
%   ~~~
%   b(Row, Row, Row, Row, Row, Row, Row, Row)
%   ~~~
%   Which at it's turn consists of 8 Row compounds.
%   ~~~
%   r(Piece, Piece, Piece, Piece, Piece, Piece, Piece, Piece)
%   ~~~
%   Here a piece can be one of two things. Either the atom empty or the Piece compound.
%   ~~~
%   p(Color, Type).
%   ~~~
%   Appart from the Board we also have the MetaData for the black and white player.
%   ~~~
%   m(QueenRokade, KingRokade, Turn, Passant).
%   ~~~
%   These compounds are used in the whole codebase.
parse_board(b(R1, R2, R3, R4, R5, R6, R7, R8), MetaWhite, MetaBlack) --> 
    parse_column(8, R8), parse_metadata(MetaBlack, b), parse_newline,
    parse_column(7, R7), parse_newline,
    parse_column(6, R6), parse_newline,
    parse_column(5, R5), parse_newline,
    parse_column(4, R4), parse_newline,
    parse_column(3, R3), parse_newline,
    parse_column(2, R2), parse_newline,
    parse_column(1, R1), parse_metadata(MetaWhite, w), parse_newline,
    parse_last_line.


%!  parse_column(-Id, -Row, +List, +End)
%
%   Parse a whole column (index + 8 Pieces).
parse_column(N, Row) --> parse_digit(N), " ", parse_eigth_pieces(Row).


%!  parse_eigth_pieces(-Row, +List, +End)
%
% Parse 8 pieces
parse_eigth_pieces(r(P1, P2, P3, P4, P5, P6, P7, P8)) --> 
    parse_optional_piece(P1),
    parse_optional_piece(P2),
    parse_optional_piece(P3),
    parse_optional_piece(P4), 
    parse_optional_piece(P5), 
    parse_optional_piece(P6), 
    parse_optional_piece(P7), 
    parse_optional_piece(P8).

%!  parse_metadata(-MetaData, +Color, +List, +End)
%
%   Parse a single piece of metadata for a given color.
parse_metadata(m(Queen, King, Turn, Coord), Color) --> 
    " [", 
    parse_optional_rocade(Color, queen, Queen),
    parse_optional_rocade(Color, king, King),
    parse_optional_en_passant(Coord),
    "]", !,
    parse_optional_turn_indicator(Turn). 


%!  parse_optional_rocade(+Color, +Piece, -Present, +List, +End)
%
%   Parse an optional rocade for a Color. Present indicated if there is a rocade possibility.
parse_optional_rocade(_, _, no)          --> parse_optional_piece(empty).
parse_optional_rocade(Color, Piece, yes) --> parse_optional_piece(p(Color, Piece)), {member(Piece, [queen, king])}.

%!  parse_optional_en_passant(-Coord, +List, +End)
%
%   Parse the optional coordinate of an en pasant posibility.
parse_optional_en_passant(no) --> "".
parse_optional_en_passant(R/C) --> parse_column_id(C), parse_digit(R).

%!  parse_optional_turn_indicator(-Present, +List, +End)
%
%   Parse the optional turn indicator. Present indicated if the indicator is present.
parse_optional_turn_indicator(yes) --> "\u261A", !.
parse_optional_turn_indicator(no) --> [].

parse_last_line --> "  abcdefgh", parse_optional_newline.

parse_newline --> "\n".

parse_optional_newline --> "" | parse_newline.