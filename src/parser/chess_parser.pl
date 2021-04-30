:- module(chess_parser, [parse_board/5]).
:- use_module('_pieces').
:- use_module('_digits').

/** <module> Chess board parsing


@author Francis Klinck
*/


%! parse_board(-Board, -Meta, -Meta, -List, +Rest) is det
parse_board(b(R1, R2, R3, R4, R5, R6, R7, R8), MetaWhite, MetaBlack) --> 
    parse_column(8, R8), parse_metadata(MetaBlack), parse_newline,
    parse_column(7, R7), parse_newline,
    parse_column(6, R6), parse_newline,
    parse_column(5, R5), parse_newline,
    parse_column(4, R4), parse_newline,
    parse_column(3, R3), parse_newline,
    parse_column(2, R2), parse_newline,
    parse_column(1, R1), parse_metadata(MetaWhite), parse_newline,
    parse_last_line.

%!
parse_column(N, Row) --> parse_digit(N), " ", parse_eigth_pieces(Row).


% Parse 8 pieces
parse_eigth_pieces(r(P1, P2, P3, P4, P5, P6, P7, P8)) --> 
    parse_optional_piece(P1), !,
    parse_optional_piece(P2), !,
    parse_optional_piece(P3), !,
    parse_optional_piece(P4), !, 
    parse_optional_piece(P5), !, 
    parse_optional_piece(P6), !, 
    parse_optional_piece(P7), !, 
    parse_optional_piece(P8), !.

parse_metadata(m(Queen, King, Turn)) --> 
    " [", 
    parse_optional_rocade(_, queen, Queen),
    parse_optional_rocade(_, king, King),
    "]", !,
    parse_optional_turn_indicator(Turn). 

parse_optional_rocade(_, _, no)          --> parse_optional_piece(empty).
parse_optional_rocade(Color, Piece, yes) --> parse_optional_piece(p(Color, Piece)), {member(Piece, [queen, king])}.

parse_optional_turn_indicator(yes) --> "\u261A", !.
parse_optional_turn_indicator(no) --> [].

parse_last_line --> "  abcdefg", parse_optional_newline.

parse_newline --> "\n".

parse_optional_newline --> "" | parse_newline.