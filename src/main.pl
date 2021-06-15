:- module(main, [main/0]).

:- initialization main.

:- use_module('parser/chess_parser').
:- use_module('search/search').
:- use_module('utils/board_utils').

/** <module> Main module

The main module defines rules used to start the chess program.
*/


%! main
% 
% The main rule, this starts the program.
main:-
    read_string(user_input, _, Str),
    string_codes(Str, Codes),
    parse_board(Board, M1, M2, Codes, []),
    color(M1, M2, Color),
    best(Board, Color, Best),
    move(Board, M1, M2, Best, NB, NM1, NM2),
    parse_board(NB, NM1, NM2, NCodes, []),
    string_codes(Out, NCodes),
    write(Out),
    halt(0).

color(m(_, _, yes, _), m(_, _, no , _), w).
color(m(_, _, no , _), m(_, _, yes, _), b).
