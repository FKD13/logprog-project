:- module(main, [main/1]).

:- initialization(main, main).

:- use_module('moves/moves').
:- use_module('parser/chess_parser').
:- use_module('search/search').
:- use_module('utils/board_utils').

/** <module> Main module

The main module defines rules used to start the chess program.
*/


%! main
% 
% The main rule, this starts the program.
main([]) :- 
    read_string(user_input, _, Str),
    string_codes(Str, Codes),
    parse_board(Board, M1, M2, Codes, []),
    color(M1, M2, Color),
    best(Board, Color, Best),
    move(Board, M1, M2, Best, NB, NM1, NM2),
    parse_board(NB, NM1, NM2, NCodes, []),
    string_codes(Out, NCodes),
    write(Out), nl,
    halt(0).
main(['TEST']) :-
    read_string(user_input, _, Str),
    string_codes(Str, Codes),
    parse_board(Board, M1, M2, Codes, []),
    color(M1, M2, Color),
    get_moves(Board, Color, Moves-[]),
    write_test_results(Board, M1, M2, Moves),
    halt(0).
main(_) :-
    nl, write("Invalid Aguments"), nl, halt(1).

write_test_results(_    , _ , _ , []).
write_test_results(Board, M1, M2, [Move]) :-
    move(Board, M1, M2, Move, NB, NM1, NM2),
    parse_board(NB, NM1, NM2, NCodes, []),
    string_codes(Out, NCodes),
    write(Out), nl.
write_test_results(Board, M1, M2, [Move | Moves]) :-
    write_test_results(Board, M1, M2, [Move]),
    write("~"), nl,
    write_test_results(Board, M1, M2, Moves).

color(m(_, _, yes, _), m(_, _, no , _), w).
color(m(_, _, no , _), m(_, _, yes, _), b).
