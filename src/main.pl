:- module(main, [main/1]).

:- set_prolog_flag(stack_limit, 2_147_483_648).

:- initialization(main, main).

:- use_module('moves/moves').
:- use_module('parser/chess_parser').
:- use_module('search/search').
:- use_module('utils/board_utils').

/** <module> Main module

The main module defines rules used to start the chess program.
*/


%!  main(+Argv)
%
%   Run the program.
%   Accepts:
%   - No arguments: run the program normally.
%   - TEST argument: run the program in test mode.
%   - Else: repot invalid arguments.
% 
%   @arg Argv The command line arguments.
main([]) :- !,
    read_string(user_input, _, Str),
    string_codes(Str, Codes),
    parse_board(Board, M1, M2, Codes, []),
    color(M1, M2, Color),
    best(Board, M1, M2, Color, Best),
    move(Board, M1, M2, Best, NB, NM1, NM2),
    parse_board(NB, NM1, NM2, NCodes, []),
    string_codes(Out, NCodes),
    write(Out), nl,
    halt(0).
main(['TEST']) :- !,
    read_string(user_input, _, Str),
    string_codes(Str, Codes),
    parse_board(Board, M1, M2, Codes, []),
    color(M1, M2, Color),
    get_moves(Board, M1, M2, Color, Moves-T),
    write_test_results(Board, M1, M2, Moves),
    halt(0).
main(_) :- !, nl, write("Invalid Aguments"), nl, halt(1).


%!  write_test_results(+Board, +MetaDataWhite, +MetaDataBlack, +Moves)
%
%   Given a Board and it's metadata, for each move:
%   - Check id the move is valid. % TODO TEST THIS.
%   - Apply to the Board.
%   - Print the Board to stdout.
%   This while separating all boards by a 
%   ~~~
%   nl, write("~"), nl
%   ~~~
%
%   @arg Board
%   @arg MetaDataWhite Metadata for the white player.
%   @arg MetaDataBlack Metadata for the black player.
%   @arg Moves The moves to apply to the board.
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


%!  color(+MetaDataWhite, +MetaDataBlack, -Color)
%
%   Given 2 sets of metadata, get the playing color.
%
%   @arg MetaDataWhite Metadata for the white player.
%   @arg MetaDataBlack Metadata for the black player.
%   @arg Color The currently playing color.
color(m(_, _, yes, _), m(_, _, no , _), w).
color(m(_, _, no , _), m(_, _, yes, _), b).
