:- initialization run.

:- ['moves/_diagonal'].
:- ['moves/_horizontal'].
:- ['moves/_vertical'].

:- ['parser/_digits'].
:- ['parser/_pieces'].
:- ['parser/chess_parser'].

:- ['utils/board_utils'].

run :-
    run_tests,
    halt(0).