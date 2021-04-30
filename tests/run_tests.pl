:- initialization run.
:- ['parser/_digits'].
:- ['parser/_pieces'].
:- ['parser/chess_parser'].

run :-
    run_tests,
    halt(0).