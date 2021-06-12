:- module(test_utils, [
    empty_board/1, 
    start_board/1, 
    column/1, 
    board_generator/1, 
    empty_generator/1,
    white_generator/1,
    black_generator/1]).

empty_board(b(
    r(empty, empty, empty, empty, empty, empty, empty, empty),
    r(empty, empty, empty, empty, empty, empty, empty, empty),
    r(empty, empty, empty, empty, empty, empty, empty, empty),
    r(empty, empty, empty, empty, empty, empty, empty, empty),
    r(empty, empty, empty, empty, empty, empty, empty, empty),
    r(empty, empty, empty, empty, empty, empty, empty, empty),
    r(empty, empty, empty, empty, empty, empty, empty, empty),
    r(empty, empty, empty, empty, empty, empty, empty, empty))).

start_board(b(
    r(p(w,rook), p(w,knight), p(w,bishop), p(w,queen), p(w,king), p(w,bishop), p(w,knight), p(w,rook)),
    r(p(w,pawn), p(w,pawn), p(w,pawn), p(w,pawn), p(w,pawn), p(w,pawn), p(w,pawn), p(w,pawn)),
    r(empty,empty,empty,empty,empty,empty,empty,empty),
    r(empty,empty,empty,empty,empty,empty,empty,empty),
    r(empty,empty,empty,empty,empty,empty,empty,empty),
    r(empty,empty,empty,empty,empty,empty,empty,empty),
    r(p(b,pawn), p(b,pawn), p(b,pawn), p(b,pawn), p(b,pawn), p(b,pawn), p(b,pawn), p(b,pawn)),
    r(p(b,rook), p(b,knight), p(b,bishop), p(b,queen), p(b,king), p(b,bishop), p(b,knight), p(b,rook)))).


column(C) :- member(C, [1,2,3,4,5,6,7,8]).

board_generator(R/C) :-
    column(R),
    column(C).

empty_generator(R/C) :-
    member(R, [3,4,5,6]),
    column(C).
    
white_generator(R/C) :-
    member(R, [1,2]),
    column(C).

black_generator(R/C) :-
    member(R, [7,8]),
    column(C).