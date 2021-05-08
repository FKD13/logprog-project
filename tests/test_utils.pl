:- module(test_utils, [empty_board/1, start_board/1]).

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