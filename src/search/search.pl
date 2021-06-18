:- module('search', [best/5, move/7]).

/** <module> search - Finding the best move.

This module is the key module of the chess program. Here the best move is searched using Alpha Beta pruning.

*/

:- use_module('../moves/check').
:- use_module('../moves/moves').
:- use_module('_score').
:- use_module('../utils/board_utils').
:- use_module('../utils/color').


%!  best(+Board, +MetaDataWhite, +MetaDataBlack, +Color, -Best)
%
%   Compute the best move by simulating the game using alpha beta pruning to a limited depth.
%   Best move here does not mean the actual best move because we use alpha beta pruning,
%   but it will be a move that will give a good result if possible.
%
%   @arg Board The board to check available moves on.
%   @arg MetaDataWhite Metadata for the white player.
%   @arg MetaDataBlack Metadata for the black player.
%   @arg Color The currently playing color.
%   @arg Best The best move to take.
best(Board, MW, MB, Color, Best) :-
    get_moves(Board, MW, MB, Color, TmpMoves-[]),
    random_permutation(TmpMoves, Moves),
    ab_prune(4, Board, MW, MB, Color, Color, b(-10001, 10001), _, s(Best, _), Moves). %, write(Score), nl.


%!  ab_prune(+Depth, +Board, +MetaDataWhite, +MetaDataBlack, +MaxColor, +Color, +Bounds, +CurrentBest, -BestMove, +Moves)
%
%   The Alpha Beta pruning algorithm. Based on the pseudocode as seen in AD2 course.
%   http://twicaagt.ugent.be/~gbrinkma/DA2_2020/lesnotas_AD2_2020.pdf Page 112-113.
%   
%   @arg Depth The remaining recursion depths.
%   @arg Board The board to check available moves on.
%   @arg MetaDataWhite Metadata for the white player.
%   @arg MetaDataBlack Metadata for the black player.
%   @arg MaxColor The color we are trying to maximize Score for.
%   @arg Color The currently playing color.
%   @arg Bounds The Min and Max bounds used in the ab_pruning
%   @arg CurrentBest The Current best encountered on this depth.
%   @arg BestMove The best move that has been found, together with its score.
%   @arg Moves The remaining moves for this depth.
ab_prune(0    , Board, _ , _ , MaxColor, _    , _          , _            , s(end, Score)     , _ ) :- get_score(Board, MaxColor, Score), !.
ab_prune(_    , _    , _ , _ , Color   , Color, b(Min, _  ), Best         , s(Best, Min)      , []) :- !.
ab_prune(_    , _    , _ , _ , MaxColor, Color, b(_  , Max), Best         , s(Best, Max)      , []) :- MaxColor \== Color, !.
ab_prune(Depth, Board, MW, MB, MaxColor, Color, b(Min, Max), CurrentBest  , s(Best, BestScore), [Move | Moves]) :-
    NextDepth is Depth - 1,
    move(Board, MW, MB, Move, NextBoard, NMW, NMB),
    next_color(Color, NextColor),
    get_moves(NextBoard, NMW, NMB, NextColor, NextMoves-[]),
    
    (check(NextBoard, Color, NextMoves) ->
        % CHECK
        % If we would make Move, we would place ourselves in a check position. Don't and continue with next move from Moves.
        ab_prune(Depth, Board, MW, MB, MaxColor, Color, b(Min, Max)  , CurrentBest, s(Best, BestScore), Moves)
        ;
        (NextMoves = [] -> 
            % STALEMATE
            % No further moves are possible by the enemy player, reached a stalemate.
            Score = 0
            ;
            % CALCULATE SCORE
            ab_prune(NextDepth, NextBoard, NMW, NMB, MaxColor, NextColor, b(Min, Max), none, s(TmpBest, TmpScore), NextMoves),
            (TmpBest = none ->
                % CHECKMATE
                % Every move in NextMoves leads to a CHECK for the other player. 
                (Color = MaxColor ->
                    % Enemy has checkmate.
                    Score = 1000
                    ;
                    % We are checkmate.
                    Score = -1000
                )
                ;
                % Use computed score.
                Score = TmpScore
            )
        ),
        (MaxColor = Color ->
            % Our turn, Maximizing score.
            (Score >= Max -> 
                Best = Move,
                BestScore = Score
                ;
                (Score > Min -> 
                    ab_prune(Depth, Board, MW, MB, MaxColor, Color, b(Score, Max), Move       , s(Best, BestScore), Moves)
                    ;
                    (CurrentBest = none -> 
                        ab_prune(Depth, Board, MW, MB, MaxColor, Color, b(Min, Max)  , not_this_one, s(Best, BestScore), Moves)
                        ;
                        ab_prune(Depth, Board, MW, MB, MaxColor, Color, b(Min, Max)  , CurrentBest, s(Best, BestScore), Moves)
                    )
                )
            )
            ;
            % Other turn, Minimizing score.
            (Score =< Min ->
                Best = Move,
                BestScore = Score
                ;
                (Score < Max ->
                    ab_prune(Depth, Board, MW, MB, MaxColor, Color, b(Min, Score), Move       , s(Best, BestScore), Moves)
                    ;
                    (CurrentBest = none -> 
                        ab_prune(Depth, Board, MW, MB, MaxColor, Color, b(Min, Max)  , not_this_one, s(Best, BestScore), Moves)
                        ;
                        ab_prune(Depth, Board, MW, MB, MaxColor, Color, b(Min, Max)  , CurrentBest, s(Best, BestScore), Moves)
                    )
                )
            )
        )
    ).
    

%!  move(+Board, +MetaDataWhite, +MetaDataBlack, +Move, -NewBoard, -NewMetaDataWhite, -NewMetaDataBlack)
%
%   Given a Move, change the Board and its metadata accordingly.
%
%   @arg Board The board to check available moves on.
%   @arg MetaDataWhite Metadata for the white player.
%   @arg MetaDataBlack Metadata for the black player.
%   @arg Move The Move to place on the board.
%   @arg NewBoard The Transformed Board to check available moves on.
%   @arg NewMetaDataWhite The Transformed Metadata for the white player.
%   @arg NewMetaDataBlack The Transformed Metadata for the black player.
move(Board, M1, M2, Move, NB, m(W1, W2, W3, W4), m(B1, B2, B3, B4)) :- 
    make_move(Board, Move, NB),
    rokade(NB, w,       M1, m(W1, W2, _ , _ )),
    rokade(NB, b,       M2, m(B1, B2, _ , _ )),
    next_turn(          M1, m(_ , _ , W3, _ )),
    next_turn(          M2, m(_ , _ , B3, _ )),
    en_passant(w, Move, M1, m(_ , _ , _ , W4)),
    en_passant(b, Move, M2, m(_ , _ , _ , B4)).


%!  make_move(+Board, +Move, -NewBoard)
%
%   Make a move on the board. A move is one of the the 2 compounds.
%   ~~~
%   % Normal move
%   m(Piece, From, To).
%   % Rokade
%   r(Move, Move).
%   ~~~
%
%   @arg Board The Board to make the Move on.
%   @arg Move The Move to make.
%   @arg NewBoard The Board after the Move has been made.
make_move(Board, m(Piece, OldCoord, Coord), NewBoard) :-
    set_piece_at(Board, OldCoord, empty, Tmp     ),
    set_piece_at(Tmp  , Coord   , Piece, NewBoard).
make_move(Board, r(m(Piece1, OldCoord1, Coord1), m(Piece2, OldCoord2, Coord2)) , NewBoard) :-
    set_piece_at(Board, OldCoord1, empty , Tmp1    ),
    set_piece_at(Tmp1 , OldCoord2, empty , Tmp2    ),
    set_piece_at(Tmp2 , Coord1   , Piece1, Tmp3    ),
    set_piece_at(Tmp3 , Coord2   , Piece2, NewBoard).


%!  next_turn(+MetaData, -NewMetaData)
%
%   Toggle the turn indicator in the metadata.
%
%   @arg MetaData The metadata.
%   @arg NewMetaData The new metadata.
next_turn(m(A, B, yes, D), m(A, B, no , D)).
next_turn(m(A, B, no , D), m(A, B, yes, D)).


%!  rokade(+Board, +Color, +MetaData, -NewMetaData)
%
%   Check if a rokade is still posible after making the move.
%
%   @arg Board The board to check available moves on.
%   @arg Color The color to check for.
%   @arg MetaData The metadata of that color.
%   @arg NewMetaData The new metadata.
rokade(_    , _, m(no , no , C, D), m(no , no , C, D)) :- !.
rokade(Board, w, m(yes, no , C, D), m(yes, no , C, D)) :- get_piece_at(Board, 1/1 , p(w, rook)), get_piece_at(Board, 1/5 , p(w, king)), !.
rokade(_    , w, m(yes, no , C, D), m(no , no , C, D)) :- !.
rokade(Board, w, m(no , yes, C, D), m(no , yes, C, D)) :- get_piece_at(Board, 1/8 , p(w, rook)), get_piece_at(Board, 1/5 , p(w, king)), !.
rokade(_    , w, m(no , yes, C, D), m(no , no , C, D)) :- !.
rokade(Board, b, m(yes, no , C, D), m(yes, no , C, D)) :- get_piece_at(Board, 8/1 , p(b, rook)), get_piece_at(Board, 8/5 , p(b, king)), !.
rokade(_    , b, m(yes, no , C, D), m(no , no , C, D)) :- !.
rokade(Board, b, m(no , yes, C, D), m(no , yes, C, D)) :- get_piece_at(Board, 8/8 , p(b, rook)), get_piece_at(Board, 8/5 , p(b, king)), !.
rokade(_    , b, m(no , yes, C, D), m(no , no , C, D)) :- !.

rokade(Board, Color, m(yes, yes, C, D), m(A, B, C, D)) :-
    rokade(Board, Color, m(yes, no, C, D), m(A, no, C, D)),
    rokade(Board, Color, m(no, yes, C, D), m(no, B, C, D)).


%!  en_passant(+Color, +Move, +MetaData, -NewMetaData)
%
%   Check if a en_passant is posible after making the move.
%
%   @arg Color The color to check for.
%   @arg Move The Move to make.
%   @arg MetaData The metadata of that color.
%   @arg NewMetaData The new metadata.
en_passant(w, m(p(w, pawn), 2/C, 4/C), m(A, B, C_, _), m(A, B, C_, 3/C)) :- !.
en_passant(b, m(p(b, pawn), 7/C, 5/C), m(A, B, C_, _), m(A, B, C_, 6/C)) :- !.
en_passant(_, _                      , m(A, B, C_, _), m(A, B, C_, no) ).