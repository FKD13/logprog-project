:- module('lists_extension', []).

% This nessesairy because https://github.com/SWI-Prolog/swipl-devel/commit/e36d3fd7c56615b83e5c7ae9ba249fbd850f27d1
% Is not yet available in my system.

%!  max_member(:Pred, -Max, +List) is semidet.
%
%   True when Max is the largest member according to Pred, which must be
%   a 2-argument callable that behaves like   (@=<)/2.  Fails if List is
%   empty.  The following call is equivalent to max_member/2:
%
%       ?- max_member(@=<, X, [6,1,8,4]).
%       X = 8.
%
%   @see max_list/2 for the maximum of a list of numbers.
max_member(Pred, Max, [H|T]) :-
    max_member_(T, Pred, H, Max).
max_member(_, _, []) :-
    fail.

max_member_([], _, Max0, Max) :-
    Max = Max0.
max_member_([H|T], Pred, Max0, Max) :-
    (   call(Pred, H, Max0)
    ->  max_member_(T, Pred, Max0, Max)
    ;   max_member_(T, Pred, H, Max)
    ).