%%% A (simple) diff

%%% Copyright (C) 2011  Tomas Abrahamsson
%%%
%%% Author: Tomas Abrahamsson <tab@lysator.liu.se>
%%%
%%% This library is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU Library General Public
%%% License as published by the Free Software Foundation; either
%%% version 2 of the License, or (at your option) any later version.
%%%
%%% This library is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% Library General Public License for more details.
%%%
%%% You should have received a copy of the GNU Library General Public
%%% License along with this library; if not, write to the Free
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
%%%

-module(tdiff).
-export([diff/2,
         diff/3,
         diff/4,
         
         both_diff/2,
         both_diff/3,
         both_diff/4
        ]).


%%---------------------------------------------------------------------
%% diff(Sx, Sy)      -> Diff
%% diff(Sx, Sy, Cmp) -> Diff
%% diff(Sx, Sy, Cmp, Cmb) -> Diff
%%   Sx = Sy = [Elem]      %% typically a list of lines, characters or words
%%     Elem = term()
%%   Cmp = element comparsion function, erlang:'=='/2 if not specifed
%%   Cmb = element combine function, erlang:min/2 if not specifed
%%   Diff = [D]
%%     D = {eq, [Elem]} |  %% [Elem] is equal (in mean of Cmp function) in Sx and Sy; combined using Cmb function
%%         {ins,[Elem]} |  %% [Elem] must be inserted into Sx to create Sy
%%         {del,[Elem]}    %% [Elem] must be removed from Sx to create Sy
%%---------------------------------------------------------------------

%% Algorithm: "An O(ND) Difference Algorithm and Its Variations"
%% by E. Myers, 1986.
%%
%% Some good info can also be found at http://neil.fraser.name/writing/diff/
%%
%% General principle of the algorithm:
%%
%% We are about to produce a diff (or editscript) on what differs (or
%% how to get from) string Sx to Sy.  We lay out a grid with the
%% symbols from Sx on the x-axis and the symbols from Sy on the Y
%% axis. The first symbol of Sx and Sy is at (0,0).
%%
%% (The Sx and Sy are strings of symbols: lists of lines or lists of
%% characters, or lists of works, or whatever is suitable.)
%%
%% Example: Sx="aXcccXe", Sy="aYcccYe" ==> the following grid is formed:
%%
%%             Sx
%%             aXcccXe
%%         Sy a\
%%            Y
%%            c  \\\
%%            c  \\\
%%            c  \\\
%%            Y
%%            e      \
%%
%% Our plan now is go from corner to corner: from (0,0) to (7,7).
%% We can move diagonally whenever the character on the x-axis and the
%% character on the y-axis are identical. Those are symbolized by the
%% \-edges in the grid above.
%%
%% When it is not possible to go diagonally (because the characters on
%% the x- and y-axis are not identical), we have to go horizontally
%% and vertically. This corresponds to deleting characters from Sx and
%% inserting characters from Sy.
%%
%% Definitions (from the "O(ND) ..." paper by E.Myers):
%%
%% * A D-path is a path with D non-diagonal edges (ie: edges that are
%%   vertical and/or horizontal).
%% * K-diagonal: the diagonal such that K=X-Y
%%   (Thus, the 0-diagonal is the one starting at (0,0), going
%%   straight down-right. The 1-diagonal is the one just to the right of
%%   the 0-diagonal: starting at (1,0) going straight down-right.
%%   There are negative diagonals as well: the -1-diagonal is the one starting
%%   at (0,1), and so on.
%% * Snake: a sequence of only-diagonal steps
%%
%% The algorithm loops over D and over the K-diagonals:
%% D = 0..(length(Sx)+length(Sy))
%%   K = -D..D in steps of 2
%%     For every such K-diagonal, we choose between the (D-1)-paths
%%     whose end-points are currently on the adjacent (K-1)- and
%%     (K+1)-diagonals: we pick the one that have gone furthest along
%%     its diagonal.
%%
%%     This means taking that (D-1)-path and going right (if
%%     we pick the (D-1)-path on the (K-1)-diagonal) or down (if we
%%     pick the (D-1)-path on the (K+1)-diagonal), thus forming a
%%     D-path from a (D-1)-path.
%%
%%     After this, we try to extend the snake as far as possible along
%%     the K-diagonal.
%%
%%     Note that this means that when we choose between the
%%     (D-1)-paths along the (K-1)- and (K+1)-diagonals, we choose
%%     between two paths, whose snakes have been extended as far as
%%     possible, ie: they are at a point where the characters Sx and
%%     Sy don't match.
%%
%% Note that with this algorithm, we always do comparions further
%% right into the strings Sx and Sy. The algorithm never goes towards
%% the beginning of either Sx or Sy do do further comparisons. This is
%% good, because this fits the way lists are built in functional
%% programming languages.

both_diff(Sx, Sy) ->
    both_diff(Sx, Sy, fun erlang:'=='/2).

both_diff(Sx, Sy, Cmp) ->
    both_diff(Sx, Sy, Cmp, fun erlang:min/2).

both_diff(Sx, Sy, Cmp, Cmb) ->
    Forward  = diff(Sx                , Sy                , Cmp, Cmb),
    Backward = diff(lists:reverse(Sx) , lists:reverse(Sy) , Cmp, Cmb),
    case length(Forward) < length(Backward) of
        true ->
            Forward;
        false ->
            [ {Op, lists:reverse(L)} || {Op, L} <- lists:reverse(Backward) ]
    end.

diff(Sx, Sy) ->
    diff(Sx, Sy, fun erlang:'=='/2).

diff(Sx, Sy, Cmp) ->
    diff(Sx, Sy, Cmp, fun erlang:min/2).

diff(Sx, Sy, Cmp, Cmb) ->
    SxLen = length(Sx),
    SyLen = length(Sy),
    DMax = SxLen + SyLen,
    EditScript = case try_dpaths(0, DMax, [{0, 0, Sx, Sy, []}], Cmp, Cmb) of
                     no            -> [{del,Sx},{ins,Sy}];
                     {ed,EditOpsR} -> edit_ops_to_edit_script(EditOpsR)
                 end,
    EditScript.


try_dpaths(D, DMax, D1Paths, Cmp, Cmb) when D =< DMax ->
    case try_kdiagonals(-D, D, D1Paths, [], Cmp, Cmb) of
        {ed, E}          -> {ed, E};
        {dpaths, DPaths} -> try_dpaths(D+1, DMax, DPaths, Cmp, Cmb)
    end;
try_dpaths(_, _DMax, _DPaths, _Cmp, _Cmb) ->
    no.

try_kdiagonals(K, D, D1Paths, DPaths, Cmp, Cmb) when K =< D ->
    DPath = if D == 0 -> hd(D1Paths);
               true   -> pick_best_dpath(K, D, D1Paths)
            end,
    case follow_snake0(DPath, Cmp, Cmb) of
        {ed, E} ->
            {ed, E};
        {dpath, DPath2} when K =/= -D ->
            try_kdiagonals(K+2, D, tl(D1Paths), [DPath2 | DPaths], Cmp, Cmb);
        {dpath, DPath2} when K =:= -D ->
            try_kdiagonals(K+2, D, D1Paths, [DPath2 | DPaths], Cmp, Cmb)
    end;
try_kdiagonals(_, _D, _, DPaths, _Cmp, _Cmb) ->
    {dpaths, lists:reverse(DPaths)}.

follow_snake0({X, Y,
              [Val1|Tx] = ATx,
              [Val2|Ty] = ATy, Cs}, Cmp, Cmb) ->
    case Cmp(Val1, Val2) of
        true ->
            follow_snake0({X+1,Y+1, Tx,Ty,
                           [{e,Cmb(Val1, Val2)} | Cs]}, Cmp, Cmb);
        false ->
            follow_snake_end({X, Y, ATx, ATy, Cs})
    end;
follow_snake0(All, _Cmp, _Cmb) -> follow_snake_end(All).

follow_snake_end({_X,_Y,[],     [],     Cs}) -> {ed, Cs};
follow_snake_end({X, Y, [],     Sy,     Cs}) -> {dpath, {X, Y, [],  Sy,  Cs}};
follow_snake_end({X, Y, oob,    Sy,     Cs}) -> {dpath, {X, Y, oob, Sy,  Cs}};
follow_snake_end({X, Y, Sx,     [],     Cs}) -> {dpath, {X, Y, Sx,  [],  Cs}};
follow_snake_end({X, Y, Sx,     oob,    Cs}) -> {dpath, {X, Y, Sx,  oob, Cs}};
follow_snake_end({X, Y, Sx,     Sy,     Cs}) -> {dpath, {X, Y, Sx,  Sy,  Cs}}.

pick_best_dpath(K, D, DPs) -> pbd(K, D, DPs).

pbd( K, D, [DP|_]) when K==-D -> go_inc_y(DP);
pbd( K, D, [DP])   when K==D  -> go_inc_x(DP);
pbd(_K,_D, [DP1,DP2|_])       -> pbd2(DP1,DP2).

pbd2({_,Y1,_,_,_}=DP1, {_,Y2,_,_,_}) when Y1 > Y2 -> go_inc_x(DP1);
pbd2(_DP1  ,           DP2)                       -> go_inc_y(DP2).

go_inc_y({X, Y, [H|Tx], Sy, Cs}) -> {X, Y+1, Tx,  Sy,  [{y,H}|Cs]};
go_inc_y({X, Y, [],     Sy, Cs}) -> {X, Y+1, oob, Sy,  Cs};
go_inc_y({X, Y, oob,    Sy, Cs}) -> {X, Y+1, oob, Sy,  Cs}.

go_inc_x({X, Y, Sx, [H|Ty], Cs}) -> {X+1, Y, Sx,  Ty,  [{x,H}|Cs]};
go_inc_x({X, Y, Sx, [],     Cs}) -> {X+1, Y, Sx,  oob, Cs};
go_inc_x({X, Y, Sx, oob,    Cs}) -> {X+1, Y, Sx,  oob, Cs}.


edit_ops_to_edit_script(EditOps) -> e2e(EditOps, _Acc=[]).

e2e([{x,C}|T], [{ins,R}|Acc]) -> e2e(T, [{ins,[C|R]}|Acc]);
e2e([{y,C}|T], [{del,R}|Acc]) -> e2e(T, [{del,[C|R]}|Acc]);
e2e([{e,X}|T], [{eq,R}|Acc])  -> e2e(T, [{eq, [X|R]}|Acc]);
e2e([{x,C}|T], Acc)           -> e2e(T, [{ins,[C]}|Acc]);
e2e([{y,C}|T], Acc)           -> e2e(T, [{del,[C]}|Acc]);
e2e([{e,X}|T], Acc)           -> e2e(T, [{eq, [X]}|Acc]);
e2e([],        Acc)           -> Acc.
