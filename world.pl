:- abolish(hunter/3).
:- abolish(wumpus/1).
:- abolish(pit/1).
:- abolish(gold/1).
:- abolish(visited/1).

:- dynamic([
  hunter/3,
  wumpus/1,
  pit/1,
  gold/1,
  grab/1,
  actions/1,
  shooted/1,
  visited/1
]).

% ---------------------------- %
% World predicates             %
% ---------------------------- %

%     +---+---+---+---+
%   4 |   |   |   | P |
%     +---+---+---+---+
%   3 | W | G | P |   |
%     +---+---+---+---+
%   2 |   |   |   |   |
%     +---+---+---+---+
%   1 | H |   | P |   |
%     +---+---+---+---+
%       1   2   3   4
% Test world
world(4,4).
gold([2, 3]).
wumpus([1, 3]).
pit([3, 1]).
pit([3, 3]).
pit([4, 4]).

%% positiion, direction, time
%% hunter([1,1], right, 1).

in_limits([X,Y]) :- world(Width, Height), X > 0, X =< Width, Y > 0, Y =< Height.

%%% this should go on a utils
up([X,Y], [X,Y_Next]) :- Y_Next is Y + 1, in_limits([X, Y_Next]).
down([X,Y], [X,Y_Next]) :- Y_Next is Y - 1, in_limits([X, Y_Next]).
right([X,Y], [X_Next,Y]) :- X_Next is X + 1, in_limits([X_Next, Y]).
left([X,Y], [X_Next,Y]) :- X_Next is X - 1, in_limits([X_Next, Y]).

neighbor([X,Y],[X_Next,Y_Next]) :- up([X,Y],[X_Next,Y_Next]); down([X,Y],[X_Next,Y_Next]); right([X,Y],[X_Next,Y_Next]); left([X,Y],[X_Next,Y_Next]).

neighbors([X,Y],N) :- findall([X_Next,Y_Next], neighbor([X,Y],[X_Next,Y_Next]), N).

%%% this line below should be asserted to KB on the board generation
breeze([X,Y]) :- pit([X_Next,Y_Next]), neighbor([X,Y],[X_Next,Y_Next]).
stench([X,Y]) :- wumpus([X_Next,Y_Next]), neighbor([X,Y],[X_Next,Y_Next]).


%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%	Code for agent
%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%	continue here and try to get list of neighbors on the terminal
%%%%%%%%%%%%%%%%%%%%%%%%
%%% percept, used by agent
forward(X) :- X.

visited([1,1]).
hunter([1,1], right, 1).
hunter([X,Y], D, T) :-
	X_Bef is X - 1,
	Y_Bef is Y,
	T_Bef is T - 1,
	in_limits([X_Bef, Y_Bef]),
	hunter([X_Bef, Y_Bef], D_Bef, T_Bef),
	forward(true).

dosomething([]).
dosomething([H|T]) :- has_pit(H), dosomething(T).

sense_breeze([X,Y]) :- breeze([X,Y]).
sense_stench([X,Y]) :- stench([X,Y]).


nearby_pit_possibilities([X,Y],N) :- findall([X_Next,Y_Next], (neighbor([X,Y],[X_Next,Y_Next]), breeze([X,Y])), N).

%% maybe_pit([]).
%% maybe_pit([H|T]) :- breeze(H), maybe_pit(T).



% how to use a list of outputs as input in a predicate


% finds all locations that possibly has pit, looking from position 2,1
list1(N) :- findall([P,Q], maybe_pit(P,Q,2,1), N).

% finds all locations that possibly has pit, looking from position 1,2
list2(N) :- findall([P,Q], maybe_pit(P,Q,1,2), N).

%% explored_locations(L) :- [[1,1],[2,1],[1,2]].

%% maybe_pit_list :- 


test_pit(P,Q) :- maybe_pit(P,Q,list1(N)), maybe_pit(P,Q,list2(N)).


%% checkBreezeList([],_).
%% checkBreezeList([H|T],S) :- checkBreezeList(T,S), breeze(H,S).


%%% (X,Y,T), start location is at position (1,1) at time = 1
start_location(1,1,1).
looking_right(D) :- D = right.

location(X_Next,Y_Next,T_Next) :- start_location(X,Y,T), T_Next is T + 1, X_Next is X + 1, Y_Next is Y, looking_right(right), forward(true).


%%% findall(Y, pit(3,Y), List).

%%% axioms page 284

%%% page 359 teaches about lists


% Test world
%     +---+---+---+---+
%   4 |   |   |   | G |
%     +---+---+---+---+
%   3 |   |   |   |   |
%     +---+---+---+---+
%   2 |   |   |   |   |
%     +---+---+---+---+
%   1 | H |   |   |   |
%     +---+---+---+---+
%       1   2   3   4
% gold(4, 4).
