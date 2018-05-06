:- abolish(hunter/3).
:- abolish(wumpus/1).
:- abolish(pit/1).
:- abolish(gini/1).
:- abolish(visited/1).
:- abolish(breeze/1).

:- dynamic([
  hunter/3,
  wumpus/1,
  pit/1,
  gini/1,
  grab/1,
  actions/1,
  shooted/1,
  visited/1,
  breeze/1
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

%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%	World Setup
%%%%%%%%%%%%%%%%%%%%%%%%

world(4,4).
gini([2, 3]).
wumpus([1, 3]).
pit([3, 1]). %% breeze [2,1], [4,1], [3,2]
pit([3, 3]). %% breeze [3,4], [3,2], [2,3], [4,3]
pit([4, 4]). %% breeze [4,3], [3,4]


find_breezes(L) :- 
	setof(
		[X_next,Y_next],
		X^Y^(pit([X,Y]),
		neighbor([X,Y],[X_next,Y_next])),
		L).

setup_breeze([]).
setup_breeze([[X,Y]|T]) :- assertz(breeze([X,Y])), setup_breeze(T).

setup_breezes :-
	find_breezes(N),
	setup_breeze(N).

find_stenches(L) :- 
	setof(
		[X_next,Y_next],
		X^Y^(pit([X,Y]),
		neighbor([X,Y],[X_next,Y_next])),
		L).

setup_stench([]).
setup_stench([[X,Y]|T]) :- assertz(stench([X,Y])), setup_stench(T).

setup_stenches :-
	find_stenches(N),
	setup_stench(N).


in_limits([X,Y]) :- world(Width, Height), X > 0, X =< Width, Y > 0, Y =< Height.

%%% this should go on a utils
up_cell([X,Y], [X,Y_next]) :- Y_next is Y + 1, in_limits([X, Y_next]).
down_cell([X,Y], [X,Y_next]) :- Y_next is Y - 1, in_limits([X, Y_next]).
right_cell([X,Y], [X_next,Y]) :- X_next is X + 1, in_limits([X_next, Y]).
left_cell([X,Y], [X_next,Y]) :- X_next is X - 1, in_limits([X_next, Y]).

neighbor([X,Y],[X_next,Y_next]) :-
	up_cell([X,Y],[X_next,Y_next]);
	down_cell([X,Y],[X_next,Y_next]);
	right_cell([X,Y],[X_next,Y_next]);
	left_cell([X,Y],[X_next,Y_next]).

neighbors([X,Y],N) :- findall([X_next,Y_next], neighbor([X,Y],[X_next,Y_next]), N).


%% nw(x,y) :- (visi(d),nfed(d)) ; (vis(e),nfed(e))
 %% w(d) ;- breza(c),nw(d),nw(e),nw(u)


%% stench([X,Y]) :- wumpus([X_next,Y_next]), neighbor([X,Y],[X_next,Y_next]).

%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%	Code for agent
%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%	continue here and try to get list of neighbors on the terminal
%%%%%%%%%%%%%%%%%%%%%%%%
%%% percept, used by agent
action(A) :- A = forward ; A = turnright; A = turnleft.

%% Agent movement
%% action(instruction, initial coordinates, initial direction, resulting position, resulting direction)
action(forward, [X_ini,Y_ini], up, [X,Y], up) :- up_cell([X_ini,Y_ini],[X,Y]).
action(forward, [X_ini,Y_ini], down, [X,Y], down) :- down_cell([X_ini,Y_ini],[X,Y]).
action(forward, [X_ini,Y_ini], right, [X,Y], right) :- right_cell([X_ini,Y_ini],[X,Y]).
action(forward, [X_ini,Y_ini], left, [X,Y], left) :- left_cell([X_ini,Y_ini],[X,Y]).

action(turnright, [X,Y], up, [X,Y], right).
action(turnright, [X,Y], right, [X,Y], down).
action(turnright, [X,Y], down, [X,Y], left).
action(turnright, [X,Y], left, [X,Y], up).

action(turnleft, [X,Y], up, [X,Y], left).
action(turnleft, [X,Y], left, [X,Y], down).
action(turnleft, [X,Y], down, [X,Y], right).
action(turnleft, [X,Y], right, [X,Y], up).

%% hunter([1,1], right, 0, _).

%% hunter([X,Y], D, T, A) :-
%% 	T >= 0, T_ini is T - 1,
%% 	hunter([X_ini,Y_ini], D_ini, T_ini, _),
%% 	action(A, [X_ini,Y_ini], D_ini, [X,Y], D).

%% hunter([X,Y], D) :-
%% 	hunter([X_ini,Y_ini], D_ini, _),
%% 	action(A, [X_ini,Y_ini], D_ini, [X,Y], D).

hunter([1,1], right).

run :- hunter([X,Y], D), run_scenario(0, [X,Y], D).

run_scenario(200, [_,_], _) :- write('!: Reached max allowed moves.'), nl, halt.
run_scenario(T, [X,Y], D) :- 
	action(A, [X,Y], D, [X_next,Y_next], D_next),
	format('~d: At ~dx~d facing ~p, action ~p.~n', [T, X, Y, D, A]),
	%% append([X,Y], visited, visited),
	Tick is T + 1,
	run_scenario(Tick, [X_next, Y_next], D_next).

%% visited([[H]|T]) :- visited([H]), visited(T).

%% sense_breeze([]).
%% sense_breeze([[H]|T]) :- breeze([H]).

hunter_alive(true).

visited([[1,1], [1,2]]).

%% there is no pit in [X,Y] if the position [X,Y] was visited
kb_no_pit([X,Y]) :-
	visited(L),
	member([X,Y], L).

%% there is no pit in neighbors of [X,Y] if had no breeze when visited
kb_no_pit([X,Y]) :-
	neighbor([X,Y],[X_next,Y_next]),
	visited(L),
	member([X_next,Y_next], L),
	not(breeze([X_next, Y_next])).

%% maybe_pits([X,Y], P) :- 
%% 	findall(
%% 		[X_next,Y_next],
%% 		(breeze([X,Y]),
%% 		neighbor([X,Y],[X_next,Y_next]),
%% 		not(visited([[X_next,Y_next]]))),
%% 		P).

%% iterate_list([]).
%% iterate_list([H|T]) :- breeze(H), iterate_list(T).

%% sense_stench([X,Y]) :-
%% 	neighbors([X,Y],N),
%% 	N .

%% sensors([X,Y], [Stench, Breeze, Glitter, Bump, Scream]) :-
%% 	Stench is sense_stench([X,Y]),
%% 	Breeze is sense_breeze([X,Y]),
%% 	Glitter is sense_glitter([X,Y]),
%% 	Bump is no,
%% 	Scream is no.

sense_breeze([X,Y]) :- breeze([X,Y]).
sense_stench([X,Y]) :- stench([X,Y]).


nearby_pit_possibilities([X,Y],N) :- findall([X_next,Y_next], (neighbor([X,Y],[X_next,Y_next]), breeze([X,Y])), N).

%% maybe_pit([]).
%% maybe_pit([H|T]) :- breeze(H), maybe_pit(T).


%% checkBreezeList([],_).
%% checkBreezeList([H|T],S) :- checkBreezeList(T,S), breeze(H,S).


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
% gini(4, 4).
