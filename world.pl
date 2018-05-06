:- abolish(wumpus/1).
:- abolish(pit/1).
:- abolish(gold/1).
:- abolish(breeze/1).
:- abolish(glitter/1).

:- dynamic([
  wumpus/1,
  pit/1,
  gold/1,
  breeze/1,
  glitter/1
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

%%%%%%%%%
%%%%%%%%%	World Setup
%%%%%%%%%

world(4,4).
gold([2, 3]).
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

setup_glitter :-
	gold([X,Y]),
	assertz(glitter([X,Y])).

setup_world :-
	setup_stenches,
	setup_breezes,
	setup_glitter.