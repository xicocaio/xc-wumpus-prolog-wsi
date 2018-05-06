:- abolish(hunter/3).
:- abolish(grab/1).
:- abolish(actions/1).
:- abolish(shooted/1).
:- abolish(visit/1).
:- abolish(kb_not_pit/1).
:- abolish(kb_not_wumpus/1).
:- abolish(kb_pit/1).
:- abolish(kb_wumpus/1).

:- dynamic([
  hunter/3,
  grab/1,
  actions/1,
  shooted/1,
  visit/1,
  kb_not_pit/1,
  kb_not_wumpus/1,
  kb_pit/1,
  kb_wumpus/1
]).

%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%	Code for agent
%%%%%%%%%%%%%%%%%%%%%%%%
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

hunter([1,1], right).

%% add senses to KB only if position [X,Y] was not already visited
use_sensors([X,Y]) :-
	visited_list(L), member([X,Y], L);
	assertz(senses([X,Y], stench([X,Y]), breeze([X,Y]), glitter([X,Y]), no, no)).

%% add visit to KB only if position [X,Y] was not already visited
add_visit([X,Y]) :-
	visited_list(L), member([X,Y], L);
	assertz(visit([X,Y])).

run :- hunter([X,Y], D), run_scenario(0, [X,Y], D).

run_scenario(200, [_,_], _) :- write('!: Reached max allowed moves.'), nl.
run_scenario(T, [X,Y], D) :- 
	use_sensors([X,Y]),
	action(A, [X,Y], D, [X_next,Y_next], D_next),
	format('~d: At ~dx~d facing ~p, action ~p.~n', [T, X, Y, D, A]),
	add_visit([X,Y]),
	Tick is T + 1,
	run_scenario(Tick, [X_next, Y_next], D_next).

%% visited([[H]|T]) :- visited([H]), visited(T).

%% sense_breeze([]).
%% sense_breeze([[H]|T]) :- breeze([H]).

hunter_alive(true).

visited_list(L) :- 
	setof([X,Y], visit([X,Y]), L).

%% no pit in [X,Y] if it is a visited position
kb_no_pit([X,Y]) :-
	visited_list(L),
	member([X,Y], L).

%% no pit on [X,Y] if it is a visited no breeze neighbor
kb_no_pit([X,Y]) :-
	visited_list(L),
	member([VX,VY], L),
	not(breeze([VX, VY])),
	neighbor([VX,VY], [X,Y]).


no_possible_pits([]).
no_possible_pits([[X,Y]|T]) :- kb_no_pit([X,Y]), no_possible_pits(T).

%% pit on [X,Y] if there is a visited breeze neighbor which
%% all other neighbors, except [X,Y], don't have pits
kb_pit([X,Y]) :- 
	visited_list(L),
	member([VBX,VBY], L),
	breeze([VBX, VBY]),
	neighbor([VBX,VBY], [X,Y]),
	not(kb_no_pit([X,Y])),
	neighbors([VBX,VBY], N),
	subtract(N, [[X,Y]], NS),
	no_possible_pits(NS).

%% no wumpus in [X,Y] if it is a visited position
kb_no_wumpus([X,Y]) :-
	visited_list(L),
	member([X,Y], L).

%% no wumpus on [X,Y] if it is a visited no breeze neighbor
kb_no_wumpus([X,Y]) :-
	visited_list(L),
	member([VX,VY], L),
	not(breeze([VX, VY])),
	neighbor([VX,VY], [X,Y]).


no_possible_wumpus([]).
no_possible_wumpus([[X,Y]|T]) :- kb_no_wumpus([X,Y]), no_possible_wumpus(T).

%% wumpus on [X,Y] if there is a visited breeze neighbor which
%% all other neighbors, except [X,Y], don't have wumpus
kb_wumpus([X,Y]) :- 
	visited_list(L),
	member([VBX,VBY], L),
	breeze([VBX, VBY]),
	neighbor([VBX,VBY], [X,Y]),
	not(kb_no_wumpus([X,Y])),
	neighbors([VBX,VBY], N),
	subtract(N, [[X,Y]], NS),
	no_possible_wumpus(NS).
