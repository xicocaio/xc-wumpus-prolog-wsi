:- abolish(hunter/2).
:- abolish(grab/1).
:- abolish(actions/1).
:- abolish(shooted/1).
:- abolish(visited/1).
:- abolish(kb_not_pit/1).
:- abolish(kb_not_wumpus/1).
:- abolish(kb_pit/1).
:- abolish(kb_wumpus/1).
:- abolish(danger/1).
:- abolish(ok/1).
:- abolish(no_gold/1).

:- dynamic([
  hunter/2,
  grab/1,
  actions/1,
  shooted/1,
  visited/1,
  kb_not_pit/1,
  kb_not_wumpus/1,
  kb_pit/1,
  kb_wumpus/1,
  danger/1,
  ok/1,
  no_gold/1
]).

%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%	Code for agent
%%%%%%%%%%%%%%%%%%%%%%%%

visited([1,1]).
ok([1,1]).
hunter([1,1], right).
no_gold(true).

%% Agent movement
%% action(instruction, initial coordinates, initial direction, resulting position, resulting direction)
%% moving forward only change position, not direction
action(forward, [X_ini,Y_ini], up, [X,Y], up) :- up_cell([X_ini,Y_ini],[X,Y]).
action(forward, [X_ini,Y_ini], down, [X,Y], down) :- down_cell([X_ini,Y_ini],[X,Y]).
action(forward, [X_ini,Y_ini], right, [X,Y], right) :- right_cell([X_ini,Y_ini],[X,Y]).
action(forward, [X_ini,Y_ini], left, [X,Y], left) :- left_cell([X_ini,Y_ini],[X,Y]).

%% turning right only change direction, not position
action(turnright, [X,Y], up, [X,Y], right).
action(turnright, [X,Y], right, [X,Y], down).
action(turnright, [X,Y], down, [X,Y], left).
action(turnright, [X,Y], left, [X,Y], up).

%% turning left only change direction, not position
action(turnleft, [X,Y], up, [X,Y], left).
action(turnleft, [X,Y], left, [X,Y], down).
action(turnleft, [X,Y], down, [X,Y], right).
action(turnleft, [X,Y], right, [X,Y], up).

%% grabbing does not change position, not direction
action(grab, [X,Y], up, [X,Y], up) :- collect_gold, write('!: Grabbed gold.'), nl.
action(grab, [X,Y], left, [X,Y], left) :- collect_gold, write('!: Grabbed gold.'), nl.
action(grab, [X,Y], down, [X,Y], down) :- collect_gold, write('!: Grabbed gold.'), nl.
action(grab, [X,Y], right, [X,Y], right) :- collect_gold, write('!: Grabbed gold.'), nl.

%% shooting arrow does not change position, not direction
action(shoot, [X,Y], up, [X,Y], up).
action(shoot, [X,Y], left, [X,Y], left).
action(shoot, [X,Y], down, [X,Y], down).
action(shoot, [X,Y], right, [X,Y], right).

%% climbing out of cave does not change position, not direction
action(climb, [X,Y], up, [X,Y], up) :- X = 1, Y = 1 -> write('Climbing out! '), nl; write('Cant climb here '), nl.
action(climb, [X,Y], left, [X,Y], left) :- X = 1, Y = 1 -> write('Climbing out! '), nl; write('Cant climb here '), nl.
action(climb, [X,Y], down, [X,Y], down) :- X = 1, Y = 1 -> write('Climbing out! '), nl; write('Cant climb here '), nl.
action(climb, [X,Y], right, [X,Y], right) :- X = 1, Y = 1 -> write('Climbing out! '), nl; write('Cant climb here '), nl.

hunter_alive(true).

%% add senses to KB only if position [X,Y] was not already visited
use_sensors([X,Y]) :-
	visited_list(L), member([X,Y], L);
	(stench([X,Y]) -> Stench = true; Stench = false),
	(breeze([X,Y]) -> Breeze = true; Breeze = false),
	(glitter([X,Y]) -> Glitter = true; Glitter = false),
	Bump = false,
	Scream = false,
	asserta(senses([X,Y], Stench, Breeze, Glitter, Bump, Scream)).

%% add visited to KB only if position [X,Y] was not already visited
add_visited([X,Y]) :-
	visited_list(L), member([X,Y], L);
	asserta(visited([X,Y])).

visited_list(L) :- 
	setof([X,Y], visited([X,Y]), L).

collect_gold :-
	hunter([X,Y],_),
	glitter([X,Y]),
	no_gold(true),
	retract(no_gold(true)),
	asserta(no_gold(false)).

run :- hunter([X,Y], D), run_scenario(0, [X,Y], D).

run_scenario(200, [_,_], _) :- write('!: Reached max allowed moves.'), nl.
run_scenario(T, [X,Y], D) :- 
	use_sensors([X,Y]), %% use hunter sensors
	%% not(visited([X,Y])) -> do_inferences; write('!: Already visited here.'), nl,
	add_visited([X,Y]), 
	heuristic(A, [X,Y], D),
	action(A, [X,Y], D, [X_next,Y_next], D_next),
	format('~d: At ~dx~d facing ~p, action ~p. ~n', [T, X, Y, D, A]),
	Tick is T + 1,
	run_scenario(Tick, [X_next, Y_next], D_next).

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


%%%%% code for trying to add to KB and reduce incrieasing time complexity of infering ok and danger positions
%% add_ok([X,Y]) :-
%% 	ok_list(L), member([X,Y], L);
%% 	asserta(ok([X,Y])).

%% ok_list(L) :- 
%% 	setof([X,Y], ok([X,Y]), L).

%% new_safe([]).
%% new_safe([[X,Y]|T]) :- add_ok([X,Y]), new_safe(T).

%% add_danger([X,Y]) :-
%% 	danger_list(L), member([X,Y], L);
%% 	asserta(danger([X,Y])).

%% danger_list(L) :- 
%% 	setof([X,Y], danger([X,Y]), L).
%% %% danger([]).

%% new_dangers([]).
%% new_dangers([H|T]) :- add_danger([X,Y]), new_dangers(T).