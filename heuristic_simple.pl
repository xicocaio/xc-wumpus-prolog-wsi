%% grabs the gold if found
heuristic(A, [X,Y], _) :-
	glitter([X,Y]),
	no_gold(true),
	A = grab.

%% gets out of the cave if at position [1,1] with gold
heuristic(A, [X,Y], _) :-
	X = 1, Y = 1, 
	no_gold(false),
	A = climb.

%% go to a ok location
heuristic(A, [X,Y], D) :-
	no_gold(true),
	do_inferences(KNP, KNW, _, _),
	action(A, [X,Y], D, [X_next,Y_next], _),
	(member([X_next, Y_next], KNP); member([X_next, Y_next], KNW)),
	format('Chose action ~p. ~n', [A]).

%% get lists of nopit, nowumpus, pit and wumpus
do_inferences(KNP, KNW, KP, KW):- 
	setof([XNP,YNP], kb_no_pit([XNP,YNP]), KNP),
	setof([XNW,YNW], kb_no_wumpus([XNW,YNW]), KNW),
	kb_pit([XP,YP]) -> setof([XP,YP], kb_pit([XP,YP]), KP) ; true,
	kb_wumpus([XW,YW]) -> setof([XW,YW], kb_wumpus([XW,YW]), KW); true.