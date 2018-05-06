%% grabs the gold if found
heuristic(A, [X,Y], _) :-
	glitter([X,Y]),
	no_gold(true),
	A = grab.

%% seach for neighbor that could be safe and unexplored
heuristic(A, [X,Y], D) :-
	no_gold(true),
	ok_list(O),
	visited_list(V),
	subtract(O, [V], ONV), %% get list ONV of OK not visited locations
	action(A, [X,Y], D, [X_next,Y_next], _),
	member([X_next,Y_next], ONV). %% get list of action that dont lead to a dangerous place

%% seach for nearest safe unexplored location
heuristic(A, [X,Y], D) :-
	no_gold(true),
	ok_list(O),
	visited_list(V),
	subtract(O, [V], ONV), %% get list ONV of OK not visited locations
	action(A, [X,Y], D, [X_next,Y_next], _),
	member([X_next,Y_next], ONV). %% get list of action that dont lead to a dangerous place

%% if grabbed gold, go to exit
heuristic(A, [X,Y], D) :-
	no_gold(false),
	visited_list(V),
	action(A, [X,Y], D, [X_next,Y_next], _),
	member([X_next,Y_next], V).

%% gets out of the cave if at position [1,1] with gold
heuristic(A, [X,Y], _) :-
	X = 1, Y = 1, 
	no_gold(false),
	A = climb.

%% get lists of nopit, nowumpus, pit and wumpus
do_inferences(KNP, KNW, KP, KW):- 
	setof([XNP,YNP], kb_no_pit([XNP,YNP]), KNP),
	setof([XNW,YNW], kb_no_wumpus([XNW,YNW]), KNW),
	kb_pit([XP,YP]) -> setof([XP,YP], kb_pit([XP,YP]), KP) ; true,
	kb_wumpus([XW,YW]) -> setof([XW,YW], kb_wumpus([XW,YW]), KW); true.