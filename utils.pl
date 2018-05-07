%% Code that is used in the whole project

in_limits([X,Y]) :- world(Width, Height), X > 0, X =< Width, Y > 0, Y =< Height.

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