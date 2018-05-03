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
gold(2, 3).
wumpus(1, 3).
pit(3, 1).
pit(3, 3).
pit(4, 4).

up(X,Y, X_Next,Y_Next) :- X_Next is X, Y_Next is Y + 1.
down(X,Y, X_Next,Y_Next) :- X_Next is X, Y_Next is Y - 1.
right(X,Y, X_Next,Y_Next) :- X_Next is X + 1, Y_Next is Y.
left(X,Y, X_Next,Y_Next) :- X_Next is X - 1, Y_Next is Y.

breeze(P, Q) :- pit(X,Y), (up(X,Y,P,Q); down(X,Y,P,Q); right(X,Y,P,Q); left(X,Y,P,Q)).

forward(X) :- X.

%%% (X,Y,T), start location is at position (1,1) at time = 1
start_location(1,1,1).
looking_right(D) :- D = 'right'.

location(X_Next,Y_Next,T_Next) :- start_location(X,Y,T), T_Next is T + 1, X_Next is X + 1, Y_Next is Y, looking_right('right'), forward(true).


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
