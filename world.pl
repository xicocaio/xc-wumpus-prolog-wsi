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
