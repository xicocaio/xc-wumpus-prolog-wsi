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

%%% this should go on a utils
up(X,Y, X_Next,Y_Next) :- X_Next is X, Y_Next is Y + 1.
down(X,Y, X_Next,Y_Next) :- X_Next is X, Y_Next is Y - 1.
right(X,Y, X_Next,Y_Next) :- X_Next is X + 1, Y_Next is Y.
left(X,Y, X_Next,Y_Next) :- X_Next is X - 1, Y_Next is Y.

%%% this line below should be asserted to KB on the board generation
breeze(P, Q) :- pit(X,Y), (up(X,Y,P,Q); down(X,Y,P,Q); right(X,Y,P,Q); left(X,Y,P,Q)).



%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%	Code for agent
%%%%%%%%%%%%%%%%%%%%%%%%
%%% percept, used by agent
maybe_pit([P,Q],[X,Y]) :- breeze(X,Y), (up(X,Y,P,Q); down(X,Y,P,Q); right(X,Y,P,Q); left(X,Y,P,Q)).

% the idea here is to use the list of sensed pits with an AND, to discover has_pit
has_pit([P,Q]) :- pit(P,Q).

stench(P, Q) :- wumpus(X,Y), (up(X,Y,P,Q); down(X,Y,P,Q); right(X,Y,P,Q); left(X,Y,P,Q)).

% how to use a list of outputs as input in a predicate


% finds all locations that possibly has pit, looking from position 2,1
list1(N) :- findall([P,Q], maybe_pit(P,Q,2,1), N).

% finds all locations that possibly has pit, looking from position 1,2
list2(N) :- findall([P,Q], maybe_pit(P,Q,1,2), N).

explored_locations(L) :- [[1,1],[2,1],[1,2]].

maybe_pit_list :- 


test_pit(P,Q) :- maybe_pit(P,Q,list1(N)), maybe_pit(P,Q,list2(N)).


%% checkBreezeList([],_).
%% checkBreezeList([H|T],S) :- checkBreezeList(T,S), breeze(H,S).

dosomething([]).
dosomething([H|T]) :- has_pit(H), dosomething(T).


forward(X) :- X.

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
