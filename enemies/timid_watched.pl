:- dynamic timid_watched/5. % timid_watched(Name, X, Y, Atk, Stun)
:- use_module(ai_utils).
:- use_module(library(random)).

% Initialize Timid Watched
init_timid_watched :-
    retractall(timid_watched(_, _, _, _, _)),
    % Initial Atk 20, Position [11, 19]
    assertz(timid_watched('Timid Watched', 11, 19, 20, 0)).

% Timid Watched Logic - Random Walk within Bounds
timid_watched_tick :-
    current_predicate(game_over/0), game_over, !.
timid_watched_tick :-
    \+ timid_watched(_, _, _, _, _), !.

timid_watched_tick :-
    timid_watched(Name, X, Y, Atk, Stun),
    
    (   Stun > 0
    ->  NewStun is Stun - 1,
        retract(timid_watched(Name, X, Y, Atk, Stun)),
        assertz(timid_watched(Name, X, Y, Atk, NewStun)),
        format('~n[Boss] ~w is stunned for ~w more turns.~n', [Name, NewStun])
    ;   % Move Logic
        random_move_bounded(X, Y, NewX, NewY),
        retract(timid_watched(Name, X, Y, Atk, Stun)),
        assertz(timid_watched(Name, NewX, NewY, Atk, Stun))
        % format('~n[Boss] ~w moved to (~w, ~w).~n', [Name, NewX, NewY]) % Debug output
    ).

% Random move within the defined rectangle:
% X: [5, 17] (Left wall at 4, Limit right movement to < 18)
% Y: [17, 21] (Walls are at 16 and 22)
random_move_bounded(X, Y, NewX, NewY) :-
    findall([NX, NY], (
        valid_step(X, Y, NX, NY),
        NX > 4, NX < 18, % Explicitly limit X to stay inside the room
        NY > 16, NY < 22
    ), PossibleMoves),
    (   PossibleMoves = []
    ->  NewX = X, NewY = Y % No valid moves, stay put
    ;   random_member([NewX, NewY], PossibleMoves)
    ).

valid_step(X, Y, NewX, NewY) :-
    member([Dx, Dy], [[0,1], [0,-1], [1,0], [-1,0]]),
    NewX is X + Dx,
    NewY is Y + Dy,
    \+ wall(NewX, NewY), % Check global wall predicate
    \+ (location(player, PX, PY), PX =:= NewX, PY =:= NewY). % Don't step on player
