:- dynamic random_walker/7.
:- use_module(ai_utils).
:- use_module(library(random)).

% Initialize 3 random walkers in the specified rectangle [44,0] to [60,21]
init_random_walker :-
    retractall(random_walker(_,_,_,_,_,_,_)),
    create_walkers(3).

create_walkers(0) :- !.
create_walkers(N) :-
    % Generate random position within X:[44,60], Y:[0,21]
    random_between(44, 60, X),
    random_between(0, 21, Y),
    
    % Ensure not spawning on a wall (simple check, retry if invalid)
    (   \+ wall_check([X, Y])
    ->  atom_concat('walker', N, Name),
        random_member([Dx, Dy], [[0,1], [0,-1], [1,0], [-1,0]]),
        assertz(random_walker(Name, X, Y, Dx, Dy, 20, 0)),
        NextN is N - 1,
        create_walkers(NextN)
    ;   create_walkers(N) % Retry
    ).

% Random Walker Logic
random_walker_tick :-
    current_predicate(game_over/0), game_over, !.
random_walker_tick :-
    \+ random_walker(_, _, _, _, _, _, _), !.

random_walker_tick :-
    findall(Name, random_walker(Name, _, _, _, _, _, _), Walkers),
    process_walkers(Walkers).

process_walkers([]).
process_walkers([Name|Rest]) :-
    move_single_walker(Name),
    process_walkers(Rest).

move_single_walker(Name) :-
    random_walker(Name, X, Y, LastDx, LastDy, Atk, Stun),
    (   Stun > 0
    ->  NewStun is Stun - 1,
        retract(random_walker(Name, X, Y, LastDx, LastDy, Atk, Stun)),
        assertz(random_walker(Name, X, Y, LastDx, LastDy, Atk, NewStun)),
        format('~n[Enemy] ~w is stunned for ~w more turns.~n', [Name, NewStun])
    ;   % Move Logic with Bounds Check
        % 1. Try to maintain momentum
        MomX is X + LastDx, 
        MomY is Y + LastDy,
        (   maybe(0.5),
            is_in_graveyard(MomX, MomY), % Check new bounds
            \+ wall_check([MomX, MomY]),
            \+ (location(player, PX, PY), PX =:= MomX, PY =:= MomY) % Avoid player
        ->  NewX = MomX, NewY = MomY, NewDx = LastDx, NewDy = LastDy
        ;   % 2. Pick new valid move
            findall([NX, NY], (
                neighbor([X, Y], [NX, NY]),
                is_in_graveyard(NX, NY), % Check new bounds
                \+ wall_check([NX, NY]),
                \+ (location(player, PX, PY), PX =:= NX, PY =:= NY)
            ), AllNeighbors),
            
            (   AllNeighbors \= []
            ->  random_member([NewX, NewY], AllNeighbors),
                NewDx is NewX - X,
                NewDy is NewY - Y
            ;   NewX = X, NewY = Y, NewDx = LastDx, NewDy = LastDy
            )
        ),
        retract(random_walker(Name, _, _, _, _, _, _)),
        assertz(random_walker(Name, NewX, NewY, NewDx, NewDy, Atk, Stun))
    ).

% Helper to check bounds [44,0] to [60,21]
is_in_graveyard(X, Y) :-
    X >= 44, X =< 60,
    Y >= 0, Y =< 21.

is_pos(X, Y, [X, Y]).
